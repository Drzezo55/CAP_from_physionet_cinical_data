library(here)
library(renv)
library(tidyverse)
library(ggplot2)
library(tibble)
library(naniar)
library(mice)
library(foreign)
library(dplyr)
library(purrr)
library(plotly)
renv::init()
# load data
df <- read.csv(file = "data/NACef_2025.csv", stringsAsFactors = FALSE)
# extarct the time series data 
# Ensure it's a Date object first
df$admission_date <- as.Date(df$admission_date, format = "%d/%m/%Y")

# Extract month as number (1–12)
df$admission_month_num <- format(df$admission_date, "%m")

# Extract admission months 
df <- df %>%
  mutate(
    admission_month_name = format(admission_date, "%B"),              # Get month name
    admission_month_name = factor(admission_month_name,               # Convert to ordered factor
                                  levels = month.name,
                                  ordered = TRUE)
  )

p <- ggplot(data = df , aes(x = admission_month_name)) +
  geom_bar(fill = "red")+
  labs(x = "Admission Month", y = "Count") +
  theme_light()
ggsave("results/admission_time.png", plot = p, width = 8, height = 6, dpi = 300)

# explore the missing and its mass
missing <- sapply(df, function(x) sum(is.na(x)))
missing_tbl <- tibble(
  column = names(missing),
  na_count = missing
) |> arrange(desc(na_count))

# vis the missing 
p <- gg_miss_var(df) + theme_light(base_size = 8)
ggsave("results/missing_values_plot.png", plot = p, width = 8, height = 6, dpi = 300)

# explore each variable type 
y <-  colnames(df)
for (colname in y) {
  cat(colname, ":", class(df[[colname]]), "\n")
}

char_cols <- names(df)[sapply(df, is.character)]
char_cols
df[char_cols] <- lapply(df[char_cols], function(x) ifelse(x == "", "no", x))
char_cols <- names(df)[sapply(df, is.character)]

for (x in y) {
  if (any(df[[x]] == "", na.rm = TRUE)) {
    print(x)
  }
}
# filter for adult age between 18, 90
sum(df$age >= 18)
new_df <-  df %>%
  filter(age != ">90" & age >= 18)
nrow(df) == nrow(new_df)
# filter columns that have missing more than half of the dataset
new_df <- new_df[ , colSums(is.na(new_df)) <= 350]
class(new_df$age)

df_v1 <- new_df %>%
  mutate(across(everything(), \(x) type.convert(x, as.is = TRUE)))

df_v2 <- df_v1[, c(char_cols, "record_id")]



summary(as.factor(df_v2$minor_criteria))

df_v2 %>%
  separate_rows(minor_criteria, sep = ",") %>%       # split comma-separated values into rows
  count(minor_criteria, sort = TRUE)                 # count each criterion
summary(as.factor(df_v2$mayor_criteria))
df_v2 %>%
  separate_rows(mayor_criteria, sep = ",") %>%       # split comma-separated values into rows
  count(mayor_criteria, sort = TRUE)  
summary(as.factor(df_v2$comorbid))
df_v2 %>%
  separate_rows(comorbid, sep = ",") %>%       # split comma-separated values into rows
  count(comorbid, sort = TRUE)  
colnames(df_v2)
df_v2 %>%
  separate_rows(ab_prev, sep = ",") %>%       # split comma-separated values into rows
  count(ab_prev, sort = TRUE, by= record_id)  
df_cleaned <- df_v2 %>%
  mutate(across(everything(), as.character))
# cleaning the character columns
for (i in colnames(df_v2)) {
  df_cleaned <- df_cleaned %>%
    separate_rows(!!sym(i), sep = ",") %>%   # !!sym() converts string to column reference
    mutate(!!sym(i) := trimws(!!sym(i)))     # trim spaces after splitting
}
#
# all the categorical columns you want to expand
cols_to_expand <- c(
  "ab_prev",
  "comorbid",
  "int_24h",
  "mdr",
  "ab_24h",
  "minor_criteria",
  "mayor_criteria",
  "treatm_fail_cause",
  "secund_infec",
  "cv"
)

# start from df_cleaned
df_wide <- df_cleaned %>%
  select(record_id, all_of(cols_to_expand)) %>%
  pivot_longer(cols = -record_id, names_to = "variable", values_to = "value") %>%
  separate_rows(value, sep = ",") %>%
  mutate(
    value = as.character(value),
    value = ifelse(is.na(value), "NA", value),
    value = paste0(variable, "_", value)
  ) %>%
  distinct(record_id, value) %>%   # remove duplicates
  mutate(flag = 1) %>%
  pivot_wider(names_from = value, values_from = flag, values_fill = 0)
sum(df_wide$record_id %in% new_df$record_id)
class(df_wide$record_id) <- class(df_v1$record_id)
df_v3 <- df_v1 %>%
  left_join(df_wide, by = "record_id")
colnames(df_v3)
# remove the original ambiguous data formats
df_v4 <- df_v3 %>%
  select(-all_of(cols_to_expand))
colnames(df_v3) %in% colnames(df_v4)
y <-  colnames(df_v4)
for (colname in y) {
  cat(colname, ":", class(df_v4[[colname]]), "\n")
}

char_cols <- names(df_v4)[sapply(df_v4, is.character)]
char_cols
df_v5 <- df_v4

df_v5$admission_date <- as.Date(df_v5$admission_date, format = "%Y-%m-%d")
df_v5$admission_month_name <- as.factor(df_v5$admission_month_name)
# then make sure again for char_cols, shall return zero
# check for missing amount 

gg_miss_var(df_v4) + theme_light(base_size = 6)
sum(is.na(df_v5))
missing <- sapply(df_v5, function(x) sum(is.na(x)))
missing_tbl <- tibble(
  column = names(missing),
  na_count = missing
) |> arrange(desc(na_count))
view(missing_tbl)


# imputation 
# Remove original Date
df_mice <- df_v5 %>% select(-admission_date)

# Run mice
imp <- mice(df_mice, m = 5, maxit = 10, printFlag = FALSE)
imp$method
imp$where
imp$chainMean
plot(imp)


# Get the completed dataset from mice
df_completed <- complete(imp)
# Loop through all columns to convert 0,1 variables to factor variables
for (col in colnames(df_completed)) {
  unique_vals <- unique(na.omit(df_completed[[col]]))  # ignore NAs
  if (all(unique_vals %in% c(0, 1))) {
    df_completed[[col]] <- factor(df_completed[[col]], levels = c(0, 1))
    cat("Converted", col, "to factor\n")
  }
}

# Loop through all columns to plot a complete dataset
for (x in colnames(df_completed)) {
  col_data <- df_completed[[x]]
  
  # Choose plot type based on variable class
  if (is.numeric(col_data) || is.integer(col_data)) {
    p <- ggplot(df_completed, aes(x = .data[[x]])) +
      geom_density(fill = "lightblue") +
      ggtitle(x)
  } else {  # categorical / character / factor
    p <- ggplot(df_completed, aes(x = .data[[x]])) +
      geom_bar(fill = "orange") +
      ggtitle(x)
  }
  
  # Convert ggplot to interactive plotly and show
  print(ggplotly(p))
  
  Sys.sleep(1)  # pause 1 second between plots
}

cleaned_data <- df_completed
sum(is.na(cleaned_data))
for (i in colnames(cleaned_data)){
  if (NA %in% cleaned_data[[i]]){
    print(i)
  }
}

# this returned the date related data 

renv::snapshot()


















