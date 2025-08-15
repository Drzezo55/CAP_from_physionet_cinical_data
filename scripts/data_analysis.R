# building a statistical model for the SOFA score after 72 hrs
library(skimr)
skim(cleaned_data)
library(gtsummary)
library(ggstatsplot)
library(ggpubr)
library(nnet)
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
renv::init(bare = TRUE)
tbl_summary(
  data = cleaned_data %>% select(-record_id, -admission_date)
)

cleaned_data$admission_month_name <- NULL
data <-  cleaned_data[complete.cases(cleaned_data),]
View(data)
sum(is.na(data))
#
class(data$sofa_72)

ggplot(data, aes(x = sofa_72)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SOFA Scores at 72h", x = "SOFA Score", y = "Count")

ggplot(data, aes(y = sofa_72)) +
  geom_boxplot(fill = "orange") +
  labs(title = "SOFA Score at 72h", y = "Score")

ggplot(data, aes(x = "", y = sofa_72)) +  # x="" makes a single violin
  geom_violin(fill = "skyblue", color = "black", width = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +  # optional: overlay boxplot
  labs(title = "Violin Plot of SOFA Score at 72h",
       x = "",
       y = "SOFA Score") +
  theme_minimal()
# filter outliers 
Q <- quantile(data$sofa_72, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$sofa_72)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿
eliminated<- subset(data, data$sofa_72 > (Q[1] - 1.5*iqr) & data$sofa_72 < (Q[2]+1.5*iqr))
library(ggstatsplot)
ggbetweenstats(
  data = eliminated,
  x = gender,        # grouping variable (categorical)
  y = sofa_72,       # numeric variable
  outlier.tagging = TRUE
)
ggplot(eliminated, aes(x = "", y = sofa_72)) +  # x="" makes a single violin
  geom_violin(fill = "skyblue", color = "black", width = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +  # optional: overlay boxplot
  labs(title = "Violin Plot of SOFA Score at 72h",
       x = "",
       y = "SOFA Score") +
  theme_minimal()

# correlation
# Pearson correlation (default)
cor(eliminated$sofa_72, eliminated$admission_sofa, use = "complete.obs", method = "pearson")

# Spearman correlation (rank-based, non-parametric)
cor(eliminated$sofa_72, eliminated$age, use = "complete.obs", method = "spearman")
ggplot(eliminated, aes(x = age, y = sofa_72)) +
  geom_point(color = "blue", alpha = 0.6) +  # scatter points
  geom_smooth(method = "lm", color = "red") +  # regression line
  labs(title = "Correlation between SOFA admission and SOFA Score at 72h",
       x = "Age",
       y = "SOFA Score") +
  theme_minimal()

ggscatter(eliminated, x = "admission_sofa", y = "sofa_72",
          add = "reg.line",            # regression line
          add.params = list(color = "red", fill = "lightgray"),
          conf.int = TRUE) +            # confidence interval
  stat_cor(method = "pearson")        # shows correlation coefficient on plot

# 
# Identify columns with more than 1 unique value
cols_to_use <- sapply(eliminated, function(x) length(unique(x)) > 1)

# Keep only those columns
eliminated_filtered <- eliminated[, cols_to_use]
eliminated_filtered$admission_date <- NULL
# Make sure categorical variables are factors
eliminated_filtered[] <- lapply(eliminated_filtered, function(x) {
  if(is.character(x)) as.factor(x) else x
})

# Fit multiple linear regression
# sofa_72 is dependent variable, all others are predictors
# Wrap all predictor names in backticks
predictors <- setdiff(names(eliminated_filtered), "sofa_72")
predictors_safe <- paste0("`", predictors, "`")
# Create formula
formula <- as.formula(paste("sofa_72 ~", paste(predictors_safe, collapse = " + ")))
# Fit the model
model <- lm(formula, data = eliminated_filtered)
# View summary
summary(model)
# daignostic visulazation
ggplot(model, aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()
# Add predicted values to your dataframe
eliminated_filtered$sofa_pred <- predict(model)

# Scatter plot of actual vs predicted
ggplot(eliminated_filtered, aes(x = sofa_pred, y = sofa_72)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual SOFA_72",
       x = "Predicted SOFA_72",
       y = "Actual SOFA_72") +
  theme_minimal()
#


ggplot(eliminated_filtered, aes(x = factor(admission_month_num), y = admission_sofa, fill = factor(admission_month_num))) +
  geom_violin(color = "black", width = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +
  labs(title = "Violin Plot of SOFA Score at 72h by Admission Month",
       x = "Admission Month",
       y = "SOFA Score",
       fill = "Month") +
  theme_minimal()



ggplot(eliminated, aes(x = gender, y = admission_sofa, fill = gender)) +
  geom_violin() + geom_boxplot(width = 0.1)


eliminated_filtered$admission_sofa
# clustering 
# Example: k-means clustering
set.seed(123)
cluster_data <- eliminated_filtered[, sapply(eliminated_filtered, is.numeric)]
km <- kmeans(cluster_data, centers = 3)
eliminated_filtered$cluster <- as.factor(km$cluster)
table(eliminated_filtered$cluster)

ggplot(eliminated_filtered, aes(x = cluster, y = sofa_72, fill = cluster)) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "SOFA_72 by Cluster", x = "Cluster", y = "SOFA_72") +
  theme_minimal()


# PCA
# Select numeric columns only
cluster_data <- eliminated_filtered[, sapply(eliminated_filtered, is.numeric)]

# Optional: scale data (recommended for PCA)
cluster_data_scaled <- scale(cluster_data)
pca <- prcomp(cluster_data_scaled, center = TRUE, scale. = TRUE)
summary(pca)
biplot(pca, scale = 0)
explained_var <- pca$sdev^2 / sum(pca$sdev^2)
barplot(explained_var, names.arg = paste0("PC", 1:length(explained_var)),
        main = "Variance Explained by Principal Components", col = "skyblue")
eliminated_filtered<- eliminated_filtered %>%
  mutate(sofa_group = case_when(
    sofa_72 <= 4 ~ "Low",
    sofa_72 <= 8 ~ "Medium",
    TRUE ~ "High"
  ))
levels(eliminated_filtered$sofa_group)

eliminated_filtered$sofa_group <- factor(eliminated_filtered$sofa_group, levels = c("Low", "Medium", "High"))
# Install if needed

# Fit multinomial logistic regression using all variables except identifiers

# Build formula
# Replace spaces, special characters, and leading numbers
names(eliminated_filtered) <- make.names(names(eliminated_filtered))
# Exclude outcome and identifiers
predictors <- setdiff(names(eliminated_filtered), c("sofa_72", "sofa_group", "record_id", "admission_date", "dicharge_date"))
# Build formula
formula <- as.formula(paste("sofa_group ~", paste(predictors, collapse = " + ")))

# Fit model
model <- multinom(formula, data = eliminated_filtered)
summary(model)
exp(coef(model))  # exponentiate to get odds ratios
z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
p_values
pred_probs <- predict(model, type = "probs")
head(pred_probs)
pred_class <- predict(model)
head(pred_class)
table(Predicted = pred_class, Actual = eliminated_filtered$sofa_group)
mean(pred_class == eliminated_filtered$sofa_group)

#
library(caret)

pred_class <- predict(model)
conf_mat <- table(Predicted = pred_class, Actual = eliminated_filtered$sofa_group)

# Visualize as heatmap
library(reshape2)
conf_long <- melt(conf_mat)

ggplot(conf_long, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix", x = "Actual SOFA Group", y = "Predicted SOFA Group") +
  theme_minimal()


pred_probs <- predict(model, type = "probs")
pred_df <- cbind(eliminated_filtered, pred_probs)

# Average predicted probability by gender
library(dplyr)
pred_summary <- pred_df %>%
  group_by(gender) %>%
  summarize(across(c(Low, Medium, High), mean))

# Convert to long format for plotting
pred_long <- pred_summary %>%
  pivot_longer(cols = c(Low, Medium, High), names_to = "SOFA_Group", values_to = "Pred_Prob")

ggplot(pred_long, aes(x = gender, y = Pred_Prob, fill = SOFA_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Predicted Probability of SOFA Groups by Gender",
       x = "Gender", y = "Predicted Probability") +
  theme_minimal()

renv::snapshot()






































