# -------------------------------------------------------------------------------------------
# PACKAGES UPLOAD FOR NECESSARY STEPS
# -------------------------------------------------------------------------------------------

# 📌 Define Required Packages for Installation
required_packages <- c(
  # 📊 Data Manipulation & Transformation
  "tidyverse",  # Collection of essential R packages (ggplot2, dplyr, tidyr, etc.)
  "dplyr",      # Data wrangling (select, filter, mutate, summarise)
  "forcats",    # Handling and reordering categorical variables (factors)
  "readr",      # Efficient reading of CSV and tabular data formats
  
  # 📈 Data Visualization
  "ggplot2",    # Data visualization and graph plotting
  "ggthemes",   # Additional themes to improve ggplot2 aesthetics
  "scales",     # Formatting scales, percentages, axis labels
  "viridis",    # Color-blind friendly color palettes
  "RColorBrewer", # Alternative color palettes for plots
  "patchwork",  # Arranging multiple ggplot2 plots together
  "GGally",     # Extensions for ggplot2 (pair plots, correlation matrices)
  "ggcorrplot", # Visualization of correlation matrices
  "reshape2",   # Reshaping data for visualization (melt, dcast functions)
  
  # 🤖 Machine Learning Models
  "caret",      # ML framework: data preprocessing, training, evaluation
  "glmnet",     # Lasso, Ridge, and Elastic Net regularized regression
  "randomForest", # Random Forest algorithm for classification & regression
  "xgboost",    # High-performance Gradient Boosting implementation
  
  # 📊 Model Evaluation & Performance Metrics
  "pROC",       # ROC Curve and AUC calculations
  "PRROC",      # Precision-Recall and ROC curves for imbalanced datasets
  "cvTools",    # Cross-validation utilities for hyperparameter tuning
  
  # ⚖ Handling Imbalanced Data
  "smotefamily", # Synthetic Minority Over-sampling Technique (SMOTE)
  
  # 📑 Report Formatting & Tables
  "knitr",      # Dynamic report generation with R Markdown
  "kableExtra"  # Enhancing kable() for formatted tables
)

# Install Missing Packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load Installed Packages
lapply(required_packages, library, character.only = TRUE)


# Load required libraries

# Data Manipulation & Transformation
library(tidyverse)        # Core R package collection for data science (includes dplyr, ggplot2, tidyr, etc.)
library(dplyr)           # Data wrangling: select(), filter(), mutate(), summarise()
library(forcats)         # Better factor ordering and manipulation
library(readr)           # Efficient reading of CSV and other tabular data formats

# Data Visualization
library(ggplot2)         # Graph plotting and visualization
library(ggthemes)        # Additional themes for ggplot2 to enhance plots
library(scales)          # Formatting scales, percentages, and axis labels
library(viridis)         # Color-blind friendly color palettes
library(RColorBrewer)    # Additional color palettes for visualizations
library(patchwork)       # Helps in arranging multiple ggplot2 plots together
library(GGally)          # Extends ggplot2 with correlation matrix, pair plots
library(ggcorrplot)      # Visualizing correlation matrices using ggplot2
library(reshape2)        # Data reshaping (e.g., melt() & dcast() functions)

# Machine Learning Models
library(caret)           # Comprehensive ML framework: data preprocessing, model training, evaluation
library(glmnet)          # Lasso & Ridge regression, Elastic Net regularization
library(randomForest)    # Implements Random Forest algorithm
library(xgboost)         # Efficient Gradient Boosting implementation

# Model Evaluation & Performance Metrics
library(pROC)            # ROC Curve analysis and AUC calculations
library(PRROC)           # Precision-Recall and ROC curves for imbalanced datasets
library(cvTools)         # Cross-validation functions for hyperparameter tuning

# Handling Imbalanced Data
library(smotefamily)     # SMOTE (Synthetic Minority Over-sampling Technique) to balance datasets

# Table Formatting for Reports
library(knitr)           # Knitting and generating markdown tables
library(kableExtra)      # Extending kable() for well-formatted tables


# -------------------------------------------------------------------------------------------
# UPLOAD THE REQUIRED CSV FILE (CHANGE THE DIRECTORY ADDRESS AS PER SYSTEM). FILE IS NAMED AS "df"
# -------------------------------------------------------------------------------------------

# Upload the provided dataset and name it as "df"

df <- read_csv("/Users/basit/Desktop/Chevening-MSc at LSHTM/Term-2/Machine Learning/Assessment/assignment2025.csv")

# -------------------------------------------------------------------------------------------
# TASK 1-A: EXPLORATORY ANALYSIS USING HUMAN-FRIENDLY AND SIMPLIFIED INFORMATION
# -------------------------------------------------------------------------------------------

# Rename column names
df1 <- df %>%
  rename("Treatment 1" = treat1, "Treatment 2" = treat2, "Subtype" = subtype, 
         "Infarction" = Infarc, "Age" = age, "Delay" = delay, "Gender" = gender, 
         "Atrial" = atrial, "Death" = death, "CTScan" = CT)

# Convert categorical variables from Y/N to Yes/No
df1 <- df1 %>%
  mutate(across(c(wakesym, Atrial, CTScan, Infarction, hep24, asp3, symptom1:symptom8), 
                ~ recode(., "Y" = "Yes", "N" = "No")))

# Recode Treatments and Gender
df1 <- df1 %>%
  mutate(`Treatment 1` = recode(`Treatment 1`, "L" = "Low Treatment", 
                                "M" = "Medium Treatment", "N" = "Placebo"),
         `Treatment 2` = recode(`Treatment 2`, "Y" = "Treated", "N" = "Placebo"),
         `Gender` = recode(`Gender`, "M" = "Male", "F" = "Female"))

# -------------------------------------------------------------------------------------------
# TASK 1-A: VISUALIZATIONS
# -------------------------------------------------------------------------------------------

#####--- 📊 HISTOGRAM FOR DELAY IN TREATMENT

# Histogram (delay in time against patients enrolled)
ggplot(df1, aes(x = Delay)) +
  geom_histogram(binwidth = 5, 
                 fill = viridis(1, option = "C"), 
                 color = "darkgrey", alpha = 0.85) + 
  geom_density(aes(y = ..density.. * nrow(df1) * 5),  # Proper scaling using nrow()
               color = "darkred", size = 1.5, alpha = 0.7, adjust = 1.2) + 
  labs(
    title = "Distribution of Delay (Time to Treatment)",
    x = "Time Delay (hours)",
    y = "Number of Patients"
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 2500),
    breaks = seq(0, 2500, by = 500)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 HISTORGRAM (AGE)

# historgram (Age distribution among patients)

ggplot(df1, aes(x = `Age`)) +  # Use backticks if there's a space in the column name
  geom_histogram(binwidth = 5, 
                 fill = viridis(1, option = "B"), 
                 color = "darkgrey", alpha = 0.85) + 
  geom_density(aes(y = ..density.. * nrow(df1) * 5),  # Proper scaling using nrow()
               color = "darkred", size = 1.5, alpha = 0.7, adjust = 1.2) + 
  labs(
    title = "Distribution of Age (Years)",
    x = "Age (Years)",
    y = "Number of Patients"
  ) +
  scale_x_continuous(
    limits = c(10, 100),
    breaks = seq(10, 100, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = seq(0, 3000, by = 500)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 BAR CHART FOR OUTCOME

# Bar Chart for Death Outcome

ggplot(df1, aes(x = as.factor(Death), fill = as.factor(Death))) +
  geom_bar(alpha = 0.85, color = "black") +  
  geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)), 
            stat = "count", vjust = -0.5, size = 5, fontface = "bold") +  # Bold Percentage Labels
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00")) +  # Colour-Blind Friendly Palette
  labs(
    title = "Class Imbalance in Death Outcome",
    x = "Death Outcome (0 = Survived, 1 = Died)",
    y = "Number of Patients"
  ) +
  scale_y_continuous(
    limits = c(0, 12500),  # Slightly below the max for better spacing
    breaks = seq(0, 12500, by = 2000)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(face = "bold", size = 14),  
    legend.position = "none",
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 BAR CHART -- GENDER

# Gender Distribution Bar Chart
# Compute percentages
df_gender <- df1 %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n))

# Gender Distribution Bar Chart (Improved)
ggplot(df_gender, aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity", alpha = 0.85, color = "black") +  
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), 
            vjust = -0.5, size = 5, fontface = "bold") +  # Bold percentage labels
  scale_fill_manual(values = c("Female" = "#e31a1c", "Male" = "#1f78b4")) +  
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Number of Patients"
  ) +
  scale_y_continuous(
    limits = c(0, max(df_gender$n) * 1.1),  # Avoid cutoff
    breaks = seq(0, max(df_gender$n) * 1.1, by = 1000)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    legend.position = "none",
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 BAR CHART -- ATRIAL FIBRILLATION

# Atrial Fibrillation Distribution Bar Chart
# Compute percentages
df_atrial <- df1 %>%
  count(Atrial) %>%
  mutate(percentage = n / sum(n))

# Atrial Fibrillation Bar Chart (Improved)
ggplot(df_atrial, aes(x = Atrial, y = n, fill = Atrial)) +
  geom_bar(stat = "identity", alpha = 0.85, color = "black") +  
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), 
            vjust = -0.5, size = 5, fontface = "bold") +  # Bold percentage labels
  scale_fill_manual(values = c("No" = "#4daf4a", "Yes" = "#984ea3")) +  
  labs(
    title = "Atrial Fibrillation Distribution",
    x = "Atrial Fibrillation",
    y = "Number of Patients"
  ) +
  scale_y_continuous(
    limits = c(0, max(df_atrial$n) * 1.1),  
    breaks = seq(0, max(df_atrial$n) * 1.1, by = 1000)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    legend.position = "none",
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 BAR CHART -- TREATMENTS

# Stacked Bar Chart for Treatment 1 vs Treatment 2

ggplot(df1, aes(x = `Treatment 1`, fill = `Treatment 2`)) +
  geom_bar(position = "fill", alpha = 0.85, color = "black") +  
  scale_fill_manual(values = c("Placebo" = "#3b4992", "Treated" = "#e69f00"), name = "Treatment 2") +  # Colour-Blind Friendly (Dark Blue & Deep Orange)
  labs(
    title = "Treatment Distribution",
    x = "Treatment 1",
    y = "Proportion of Patients"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Show as percentages
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    legend.position = "top",  # Move legend inside for better balance
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 BAR CHART -- STROKE SUB-TYPES

# Create data frame with stroke subtype counts
df_stroke <- df1 %>%
  count(Subtype) %>%
  mutate(percentage = n / sum(n))  # Calculate percentages

# Final Stroke Subtype Bar Chart (Legend Removed)
ggplot(df_stroke, aes(x = Subtype, y = n, fill = Subtype)) +
  geom_bar(stat = "identity", alpha = 0.85, color = "black") +  
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), 
            vjust = -0.5, size = 5) +  # Add percentage labels
  scale_fill_manual(values = c("LACS" = "#377EB8", "OTH" = "#984EA3", "PACS" = "#E41A1C", 
                               "POCS" = "#FF7F00", "TACS" = "#FFFF33"), guide = "none") +  # Remove legend completely
  labs(
    title = "Stroke Subtype Distribution",
    x = "Stroke Subtype",
    y = "Number of Patients"
  ) +
  scale_y_continuous(
    limits = c(0, max(df_stroke$n) * 1.1),  # Adjusted to prevent cut-off
    breaks = seq(0, max(df_stroke$n) * 1.1, by = 1000)  # Auto-adjusted breaks
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18), 
    axis.title.x = element_text(face = "bold", size = 16),  
    axis.title.y = element_text(face = "bold", size = 16),  
    axis.text = element_text(size = 14),  
    panel.grid.major = element_line(size = 0.2, linetype = 'dotted'),
    panel.grid.minor = element_blank()
  )

#####--- 📊 HEATMAP -- KEY CORRELATION

# Correlation heatmap
# Select numerical variables
numeric_vars <- df1[, c("Delay", "Age", "sbp", "Death")]

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Select numerical variables
numeric_vars <- df1[, c("Delay", "Age", "sbp", "Death")]

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Plot the final improved heatmap
ggcorrplot(cor_matrix, 
           method = "square",  # Uniform shape
           type = "lower",  # Show only the lower triangle
           lab = TRUE,  # Display correlation values
           lab_size = 6,  # Larger text size
           colors = viridis(3),  # Colour-blind-friendly palette
           title = "Correlation Heatmap",
           outline.col = "black",  # Darker grid for better visibility
           legend.title = "Correlation Coefficient") +  # Clear legend title
  theme_minimal(base_size = 18) +  # Adjust text for better readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  # Larger, bolder title
    axis.title = element_blank(),  # Remove "Var1" and "Var2"
    legend.text = element_text(size = 16),  # Increase legend text size
    legend.title = element_text(size = 18, face = "bold"),  # Improve legend title visibility
    legend.position = "right",  # Move legend slightly away from the plot
    panel.grid.major = element_blank(),  # Remove background grid lines for a cleaner look
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Darker grid for better visibility
  )

# -------------------------------------------------------------------------------------------
# STEP 1-B: DATA LOADING & ONE-HOT ENCODING (ML-FRIENDLY VERSION)
# -------------------------------------------------------------------------------------------

# dataset "df" already uploaded, while exploratory analysis was performed earlier on human-friendly version "df1"
# this will reduce the burden of rearranging the file again

# Ensure valid column names
colnames(df) <- make.names(colnames(df))

# Convert categorical variables to factors
categorical_cols <- c("consc", "gender", "wakesym", "atrial", "CT", "Infarc", 
                      "hep24", "asp3", "subtype", "treat1", "treat2")

df[categorical_cols] <- lapply(df[categorical_cols], as.factor)


# One-Hot Encoding

# Define original categorical variables with correct names from `df`
original_categorical_vars <- c("consc", "gender", "wakesym", "atrial", "CT", "Infarc", 
                               "hep24", "asp3", "subtype", "treat1", "treat2")

# Apply One-Hot Encoding using model.matrix (ensuring death is retained)
df_encoded <- as.data.frame(model.matrix(~ . - 1, data = df))
df_encoded$death <- df$death  # Re-add target variable


# Extract encoded variable names from the dataset
encoded_vars <- colnames(df_encoded)

# Function to correctly count the number of new binary columns
count_encoded_columns <- function(var) {
  sum(grepl(paste0("^", gsub(" ", "", tolower(var))), encoded_vars)) + 
    sum(grepl(paste0("^", var, "[Y]"), encoded_vars))  # Handles "Y" added in encoding
}

# Generate updated summary table
encoding_summary <- data.frame(
  "Original Variable" = original_categorical_vars,
  "Unique Categories (Before Encoding)" = sapply(original_categorical_vars, function(var) n_distinct(df[[var]])),
  "New Binary Columns (After Encoding)" = sapply(original_categorical_vars, count_encoded_columns)
)

# Print the corrected summary table
print(encoding_summary)

# Check final column names in `df_encoded`
colnames(df_encoded)

# Save as CSV file if required (update the directory as per your system)
write_csv(encoding_summary, "/Users/basit/Desktop/Chevening-MSc at LSHTM/Term-2/Machine Learning/Assessment/one_hot_encoding_summary.csv")

# -------------------------------------------------------------------------------------------
# 📌 STEP 1-B: DATA SPLIT (TRAIN AND VALIDATION) & SMOTE APPLICATION ON TRAIN DATA ONLY
# -------------------------------------------------------------------------------------------

set.seed(123)  # Ensure reproducibility

# Perform Stratified Split (80% Train, 20% Validation)
splitIndex <- createDataPartition(df_encoded$death, p = 0.8, list = FALSE)
train_set <- df_encoded[splitIndex, ]
validation_set <- df_encoded[-splitIndex, ]

# Convert `death` to numeric (SMOTE requires numeric target)
train_set$death <- as.numeric(as.character(train_set$death))

# Compute Class Distribution Before SMOTE
class_dist_before <- prop.table(table(train_set$death)) * 100  # Convert proportions to percentages
cat("🔹 Class Distribution Before SMOTE:\n")
print(class_dist_before)

# Extract Features & Target Separately
X_train <- train_set[, -which(names(train_set) == "death")]  # Features (Excluding `death`)
y_train <- train_set$death  # Target variable

# Apply SMOTE (WITHOUT DOWN-SAMPLING)
set.seed(123)
train_smote <- SMOTE(X = X_train, target = y_train, K = 5, dup_size = 5)

# Convert SMOTE output back to dataframe
train_smote_data <- train_smote$data
colnames(train_smote_data)[ncol(train_smote_data)] <- "death"  # Rename target column
train_smote_data$death <- factor(train_smote_data$death, levels = c(0,1))  # Ensure factor format

# Compute Class Distribution After SMOTE
class_dist_after <- prop.table(table(train_smote_data$death)) * 100  # Convert proportions to percentages
cat("🔹 Class Distribution After SMOTE:\n")
print(class_dist_after)

# Assign `train_smote_data` directly to `train_set_balanced` (NO DOWN-SAMPLING)
train_set_balanced <- train_smote_data

# Print dataset dimensions
cat("Balanced Training Set (After SMOTE, No Down-Sampling):", dim(train_set_balanced), "\n")
cat("Validation Set:", dim(validation_set), "\n")

# -------------------------------------------------------------------------------------------
# STEP 1-B: BEFORE-AFTER SMOTE DATA VISUALIZATIONS
# -------------------------------------------------------------------------------------------

#####--- 📊 BAR CHART - SMOTE RESULTS

# Create data frames for plotting (Extracting directly from computed values)
smote_comparison <- data.frame(
  Dataset = factor(c("Before SMOTE", "After SMOTE"), levels = c("Before SMOTE", "After SMOTE")),
  Death_0 = c(class_dist_before["0"], class_dist_after["0"]),  # Class 0 percentages
  Death_1 = c(class_dist_before["1"], class_dist_after["1"])   # Class 1 percentages
)

# Convert data to long format for ggplot
smote_long <- reshape2::melt(smote_comparison, id.vars = "Dataset")

# Plot the bar chart
ggplot(smote_long, aes(x = Dataset, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.65, color = "white") +  # Stacked bar with white borders
  scale_fill_manual(values = c("#440154FF", "#FDE725FF"), labels = c("Survived = 0", "Died = 1")) +  # Viridis color-blind palette
  geom_text(aes(label = sprintf("%.1f%%", value)), 
            position = position_stack(vjust = 0.5), 
            size = 6, fontface = "bold",
            color = ifelse(smote_long$variable == "Death_0", "white", "black")) +  # White text on dark purple, black on yellow
  labs(
    title = "Class Distribution Before (Stratified Only) and After SMOTE",
    subtitle = "SMOTE enhances minority class representation while maintaining real-world distribution",
    x = "Dataset",
    y = "Percentage (%)",
    fill = "Class",
    caption = "One-Hot Encoded Version"
  ) +
  theme_minimal(base_size = 16) +  # Clean, elegant aesthetic
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),  # Centered caption
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.spacing.x = unit(0.6, 'cm'),  # Adjusted spacing in the legend
    axis.text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 16)
  )


# --------------------------------------------------
# STEP 2-A-A: STANDARDIZATION OF DATA
# --------------------------------------------------
numeric_features <- c("delay", "age", "sbp")

# Standardisation using training set mean and SD
preProc <- preProcess(train_set_balanced[, numeric_features], method = c("center", "scale"))
train_set_balanced[, numeric_features] <- predict(preProc, train_set_balanced[, numeric_features])
validation_set[, numeric_features] <- predict(preProc, validation_set[, numeric_features])

# Convert target variable to factor
train_set_balanced$death <- as.factor(train_set_balanced$death)
validation_set$death <- as.factor(validation_set$death)

# --------------------------------------------------
# STEP 2-A-B: PREPARE DATA FOR REGULARIZED REGRESSION
# --------------------------------------------------
X_train <- as.matrix(train_set_balanced[, -which(names(train_set_balanced) == "death")])  # Features
y_train <- train_set_balanced$death  # Target variable

X_validation <- as.matrix(validation_set[, -which(names(validation_set) == "death")])
y_validation <- validation_set$death

# --------------------------------------------------
# STEP 2-A-C: TRAIN REGULARIZED LOGISTIC REGRESSION (LASSO, RIDGE, ELASTIC NET ON DEFAULT SCORES)
# --------------------------------------------------

# Lasso (L1)
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)  
lasso_model <- glmnet(X_train, y_train, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)

# Ridge (L2)
cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)  
ridge_model <- glmnet(X_train, y_train, family = "binomial", alpha = 0, lambda = cv_ridge$lambda.min)

# Elastic Net (α = 0.5 initially)
cv_elastic <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0.5)  
elastic_model <- glmnet(X_train, y_train, family = "binomial", alpha = 0.5, lambda = cv_elastic$lambda.min)


# --------------------------------------------------
# STEP 2-A-D: COMPUTE PERFORMANCE METRICS
# --------------------------------------------------

compute_metrics <- function(true_labels, predicted_probs, threshold = 0.5) {
  # Convert probabilities to binary labels using the threshold
  predicted_labels <- factor(ifelse(predicted_probs > threshold, 1, 0), levels = c(0, 1))
  true_labels <- factor(true_labels, levels = c(0, 1))
  
  # Compute Confusion Matrix
  cm <- confusionMatrix(predicted_labels, true_labels, positive = "1")
  
  # Extract values from Confusion Matrix and ensure they are numeric
  TP <- as.numeric(cm$table[2, 2])  # True Positives
  TN <- as.numeric(cm$table[1, 1])  # True Negatives
  FP <- as.numeric(cm$table[2, 1])  # False Positives
  FN <- as.numeric(cm$table[1, 2])  # False Negatives
  
  # Prevent MCC overflow by ensuring denominator is not zero
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  # Compute MCC safely
  MCC <- ifelse(denominator > 0 & !is.na(denominator), ((TP * TN) - (FP * FN)) / denominator, NA)
  
  # Handle potential NaN or Inf issues in calculations
  MCC <- ifelse(is.nan(MCC) | is.infinite(MCC), NA, MCC)
  
  # Return performance metrics
  list(
    Accuracy = as.numeric(cm$overall["Accuracy"]),
    Recall = as.numeric(cm$byClass["Sensitivity"]),   # True Positive Rate
    Specificity = as.numeric(cm$byClass["Specificity"]), # True Negative Rate
    Precision = as.numeric(cm$byClass["Precision"]), # Positive Predictive Value
    F1_Score = ifelse(is.na(cm$byClass["Precision"]) | is.na(cm$byClass["Sensitivity"]) | 
                        (cm$byClass["Precision"] + cm$byClass["Sensitivity"] == 0), 
                      NA, 
                      2 * ((cm$byClass["Precision"] * cm$byClass["Sensitivity"]) /
                             (cm$byClass["Precision"] + cm$byClass["Sensitivity"]))),
    MCC = MCC,  # Matthews Correlation Coefficient
    Confusion_Matrix = cm$table
  )
}

#save the performance metrics
metrics_lasso <- compute_metrics(y_validation, predict(lasso_model, X_validation, type = "response"))
metrics_ridge <- compute_metrics(y_validation, predict(ridge_model, X_validation, type = "response"))
metrics_elastic <- compute_metrics(y_validation, predict(elastic_model, X_validation, type = "response"))

# --------------------------------------------------
# STEP 2-A-F: Performance metrics results table
# --------------------------------------------------
# Store results in a single data frame
performance_results <- data.frame(
  Model = c("Lasso (L1)", "Ridge (L2)", "Elastic Net"),
  Accuracy = c(metrics_lasso$Accuracy, metrics_ridge$Accuracy, metrics_elastic$Accuracy),
  Recall = c(metrics_lasso$Recall, metrics_ridge$Recall, metrics_elastic$Recall),
  Specificity = c(metrics_lasso$Specificity, metrics_ridge$Specificity, metrics_elastic$Specificity),
  Precision = c(metrics_lasso$Precision, metrics_ridge$Precision, metrics_elastic$Precision),
  F1_Score = c(metrics_lasso$F1_Score, metrics_ridge$F1_Score, metrics_elastic$F1_Score),
  MCC = c(metrics_lasso$MCC, metrics_ridge$MCC, metrics_elastic$MCC)
)

# Save results to CSV for future reference #USE THE REQUIRED DESTINATION FOLDER ADDRESS
write.csv(performance_results, "Model_Performance_Comparison.csv", row.names = FALSE)

# Print the performance summary
print(performance_results)

# --------------------------------------------------
# TASK 2-A: VISUALIZATION
# --------------------------------------------------

# Define a Color-Blind Friendly Palette
color_palette <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF", "#F94144", "#6A3D9A")


#####--- 📊 PERFORMANCE METRICS

# Convert metrics to percentage for better readability
performance_results_percentage <- performance_results
performance_results_percentage[, -1] <- round(performance_results_percentage[, -1] * 100, 1)

# Convert to long format for ggplot
performance_long <- melt(performance_results_percentage, id.vars = "Model")

# Adjust text placement dynamically based on value size
ggplot(performance_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.7, color = "black") +
  scale_fill_viridis_d(option = "C") +  # Color-blind friendly
  geom_text(
    aes(label = paste0(value, "%")),
    vjust = ifelse(performance_long$value > 80, 1.5, -0.3),
    size = 5, fontface = "bold",
    color = ifelse(performance_long$value > 80, "white", "black")  # Better contrast
  ) +
  facet_wrap(~ variable, scales = "fixed", ncol = 3) +
  ylim(0, max(performance_long$value) + 10) +  # Moved inside ggplot
  labs(
    title = "Model Performance Comparison: Lasso, Ridge, and Elastic Net",
    subtitle = "Comparing Regularised Regression Models Using Key Metrics (in %)",
    x = "Model Type",
    y = "Metric Score (%)",
    fill = "Metric Type"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",  # Remove legend (facet labels are clear)
    axis.text.x = element_text(angle = 15, vjust = 0.7),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold")  # Improve facet labels
  )


#####--- 📊 ROC CURVE 


# Compute ROC and AUC for each model
roc_lasso <- roc(y_validation, predict(lasso_model, X_validation, type = "response"))
roc_ridge <- roc(y_validation, predict(ridge_model, X_validation, type = "response"))
roc_elastic <- roc(y_validation, predict(elastic_model, X_validation, type = "response"))

# Extract AUC values
auc_lasso <- round(auc(roc_lasso), 3)
auc_ridge <- round(auc(roc_ridge), 3)
auc_elastic <- round(auc(roc_elastic), 3)

# Plot ROC Curves with transparency and better distinction
ggplot() +
  geom_line(aes(x = 1 - roc_lasso$specificities, y = roc_lasso$sensitivities, color = "Lasso (L1)"), linetype = "dotted", size = 1.5, alpha = 0.8) +
  geom_line(aes(x = 1 - roc_ridge$specificities, y = roc_ridge$sensitivities, color = "Ridge (L2)"), linetype = "dashed", size = 1.5, alpha = 0.8) +
  geom_line(aes(x = 1 - roc_elastic$specificities, y = roc_elastic$sensitivities, color = "Elastic Net"), linetype = "solid", size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("Lasso (L1)" = "blue", "Ridge (L2)" = "green", "Elastic Net" = "red")) +
  annotate("text", x = 0.4, y = 0.25, label = paste("AUC (Lasso) =", auc_lasso), size = 5, color = "blue", fontface = "bold") +
  annotate("text", x = 0.4, y = 0.2, label = paste("AUC (Ridge) =", auc_ridge), size = 5, color = "green", fontface = "bold") +
  annotate("text", x = 0.4, y = 0.15, label = paste("AUC (Elastic Net) =", auc_elastic), size = 5, color = "red", fontface = "bold") +
  labs(
    title = "ROC Curve Comparison: Lasso, Ridge, and Elastic Net",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Model"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18)
  )

#####--- 📊 CONFUSION MATRIX HEATMAP


# Function to plot confusion matrix with better readability
plot_confusion_matrix <- function(conf_matrix, model_name) {
  cm_data <- as.data.frame(conf_matrix$table)
  colnames(cm_data) <- c("Actual", "Predicted", "Count")
  
  # Compute percentage values
  total <- sum(cm_data$Count)
  cm_data$Percentage <- round(cm_data$Count / total * 100, 1)
  
  # Assign labels for TP, FP, TN, FN
  label_map <- c("TN", "FP", "FN", "TP")
  cm_data$Label <- label_map
  
  p <- ggplot(cm_data, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste(Label, "\n", Count, "\n", Percentage, "%")), color = "black", size = 6, fontface = "bold") +
    scale_fill_gradient(low = "white", high = "blue") +  # Monochrome blue gradient
    labs(
      title = paste("Confusion Matrix -", model_name),
      x = "Actual Class",
      y = "Predicted Class",
      fill = "Count"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18)
    )
  
  # Save plot
  ggsave(paste0("Confusion_Matrix_", model_name, ".jpeg"), plot = p, width = 8, height = 6, dpi = 300)
  print(p)  # Display plot inline
}

# file creation
cm_lasso <- confusionMatrix(factor(ifelse(predict(lasso_model, X_validation, type = "response") > 0.5, 1, 0), levels = c(0, 1)), 
                            factor(y_validation, levels = c(0, 1)))

cm_ridge <- confusionMatrix(factor(ifelse(predict(ridge_model, X_validation, type = "response") > 0.5, 1, 0), levels = c(0, 1)), 
                            factor(y_validation, levels = c(0, 1)))

cm_elastic <- confusionMatrix(factor(ifelse(predict(elastic_model, X_validation, type = "response") > 0.5, 1, 0), levels = c(0, 1)), 
                              factor(y_validation, levels = c(0, 1)))

#plot display
plot_confusion_matrix(cm_lasso, "Lasso")
plot_confusion_matrix(cm_ridge, "Ridge")
plot_confusion_matrix(cm_elastic, "Elastic Net")


#####--- 📊 COMPACT TOP FEATURE IMPORTANCE GRAPH (With Percentage Labels if required)


# Function to extract and prepare top features for visualization
extract_top_features <- function(model, model_name) {
  importance <- as.data.frame(as.matrix(coef(model)))
  colnames(importance) <- c("Coefficient")
  importance$Feature <- rownames(importance)
  importance$Coefficient <- as.numeric(importance$Coefficient)  # Ensure numeric values
  importance <- importance[order(abs(importance$Coefficient), decreasing = TRUE), ]
  importance <- importance[2:11, ]  # Exclude intercept, select top 10
  importance$Model <- model_name
  return(importance)
}

# Extract top features for each model
lasso_top_features <- extract_top_features(lasso_model, "Lasso")
ridge_top_features <- extract_top_features(ridge_model, "Ridge")
elastic_top_features <- extract_top_features(elastic_model, "Elastic Net")

# Combine all top features
top_features_combined <- bind_rows(lasso_top_features, ridge_top_features, elastic_top_features)

# Compute average importance to sort features globally
top_features_combined <- top_features_combined %>%
  group_by(Feature) %>%
  mutate(Avg_Importance = mean(abs(Coefficient))) %>%
  ungroup()

# Enhanced plot with color-blind friendly palette
ggplot(top_features_combined, aes(x = reorder(Feature, Avg_Importance), y = abs(Coefficient), fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_text(aes(label = round(abs(Coefficient), 2)),
            position = position_dodge(width = 0.7),
            vjust = ifelse(abs(top_features_combined$Coefficient) > 0.7, 1.5, -0.3), 
            size = 4, fontface = "bold", 
            color = "black") +  # Ensure good contrast
  coord_flip() +
  scale_fill_viridis_d(option = "C") +  # Color-blind friendly
  labs(title = "Top 10 Features for Lasso, Ridge, and Elastic Net",
       subtitle = "Ordered by Average Absolute Importance Across Models",
       x = "Feature",
       y = "Absolute Coefficient Value",
       fill = "Model") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        strip.text = element_text(size = 14, face = "bold"))



#####--- 📊 COMPACT TOP FEATURE IMPORTANCE GRAPH (Without Percentage Labels if required)

# Compute average importance to sort features globally
top_features_combined <- top_features_combined %>%
  group_by(Feature) %>%
  mutate(Avg_Importance = mean(abs(Coefficient))) %>%
  ungroup()

# Enhanced plot without direct percentage labels
ggplot(top_features_combined, aes(x = reorder(Feature, Avg_Importance), y = abs(Coefficient), fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  
  # Remove individual text labels for better clarity
  coord_flip() +
  
  # Use a color-blind friendly palette
  scale_fill_viridis_d(option = "C") +
  
  # Refined labels and theme
  labs(title = "Top 10 Features for Lasso, Ridge, and Elastic Net(Default)",
       subtitle = "Ordered by Average Absolute Importance Across Models",
       x = "Feature",
       y = "Absolute Coefficient Value",
       fill = "Model") +
  
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14, face = "italic"),
        strip.text = element_text(size = 14, face = "bold"))


# -------------------------------------------------------------------------------------------
# STEP 2-B-A: PARAMETER TUNING FOR IMPROVED MODELING
# -------------------------------------------------------------------------------------------

# Function to find the optimal threshold based on Youden’s Index and F1-score
tune_threshold <- function(true_labels, predicted_probs) {
  thresholds <- seq(0, 1, by = 0.01)
  metrics <- data.frame(Threshold = thresholds, Sensitivity = NA, Specificity = NA, F1 = NA)
  
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    predicted_labels <- factor(ifelse(predicted_probs > threshold, 1, 0), levels = c(0, 1))
    cm <- confusionMatrix(predicted_labels, true_labels, positive = "1")
    
    sensitivity <- cm$byClass["Sensitivity"]
    specificity <- cm$byClass["Specificity"]
    f1_score <- cm$byClass["F1"]
    
    metrics$Sensitivity[i] <- sensitivity
    metrics$Specificity[i] <- specificity
    metrics$F1[i] <- f1_score
  }
  
  # Find best thresholds
  best_threshold_youden <- metrics$Threshold[which.max(metrics$Sensitivity + metrics$Specificity - 1)]
  best_threshold_f1 <- metrics$Threshold[which.max(metrics$F1)]
  
  return(list(Youden = best_threshold_youden, F1 = best_threshold_f1, Metrics = metrics))
}

# Compute optimal thresholds for each model
opt_lasso <- tune_threshold(y_validation, predict(lasso_model, X_validation, type = "response"))
opt_ridge <- tune_threshold(y_validation, predict(ridge_model, X_validation, type = "response"))
opt_elastic <- tune_threshold(y_validation, predict(elastic_model, X_validation, type = "response"))

# Print best thresholds
cat("Best Threshold (Lasso) - Youden:", opt_lasso$Youden, "| F1-Score:", opt_lasso$F1, "\n")
cat("Best Threshold (Ridge) - Youden:", opt_ridge$Youden, "| F1-Score:", opt_ridge$F1, "\n")
cat("Best Threshold (Elastic Net) - Youden:", opt_elastic$Youden, "| F1-Score:", opt_elastic$F1, "\n")

# -------------------------------------------------------------------------------------------
# STEP 2-B-B: VISUALIZATIONS
# -------------------------------------------------------------------------------------------

#####--- 📊 COMPACT TOP FEATURE IMPORTANCE GRAPH

# Function to extract top features after tuning
extract_top_features_tuned <- function(model, model_name) {
  importance <- as.data.frame(as.matrix(coef(model)))
  colnames(importance) <- c("Coefficient")
  importance$Feature <- rownames(importance)
  importance$Coefficient <- as.numeric(importance$Coefficient)  # Ensure numeric values
  importance <- importance[order(abs(importance$Coefficient), decreasing = TRUE), ]
  importance <- importance[2:11, ]  # Exclude intercept, select top 10
  importance$Model <- model_name
  importance$Tuning <- "Tuned"
  return(importance)
}

# Extract top features after tuning
lasso_top_features_tuned <- extract_top_features_tuned(lasso_model, "Lasso")
ridge_top_features_tuned <- extract_top_features_tuned(ridge_model, "Ridge")
elastic_top_features_tuned <- extract_top_features_tuned(elastic_model, "Elastic Net")

# Combine all tuned features
top_features_tuned_combined <- bind_rows(lasso_top_features_tuned, ridge_top_features_tuned, elastic_top_features_tuned)

# Compute average importance to sort features globally
top_features_tuned_combined <- top_features_tuned_combined %>%
  group_by(Feature) %>%
  mutate(Avg_Importance = mean(abs(Coefficient))) %>%
  ungroup()


#####--- With Percentages if required

# Enhanced bar plot for feature importance after tuning
ggplot(top_features_tuned_combined, aes(x = reorder(Feature, Avg_Importance), y = abs(Coefficient), fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_text(aes(label = round(abs(Coefficient), 2)),
            position = position_dodge(width = 0.7),
            vjust = ifelse(abs(top_features_tuned_combined$Coefficient) > 0.7, 1.5, -0.3), 
            size = 4, fontface = "bold", 
            color = "black") +  # Ensure good contrast
  coord_flip() +
  scale_fill_viridis_d(option = "C") +  # Color-blind friendly
  labs(title = "Top 10 Features for Lasso, Ridge, and Elastic Net (Tuned)",
       subtitle = "Ordered by Average Absolute Importance Across Models After Tuning",
       x = "Feature",
       y = "Absolute Coefficient Value",
       fill = "Model") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        strip.text = element_text(size = 14, face = "bold"))


# Enhanced bar plot for feature importance after tuning
ggplot(top_features_tuned_combined, aes(x = reorder(Feature, Avg_Importance), y = abs(Coefficient), fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  
  # Remove percentage labels for a cleaner visual
  coord_flip() +
  
  # Use a color-blind friendly palette
  scale_fill_viridis_d(option = "C") +
  
  # Updated labels and title
  labs(title = "Top 10 Features for Lasso, Ridge, and Elastic Net (Tuned)",
       subtitle = "Ordered by Average Absolute Importance Across Models After Tuning",
       x = "Feature",
       y = "Absolute Coefficient Value",
       fill = "Model") +
  
  # Apply refined theme settings
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14, face = "italic"),
        strip.text = element_text(size = 14, face = "bold"))


#####--- Without Percentages if required

#####--- 📊 COMPARATIVE SUMMARY

# Summarise results
threshold_summary <- data.frame(
  Model = c("Lasso", "Ridge", "Elastic Net"),
  Best_Threshold_Youden = c(opt_lasso$Youden, opt_ridge$Youden, opt_elastic$Youden),
  Best_Threshold_F1 = c(opt_lasso$F1, opt_ridge$F1, opt_elastic$F1)
)

print(threshold_summary)

# Save results (Update the directory as per your system)
write.csv(threshold_summary, "Threshold_Optimization_Results.csv", row.names = FALSE)

#####--- 📊 COMBINED PERFORMANCE METRICS FOR DEFAULT AND OPTIMIZED RESULTS

compute_metrics <- function(true_labels, predicted_probs, threshold = 0.5) {
  predicted_labels <- factor(ifelse(predicted_probs > threshold, 1, 0), levels = c(0, 1))
  true_labels <- factor(true_labels, levels = c(0, 1))
  
  cm <- confusionMatrix(predicted_labels, true_labels, positive = "1")
  
  TP <- as.numeric(cm$table[2, 2])
  TN <- as.numeric(cm$table[1, 1])
  FP <- as.numeric(cm$table[2, 1])
  FN <- as.numeric(cm$table[1, 2])
  
  denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  MCC <- ifelse(denominator > 0 & !is.na(denominator), ((TP * TN) - (FP * FN)) / denominator, NA)
  
  list(
    Accuracy = as.numeric(cm$overall["Accuracy"]),
    Recall = as.numeric(cm$byClass["Sensitivity"]),
    Specificity = as.numeric(cm$byClass["Specificity"]),
    Precision = as.numeric(cm$byClass["Precision"]),
    F1_Score = ifelse(is.na(cm$byClass["Precision"]) | is.na(cm$byClass["Sensitivity"]) | 
                        (cm$byClass["Precision"] + cm$byClass["Sensitivity"] == 0), 
                      NA, 
                      2 * ((cm$byClass["Precision"] * cm$byClass["Sensitivity"]) /
                             (cm$byClass["Precision"] + cm$byClass["Sensitivity"]))),
    MCC = MCC,
    Confusion_Matrix = cm$table
  )
}

# Compute default and optimised performance metrics
metrics_default_lasso <- compute_metrics(y_validation, predict(lasso_model, X_validation, type = "response"), threshold = 0.5)
metrics_default_ridge <- compute_metrics(y_validation, predict(ridge_model, X_validation, type = "response"), threshold = 0.5)
metrics_default_elastic <- compute_metrics(y_validation, predict(elastic_model, X_validation, type = "response"), threshold = 0.5)

metrics_opt_lasso <- compute_metrics(y_validation, predict(lasso_model, X_validation, type = "response"), threshold = opt_lasso$F1)
metrics_opt_ridge <- compute_metrics(y_validation, predict(ridge_model, X_validation, type = "response"), threshold = opt_ridge$F1)
metrics_opt_elastic <- compute_metrics(y_validation, predict(elastic_model, X_validation, type = "response"), threshold = opt_elastic$F1)

# Combine results
performance_comparison <- data.frame(
  Model = rep(c("Lasso", "Ridge", "Elastic Net"), each = 2),
  Threshold_Type = rep(c("Default (0.5)", "Optimised (F1)"), times = 3),
  Accuracy = c(metrics_default_lasso$Accuracy, metrics_opt_lasso$Accuracy,
               metrics_default_ridge$Accuracy, metrics_opt_ridge$Accuracy,
               metrics_default_elastic$Accuracy, metrics_opt_elastic$Accuracy),
  Recall = c(metrics_default_lasso$Recall, metrics_opt_lasso$Recall,
             metrics_default_ridge$Recall, metrics_opt_ridge$Recall,
             metrics_default_elastic$Recall, metrics_opt_elastic$Recall),
  Specificity = c(metrics_default_lasso$Specificity, metrics_opt_lasso$Specificity,
                  metrics_default_ridge$Specificity, metrics_opt_ridge$Specificity,
                  metrics_default_elastic$Specificity, metrics_opt_elastic$Specificity),
  Precision = c(metrics_default_lasso$Precision, metrics_opt_lasso$Precision,
                metrics_default_ridge$Precision, metrics_opt_ridge$Precision,
                metrics_default_elastic$Precision, metrics_opt_elastic$Precision),
  F1_Score = c(metrics_default_lasso$F1_Score, metrics_opt_lasso$F1_Score,
               metrics_default_ridge$F1_Score, metrics_opt_ridge$F1_Score,
               metrics_default_elastic$F1_Score, metrics_opt_elastic$F1_Score),
  MCC = c(metrics_default_lasso$MCC, metrics_opt_lasso$MCC,
          metrics_default_ridge$MCC, metrics_opt_ridge$MCC,
          metrics_default_elastic$MCC, metrics_opt_elastic$MCC)
)

print(performance_comparison)

# Save to CSV (Update the directory as per your system)
write.csv(performance_comparison, "/Users/basit/Desktop/Chevening-MSc at LSHTM/Term-2/Machine Learning/Assessment/Performance_Comparison.csv", row.names = FALSE)


#####--- 📊 CONFUSION METRICS AND KEY PARAMETERS

# Filter data for each model
elastic_performance <- subset(performance_long, Model == "Elastic Net")
lasso_performance <- subset(performance_long, Model == "Lasso")
ridge_performance <- subset(performance_long, Model == "Ridge")

# Ensure data is in the correct format before visualization
performance_comparison_long <- reshape2::melt(performance_comparison, id.vars = c("Model", "Threshold_Type"))

# Function to generate bar charts for each model
plot_model_performance <- function(data, model_name) {
  ggplot(data, aes(x = Threshold_Type, y = value, fill = Threshold_Type)) +
    geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6, color = "black") +
    
    # Dynamically position text labels based on values
    geom_text(aes(label = paste0(round(value, 1), "%"), color = Threshold_Type), 
              position = position_dodge(0.6), 
              vjust = -0.5, size = 5, fontface = "bold") +
    
    facet_wrap(~ variable, scales = "free_y", ncol = 3, strip.position = "top") +  
    scale_fill_manual(values = c("Default (0.5)" = "#440154FF", "Optimised (F1)" = "#FDE725FF")) +  
    scale_color_manual(values = c("Default (0.5)" = "red", "Optimised (F1)" = "orange")) +  
    
    labs(
      title = paste("Performance Comparison:", model_name),
      subtitle = "Comparing Default vs. Optimised Thresholds",
      x = "Threshold Type",
      y = "Metric Score (%)",
      fill = "Threshold Type"
    ) +
    
    theme_minimal(base_size = 14) +  
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 15, vjust = 0.7, size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Generate plots for each model using filtered data
plot_model_performance(subset(performance_comparison_long, Model == "Lasso"), "Lasso")
plot_model_performance(subset(performance_comparison_long, Model == "Ridge"), "Ridge")
plot_model_performance(subset(performance_comparison_long, Model == "Elastic Net"), "Elastic Net")



# Function to compute confusion matrix and extract TP, TN, FP, FN
compute_conf_matrix <- function(true_labels, predicted_probs, threshold) {
  predicted_labels <- factor(ifelse(predicted_probs > threshold, 1, 0), levels = c(0, 1))
  true_labels <- factor(true_labels, levels = c(0, 1))
  
  cm <- confusionMatrix(predicted_labels, true_labels, positive = "1")
  
  return(list(
    Table = cm$table,
    Accuracy = as.numeric(cm$overall["Accuracy"]),
    Sensitivity = as.numeric(cm$byClass["Sensitivity"]),  # Recall
    Specificity = as.numeric(cm$byClass["Specificity"]),
    Precision = as.numeric(cm$byClass["Precision"]),
    F1_Score = as.numeric(cm$byClass["F1"])
  ))
}

# Compute confusion matrices for default and optimised thresholds
cm_default_lasso <- compute_conf_matrix(y_validation, predict(lasso_model, X_validation, type = "response"), threshold = 0.5)
cm_opt_lasso <- compute_conf_matrix(y_validation, predict(lasso_model, X_validation, type = "response"), threshold = opt_lasso$F1)

cm_default_ridge <- compute_conf_matrix(y_validation, predict(ridge_model, X_validation, type = "response"), threshold = 0.5)
cm_opt_ridge <- compute_conf_matrix(y_validation, predict(ridge_model, X_validation, type = "response"), threshold = opt_ridge$F1)

cm_default_elastic <- compute_conf_matrix(y_validation, predict(elastic_model, X_validation, type = "response"), threshold = 0.5)
cm_opt_elastic <- compute_conf_matrix(y_validation, predict(elastic_model, X_validation, type = "response"), threshold = opt_elastic$F1)


# Function to visualize confusion matrix
plot_confusion_matrix <- function(cm_data, model_name, threshold_type) {
  cm_df <- as.data.frame(cm_data$Table)
  colnames(cm_df) <- c("Actual", "Predicted", "Count")
  
  # Compute percentage values
  total <- sum(cm_df$Count)
  cm_df$Percentage <- round(cm_df$Count / total * 100, 1)
  
  # Define labels for clarity
  cm_labels <- c("TN", "FP", "FN", "TP")
  cm_df$Label <- cm_labels
  
  p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "black") +
    geom_text(aes(label = paste(Label, "\n", Count, "\n", Percentage, "%")),
              color = "black", size = 5, fontface = "bold") +
    scale_fill_gradient(low = "white", high = "blue") +  
    labs(title = paste("Confusion Matrix -", model_name, "-", threshold_type),
         x = "Actual Class",
         y = "Predicted Class",
         fill = "Count") +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(face = "bold", size = 18))
  
  return(p)
}

# Generate and display confusion matrix plots before saving
plot_cm_lasso_default <- plot_confusion_matrix(cm_default_lasso, "Lasso", "Default")
plot_cm_lasso_optimised <- plot_confusion_matrix(cm_opt_lasso, "Lasso", "Optimised")

plot_cm_ridge_default <- plot_confusion_matrix(cm_default_ridge, "Ridge", "Default")
plot_cm_ridge_optimised <- plot_confusion_matrix(cm_opt_ridge, "Ridge", "Optimised")

plot_cm_elastic_default <- plot_confusion_matrix(cm_default_elastic, "Elastic Net", "Default")
plot_cm_elastic_optimised <- plot_confusion_matrix(cm_opt_elastic, "Elastic Net", "Optimised")

# Explicitly print plots to ensure they are generated
print(plot_cm_lasso_default)

print(plot_cm_lasso_optimised)

print(plot_cm_ridge_default)

print(plot_cm_ridge_optimised)

print(plot_cm_elastic_default)

print(plot_cm_elastic_optimised)


#####--- 📊 AUC-ROC PLOT

# Compute ROC and AUC for optimized models using their respective F1-score thresholds
roc_lasso_opt <- roc(y_validation, predict(lasso_model, X_validation, type = "response"))
roc_ridge_opt <- roc(y_validation, predict(ridge_model, X_validation, type = "response"))
roc_elastic_opt <- roc(y_validation, predict(elastic_model, X_validation, type = "response"))

# Extract AUC values
auc_lasso_opt <- round(auc(roc_lasso_opt), 3)
auc_ridge_opt <- round(auc(roc_ridge_opt), 3)
auc_elastic_opt <- round(auc(roc_elastic_opt), 3)

# Create a data frame for visualization
roc_data_opt <- data.frame(
  FPR = c(1 - roc_lasso_opt$specificities, 1 - roc_ridge_opt$specificities, 1 - roc_elastic_opt$specificities),
  TPR = c(roc_lasso_opt$sensitivities, roc_ridge_opt$sensitivities, roc_elastic_opt$sensitivities),
  Model = rep(c("Lasso (L1)", "Ridge (L2)", "Elastic Net"), 
              times = c(length(roc_lasso_opt$sensitivities), length(roc_ridge_opt$sensitivities), length(roc_elastic_opt$sensitivities)))
)

# Generate the ROC plot for optimized models
ggplot(roc_data_opt, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +  # Reference diagonal
  scale_color_manual(values = c("Lasso (L1)" = "blue", "Ridge (L2)" = "green", "Elastic Net" = "red")) +
  annotate("text", x = 0.4, y = 0.25, label = paste("AUC (Lasso) =", auc_lasso_opt), size = 5, color = "blue", fontface = "bold") +
  annotate("text", x = 0.4, y = 0.2, label = paste("AUC (Ridge) =", auc_ridge_opt), size = 5, color = "green", fontface = "bold") +
  annotate("text", x = 0.4, y = 0.15, label = paste("AUC (Elastic Net) =", auc_elastic_opt), size = 5, color = "red", fontface = "bold") +
  labs(
    title = "ROC Curve Comparison (Tuned): Lasso, Ridge, and Elastic Net",
    subtitle = "Regularised Regression Models Based on AUC with Tuned Thresholds",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Model"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18)
  )


# -------------------------------------------------------------------------------------------
# STEP 3-A-A: CONVERT DATA INTO XGBOOST FORMAT
# -------------------------------------------------------------------------------------------

# Convert factor labels to numeric (XGBoost requires numeric target variable)
train_set_balanced$death <- as.numeric(as.character(train_set_balanced$death))
validation_set$death <- as.numeric(as.character(validation_set$death))

# Create `DMatrix` objects for training and validation
dtrain <- xgb.DMatrix(data = as.matrix(train_set_balanced[, -which(names(train_set_balanced) == "death")]), 
                      label = train_set_balanced$death)
dvalid <- xgb.DMatrix(data = as.matrix(validation_set[, -which(names(validation_set) == "death")]), 
                      label = validation_set$death)

# -------------------------------------------------------------------------------------------
# STEP 3-A-B: TRAIN XGBOOST MODEL (BASELINE)
# -------------------------------------------------------------------------------------------

# Set initial hyperparameters for XGBoost
xgb_params <- list(
  booster = "gbtree",        # Use tree-based boosting
  objective = "binary:logistic", # Binary classification
  eval_metric = "auc",       # Use AUC for performance evaluation
  eta = 0.1,                 # Learning rate
  max_depth = 6,             # Tree depth
  subsample = 0.8,           # Row sampling
  colsample_bytree = 0.8,    # Feature sampling
  min_child_weight = 1       # Minimum sum of instance weight needed in a child
)

# Train XGBoost model
set.seed(123)
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 150,             # Number of boosting iterations
  watchlist = list(train = dtrain, val = dvalid),
  print_every_n = 10,        # Print progress every 10 rounds
  early_stopping_rounds = 20 # Stop if validation AUC does not improve for 20 rounds
)

# -------------------------------------------------------------------------------------------
# STEP 3-A-C: EVALUATE MODEL PERFORMANCE
# -------------------------------------------------------------------------------------------

# Predict probabilities on validation set
xgb_predictions_prob <- predict(xgb_model, dvalid)

# Convert probabilities to class labels using threshold = 0.5
xgb_predictions <- ifelse(xgb_predictions_prob > 0.5, 1, 0)

# Compute confusion matrix
conf_matrix_default <- confusionMatrix(factor(xgb_predictions, levels = c(0,1)), 
                                       factor(validation_set$death, levels = c(0,1)))

# Extract key metrics for default model
accuracy_xg_default <- as.numeric(conf_matrix_default$overall["Accuracy"])
recall_xg_default <- as.numeric(conf_matrix_default$byClass["Sensitivity"])  
specificity_xg_default <- as.numeric(conf_matrix_default$byClass["Specificity"]) 
precision_xg_default <- as.numeric(conf_matrix_default$byClass["Precision"])

# Compute F1-score safely
if (!is.na(precision_xg_default) & !is.na(recall_xg_default) & (precision_xg_default + recall_xg_default) > 0) {
  f1_score_xg_default <- 2 * (precision_xg_default * recall_xg_default) / (precision_xg_default + recall_xg_default)
} else {
  f1_score_xg_default <- NA
}

# Store results separately for visualization
performance_results_default <- data.frame(
  Metric = c("Accuracy", "Recall (Sensitivity)", "Specificity", "Precision", "F1-Score"),
  Value = c(accuracy_xg_default, recall_xg_default, specificity_xg_default, precision_xg_default, f1_score_xg_default)
)

# -------------------------------------------------------------------------------------------
# STEP 3-A-D: VISUALISATIONS
# -------------------------------------------------------------------------------------------

#####--- 📊 FEATURE IMPORTANCE GRAPH

# Feature Importance Plot
importance_matrix <- xgb.importance(feature_names = colnames(train_set_balanced[, -which(names(train_set_balanced) == "death")]), 
                                    model = xgb_model)

ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(title = "Feature Importance (Gradient Boosting)",
       x = "Feature",
       y = "Importance (Gain)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))

#####--- 📊 PERFORMANCE METRICS COMPARISON GRAPH (Default)

# Store default results in a single data frame
performance_results_default <- data.frame(
  Metric = c("Accuracy", "Recall (Sensitivity)", "Specificity", "Precision", "F1-Score"),
  Value = c(accuracy_xg_default, recall_xg_default, specificity_xg_default, precision_xg_default, f1_score_xg_default)
)

# Convert values to percentage
performance_results_default$Value <- round(performance_results_default$Value * 100, 2)

# Plot with improved label positioning and contrast
ggplot(performance_results_default, aes(x = reorder(Metric, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  
  # Position labels dynamically based on value size
  geom_text(aes(label = paste0(Value, "%")),
            hjust = ifelse(performance_results_default$Value < 10, -0.2, 1.2),  # Outside for small values, inside for large ones
            color = ifelse(performance_results_default$Value < 10, "black", "white"),  # White text for dark bars
            size = 6, fontface = "bold") +
  
  scale_fill_viridis_d(option = "C") +  # Colour-blind friendly
  coord_flip() +  # Flip axis for better readability
  labs(title = "Performance Metrics - XGBoost (Default)",
       subtitle = "Threshold = 0.5 (Baseline Performance)",
       x = "Metric",
       y = "Score (%)") +
  
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        legend.position = "none")  # Remove legend (not needed)



#####--- 📊 STEP 3-A-E: ROC CURVE & AUC

# Compute ROC and AUC
roc_curve <- roc(validation_set$death, xgb_predictions_prob)
auc_xgb <- auc(roc_curve)

# Plot ROC Curve
ggplot() +
  geom_line(aes(x = 1 - roc_curve$specificities, y = roc_curve$sensitivities), 
            color = "blue", size = 1.5) +
  geom_abline(linetype = "dashed", color = "gray") +  # Diagonal reference line
  annotate("text", x = 0.4, y = 0.2, label = paste("AUC =", round(auc_xgb, 3)), 
           size = 6, color = "blue", fontface = "bold") +
  labs(title = "ROC Curve - XGBoost",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))

#####--- 📊 STEP 3-A-F: CONFUSION MATRIX HEATMAP (Default XGBoost)

# Prepare data for heatmap
conf_matrix_df <- as.data.frame(conf_matrix_default$table)
colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")

# Plot Confusion Matrix Heatmap
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix - XGBoost (Default)",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Count") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))

# -------------------------------------------------------------------------------------------
# STEP 3-B-A: GRID SEARCH FOR HYPERPARAMETER TUNING
# -------------------------------------------------------------------------------------------

# Define hyperparameter tuning grid
grid_xgb_params_optt <- expand.grid(
  eta = c(0.01, 0.05, 0.1),             # Learning rate
  max_depth = c(4, 6, 8),               # Maximum tree depth
  min_child_weight = c(1, 3, 5),        # Minimum child weight
  scale_pos_weight = c(1, 2, 3),        # Class imbalance correction
  early_stopping_rounds = c(10, 20, 30) # Early stopping rounds
)

# Perform 5-fold cross-validation
set.seed(123)
cv_xgb_results_optt <- lapply(1:nrow(grid_xgb_params_optt), function(i) {
  
  params_xgb_optt <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = grid_xgb_params_optt$eta[i],
    max_depth = grid_xgb_params_optt$max_depth[i],
    min_child_weight = grid_xgb_params_optt$min_child_weight[i],
    scale_pos_weight = grid_xgb_params_optt$scale_pos_weight[i]
  )
  
  cv_xgb_optt <- xgb.cv(
    params = params_xgb_optt,
    data = dtrain,
    nrounds = 150,
    nfold = 5,
    metrics = "auc",
    early_stopping_rounds = grid_xgb_params_optt$early_stopping_rounds[i],
    verbose = FALSE
  )
  
  return(list(
    params = params_xgb_optt,
    best_auc = max(cv_xgb_optt$evaluation_log$test_auc_mean),
    best_nrounds = which.max(cv_xgb_optt$evaluation_log$test_auc_mean)
  ))
})

# Extract best model
best_xgb_model_optt <- cv_xgb_results_optt[[which.max(sapply(cv_xgb_results_optt, function(x) x$best_auc))]]
best_xgb_params_optt <- best_xgb_model_optt$params
best_xgb_nrounds_optt <- best_xgb_model_optt$best_nrounds

cat("🔹 Best Parameters Selected (Optimized):\n")
print(best_xgb_params_optt)
cat("🔹 Optimal Rounds (Optimized):", best_xgb_nrounds_optt, "\n")

# -------------------------------------------------------------------------------------------
# STEP 3-B-B: TRAIN FINAL MODEL USING BEST PARAMETERS (Optimized)
# -------------------------------------------------------------------------------------------

set.seed(123)
final_xgb_model_optt <- xgb.train(
  params = best_xgb_params_optt,
  data = dtrain,
  nrounds = best_xgb_nrounds_optt,
  watchlist = list(train = dtrain, val = dvalid),
  early_stopping_rounds = best_xgb_params_optt$early_stopping_rounds,
  print_every_n = 10
)

# Predict probabilities on validation set
xgb_pred_probs_optt <- predict(final_xgb_model_optt, dvalid)

# -------------------------------------------------------------------------------------------
# STEP 3-B-C: FIND BEST DECISION THRESHOLD (Optimized)
# -------------------------------------------------------------------------------------------

library(pROC)

# Compute ROC curve
roc_xgb_optt <- roc(validation_set$death, xgb_pred_probs_optt)

# Create dataframe for threshold tuning
roc_xgb_df_optt <- data.frame(
  threshold = roc_xgb_optt$thresholds,
  sensitivity = roc_xgb_optt$sensitivities,
  specificity = roc_xgb_optt$specificities
)

# Compute Youden’s Index (Sensitivity + Specificity - 1)
roc_xgb_df_optt$youden_index <- roc_xgb_df_optt$sensitivity + roc_xgb_df_optt$specificity - 1

# Compute F1-score for each threshold
roc_xgb_df_optt$precision <- roc_xgb_df_optt$sensitivity / (roc_xgb_df_optt$sensitivity + (1 - roc_xgb_df_optt$specificity))
roc_xgb_df_optt$f1_score <- 2 * ((roc_xgb_df_optt$precision * roc_xgb_df_optt$sensitivity) / (roc_xgb_df_optt$precision + roc_xgb_df_optt$sensitivity))

# Find best thresholds
best_xgb_threshold_youden_optt <- roc_xgb_df_optt$threshold[which.max(roc_xgb_df_optt$youden_index)]
best_xgb_threshold_f1_optt <- roc_xgb_df_optt$threshold[which.max(roc_xgb_df_optt$f1_score)]

cat("🔹 Best Threshold (Youden’s Index - Optimized):", round(best_xgb_threshold_youden_optt, 4), "\n")
cat("🔹 Best Threshold (F1-score - Optimized):", round(best_xgb_threshold_f1_optt, 4), "\n")

# Apply best threshold (F1-score) for classification
xgb_final_preds_optt <- ifelse(xgb_pred_probs_optt > best_xgb_threshold_f1_optt, 1, 0)

# -------------------------------------------------------------------------------------------
#  STEP 3-B-D: VISUALIZATIONS
# -------------------------------------------------------------------------------------------

#####--- 📊 CONFUSION MATRIX

# Compute confusion matrix for optimized model
conf_xgb_matrix_optt <- confusionMatrix(factor(xgb_final_preds_optt, levels = c(0,1)), 
                                        factor(validation_set$death, levels = c(0,1)))

# Extract key metrics for optimized model
accuracy_xgb_optt <- as.numeric(conf_xgb_matrix_optt$overall["Accuracy"])
recall_xgb_optt <- as.numeric(conf_xgb_matrix_optt$byClass["Sensitivity"])  
specificity_xgb_optt <- as.numeric(conf_xgb_matrix_optt$byClass["Specificity"]) 
precision_xgb_optt <- as.numeric(conf_xgb_matrix_optt$byClass["Precision"])

# Compute F1-score (do NOT convert to percentage)
f1_score_xgb_optt <- if (!is.na(precision_xgb_optt) & !is.na(recall_xgb_optt) & (precision_xgb_optt + recall_xgb_optt) > 0) {
  2 * (precision_xgb_optt * recall_xgb_optt) / (precision_xgb_optt + recall_xgb_optt)
} else {
  NA
}

# Convert Accuracy, Recall, Specificity, Precision, and F1-Score to percentage
performance_xgb_results_optt <- data.frame(
  Metric = c("Accuracy", "Recall (Sensitivity)", "Specificity", "Precision", "F1-Score"),
  Value = c(accuracy_xgb_optt * 100,  # Convert to percentage
            recall_xgb_optt * 100,   # Convert to percentage
            specificity_xgb_optt * 100,  # Convert to percentage
            precision_xgb_optt * 100,  # Convert to percentage
            f1_score_xgb_optt * 100)  # Convert F1-Score to percentage correctly
)

# Print to confirm correct formatting
cat("\n🔹 XGBoost Optimized Model Performance Metrics (Fixed):\n")
print(performance_xgb_results_optt)


#####--- 📊 PERFORMANCE METRICS

ggplot(performance_xgb_results_optt, aes(x = reorder(Metric, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  
  # Adjust label position dynamically for better visibility
  geom_text(aes(label = paste0(Value, "%"),
                color = ifelse(Metric == "Specificity", "black",  # Use black for yellow bar
                               ifelse(Value > 60, "white", "black"))),  
            hjust = ifelse(performance_xgb_results_optt$Value > 60, 1.1, -0.2),  
            size = 6, fontface = "bold") +  
  
  scale_fill_viridis_d(option = "C") +  
  coord_flip() +  
  labs(title = "Performance Metrics - XGBoost (Optimized)",
       subtitle = "Threshold adjusted for better classification",
       x = "Metric",
       y = "Score (%)") +
  
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        legend.position = "none")  # Remove legend (not needed)


#####--- 📊 CONFUSION MATRIX HEATMAP

# calculations
conf_matrix_df_optt <- as.data.frame(conf_xgb_matrix_optt$table)
colnames(conf_matrix_df_optt) <- c("Actual", "Predicted", "Count")

ggplot(conf_matrix_df_optt, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix - XGBoost (Optimized)",
       x = "Actual Class",
       y = "Predicted Class",
       fill = "Count") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))


#####--- 📊 ROC CURVE (OPTIMIZED VERSION)

# ggploting
ggplot() +
  geom_line(aes(x = 1 - roc_xgb_optt$specificities, y = roc_xgb_optt$sensitivities), color = "blue", size = 1.5) +
  geom_abline(linetype = "dashed", color = "gray") +  
  annotate("text", x = 0.4, y = 0.2, label = paste("AUC =", round(auc(roc_xgb_optt), 3)), 
           size = 6, color = "blue", fontface = "bold") +
  labs(title = "ROC Curve - XGBoost (Optimized)",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))


#####--- 📊 FEATURE IMPORTANCE GRAPH

#calculations
importance_matrix_optt <- xgb.importance(
  feature_names = colnames(train_set_balanced[, -which(names(train_set_balanced) == "death")]), 
  model = final_xgb_model_optt
)

ggplot(importance_matrix_optt, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(title = "Feature Importance - XGBoost (Optimized)",
       x = "Feature",
       y = "Importance (Gain)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))



# -------------------------------------------------------------------------------------------
# 📊 ADDTIONAL CODING: ENHANCED FEATURE IMPORTANCE GRAPHS FOR DEFAULT AND OPTIMIZED BOOSTING
# -------------------------------------------------------------------------------------------

#####--- 📊 FEATURE IMPORTANCE MONO-CHROME HEAT MAP

# Ensure Feature Importance is Extracted Correctly
importance_matrix <- xgb.importance(
  feature_names = colnames(train_set_balanced[, -which(names(train_set_balanced) == "death")]), 
  model = xgb_model
)

# Feature Importance Plot
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  labs(title = "Feature Importance - XGBoost",
       x = "Feature",
       y = "Importance (Gain)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))


# Extract Feature Importance for Optimized Model
importance_matrix_optt <- xgb.importance(
  feature_names = colnames(train_set_balanced[, -which(names(train_set_balanced) == "death")]), 
  model = final_xgb_model_optt
)

# Feature Importance Plot for Optimized Model
ggplot(importance_matrix_optt, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  coord_flip() +
  labs(title = "Feature Importance - XGBoost (Optimized)",
       x = "Feature",
       y = "Importance (Gain)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18))


