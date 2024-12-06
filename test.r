#Load the package
library(devtools)
install_github("AU-R-Programming/FinalProject13")

# Load the necessary functions from your package
library(FinalProject13)

# Load dataset from extdata folder
data_path <- system.file("extdata", "expenses.csv", package = "FinalProject13")
expenses <- read.csv(data_path)

median_charges <- 9382

# Create a new column indicating whether charges are greater than the median
expenses$charges_above_median <- ifelse(expenses$charges > median_charges, 1, 0)

# Create a new column for age in years older than 18
expenses$years_older_than_min <- expenses$age - 18

# Create a new column for bmi greater than min of 15.96
expenses$bmi_greater_than_min <- expenses$bmi - 15.96

# Ensure that both 'bmi' and 'children' columns are numeric
expenses$years_older_than_min <- as.numeric(expenses$years_older_than_min)
expenses$bmi_greater_than_min <- as.numeric(expenses$bmi_greater_than_min)

# Create the predictor matrix (bmi and children columns) as a numeric matrix
X <- as.matrix(expenses[, c("years_older_than_min", "bmi_greater_than_min")])

# Response vector (smoker column)
y <- expenses$charges_above_median

# Apply the estimate_beta function to estimate the beta coefficients
model <- estimate_beta(X, y)

# Print estimated coefficients
cat("Estimated coefficients for the logistic regression model:\n")
print(model$coefficients)

# Make predictions on the same data (for testing)
y_pred <- predict_prob(X, model$coefficients)

# Calculate confusion metrics using a cutoff of 0.5 (common threshold for binary classification)
metrics <- confusion_metrics(y, y_pred, cutoff = 0.5)

# Print confusion matrix and evaluation metrics
cat("\nConfusion Matrix:\n")
print(metrics$confusion_matrix)

cat("\nModel Performance Metrics:\n")
cat("Prevalence: ", metrics$prevalence, "\n")
cat("Accuracy: ", metrics$accuracy, "\n")
cat("Sensitivity: ", metrics$sensitivity, "\n")
cat("Specificity: ", metrics$specificity, "\n")
cat("False Discovery Rate (FDR): ", metrics$fdr, "\n")
cat("Diagnostic Odds Ratio (DOR): ", metrics$dor, "\n")

#Bootstrap
# Select the columns we are interested in
# Create the predictor matrix (bmi and children columns) as a numeric matrix
X <- as.matrix(expenses[, c("years_older_than_min", "bmi_greater_than_min")])
# Response vector (smoker column)
y <- expenses$charges_above_median

# Perform bootstrap to calculate confidence intervals for the beta coefficients
bootstrap_result <- bootstrap_ci(X, y, alpha = 0.05, num_bootstrap = 20)

# Display the confidence intervals
print(bootstrap_result)
  


#references
#https://chatgpt.com/share/6751295b-685c-8010-9fc9-176f596d9850
#https://chatgpt.com/share/675277dc-b844-800a-ab65-b7aa75818653