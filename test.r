#Load the package
#library(devtools)
#install_github("AU-R-Programming/FinalProject13")

# Load the necessary functions from your package
library(FinalProject13)

# Load dataset from extdata folder
data_path <- system.file("extdata", "expenses.csv", package = "FinalProjectGroup13")
expenses <- read.csv(data_path)

# Preprocess the data
# Convert the 'smoker' column to binary (1 for "yes", 0 for "no")
expenses$smoker_binary <- ifelse(expenses$smoker == "yes", 1, 0)

# Extract relevant columns (children and smoker_binary)
X <- expenses$children  # predictor variable
y <- expenses$smoker_binary  # response variable

# Reshape X into a matrix (needed for logistic regression)
X_matrix <- matrix(X, ncol = 1)

# Run logistic regression to estimate the relationship between children and smoking status
model <- estimate_beta(X_matrix, y)

# Print estimated coefficients
cat("Estimated coefficients for the logistic regression model:\n")
print(model$coefficients)

# Make predictions on the same data (for testing)
y_pred <- predict_prob(X_matrix, model$coefficients)

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


#references
#https://chatgpt.com/share/6751295b-685c-8010-9fc9-176f596d9850
