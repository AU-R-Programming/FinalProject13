#' @title Generate Confusion Matrix and Metrics
#' @description Computes confusion matrix and diagnostic metrics.
#' @param y_true Actual response values (binary: 0 or 1)
#' @param y_pred Predicted probabilities
#' @param cutoff Cutoff threshold for classification (default is 0.5)
#' @return A list containing confusion matrix and metrics
#' @export
confusion_metrics <- function(y_true, y_pred, cutoff = 0.5) {
  y_class <- ifelse(y_pred > cutoff, 1, 0)
  TP <- sum(y_true == 1 & y_class == 1)
  TN <- sum(y_true == 0 & y_class == 0)
  FP <- sum(y_true == 0 & y_class == 1)
  FN <- sum(y_true == 1 & y_class == 0)
  
  list(
    confusion_matrix = matrix(c(TP, FN, FP, TN), nrow = 2,
                              dimnames = list(c("Predicted 1", "Predicted 0"),
                                              c("Actual 1", "Actual 0"))),
    prevalence = mean(y_true),
    accuracy = (TP + TN) / length(y_true),
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    fdr = FP / (FP + TP),
    dor = (TP / FN) / (FP / TN)
  )
}


# References
#https://chatgpt.com/share/674f97f9-2ce0-8010-9ff6-3a008375a72d