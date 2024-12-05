#' @title Bootstrap Confidence Intervals for Beta Coefficients
#' 
#' @description
#' Calculates bootstrap confidence intervals for estimated beta coefficients.
#' 
#' @param X A matrix of predictor variables
#' @param y A vector of binary response variables
#' @param num_bootstraps Number of bootstrap samples (default is 20)
#' @param alpha Significance level for confidence intervals (default is 0.05)
#' 
#' @return A matrix with lower and upper bounds for each beta coefficient
#' 
#' @examples
#' ci <- bootstrap_ci(X, y, num_bootstraps = 100, alpha = 0.05)
#' 
#' @export
bootstrap_ci <- function(X, y, num_bootstraps = 20, alpha = 0.05) {
  X <- cbind(1, X)  # Add intercept term
  n <- nrow(X)
  beta_boot <- matrix(NA, nrow = num_bootstraps, ncol = ncol(X))
  
  for (i in seq_len(num_bootstraps)) {
    indices <- sample(seq_len(n), size = n, replace = TRUE)
    X_boot <- X[indices, ]
    y_boot <- y[indices]
    beta_boot[i, ] <- estimate_beta(X_boot, y_boot)$coefficients
  }
  
  # Compute percentile confidence intervals
  lower_bound <- apply(beta_boot, 2, quantile, probs = alpha / 2)
  upper_bound <- apply(beta_boot, 2, quantile, probs = 1 - alpha / 2)
  
  cbind(lower_bound, upper_bound)
}


#References
#https://chatgpt.com/share/674f97f9-2ce0-8010-9ff6-3a008375a72d
