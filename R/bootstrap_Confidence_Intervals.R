#' @title Bootstrap Confidence Intervals for Beta Coefficients
#'
#' @description
#' Calculates the bootstrap confidence intervals for the estimated beta coefficients.
#'
#' @param X A matrix of predictor variables (features)
#' @param y A vector of binary response variables (0 or 1)
#' @param alpha The significance level (default is 0.05 for 95% confidence intervals)
#' @param num_bootstrap The number of bootstrap resamples (default is 20)
#'
#' @return A list containing:
#'   \item{lower_bound}{The lower bound of the confidence interval for each coefficient}
#'   \item{upper_bound}{The upper bound of the confidence interval for each coefficient}
#'
#' @examples
#' X <- matrix(rnorm(100 * 2), ncol = 2)
#' y <- rbinom(100, 1, 0.5)
#' ci <- bootstrap_ci(X, y, alpha = 0.05, num_bootstrap = 100)
#'
#' @export
bootstrap_ci <- function(X, y, alpha = 0.05, num_bootstrap = 20) {
  # Function to estimate beta coefficients
  estimate_beta <- function(X, y) {
    X <- cbind(1, X)
    starting_beta <- solve(t(X) %*% X) %*% t(X) %*% y
    loss_function <- function(beta) {
      p <- 1 / (1 + exp(-X %*% beta))
      p <- pmax(pmin(p, 0.9999), 0.0001)
      -sum(y * log(p) + (1 - y) * log(1 - p))
    }
    result <- optim(par = starting_beta, fn = loss_function)
    return(result$par)
  }
  
  # Perform bootstrap resampling
  bootstrap_coefs <- matrix(NA, nrow = num_bootstrap, ncol = ncol(X) + 1)
  
  for (i in 1:num_bootstrap) {
    # Resample indices
    indices <- sample(1:length(y), length(y), replace = TRUE)
    X_resample <- X[indices, ]
    y_resample <- y[indices]
    
    # Estimate beta on the bootstrap sample
    bootstrap_coefs[i, ] <- estimate_beta(X_resample, y_resample)
  }
  
  # Calculate the confidence intervals
  lower_bound <- apply(bootstrap_coefs, 2, function(coef) quantile(coef, alpha / 2))
  upper_bound <- apply(bootstrap_coefs, 2, function(coef) quantile(coef, 1 - alpha / 2))
  
  # Return the results
  list(
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}
