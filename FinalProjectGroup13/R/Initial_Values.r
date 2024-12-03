#' @title Estimate Beta Coefficients
#' 
#' @description
#' estimates the coefficient vector βwhich includes the independent variables/predictors plus the intercept
#' 
#' @param X A matrix of predictor variables (features)
#' @param y A vector of binary response variables (0 or )1
#' 
#' @return A list containing:
#'   \item{coefficients}{The estimated beta coefficients}
#'   \item{fitted_values}{The predicted probabilities for the training data}
#'   \item{starting_beta}{Initial beta values calculated using least squares}
#'   \item{optimization_info}{A list containing additional optimization details:
#'     \itemize{
#'       \item{value}{The value of the loss function at the optimum}
#'       \item{counts}{Number of function and gradient evaluations}
#'     }
#'   }
#'
#' @examples
#' X <- matrix(rnorm(100 * 2), ncol = 2)
#' y <- rbinom(100, 1, 0.5)
#' model <- estimate_beta(X, y)
#' 
#' @export
estimate_beta <- function(X, y) {
    # Add intercept term column
    X <- cbind(1, X)
    #get starting beta using least squares
    starting_beta <- solve(t(X) %*% X) %*% t(X) %*% y
    #Define loss function
    loss_function <- function(beta) {
        #calculate probabilities
        p <- 1 / (1 + exp(-X %*% beta))
        # make sure probabilites are between 0 and 1 so log works as intended
        p <- pmax(pmin(p, 0.9999), 0.0001) 
        # Return result of loss function
        -sum(y * log(p) + (1 - y) * log(1 - p))
    }
    # find the most optimized beta values
    result <- optim(
        par = starting_beta,
        fn = loss_function
    )

    # calculated fitted probabilites
    fitted_probabilities <- 1 / (1 + exp(-X %*% result$par))
    # Return results
    list(
        coefficients = result$par,
        fitted_values = fitted_probabilities,
        starting_beta = starting_beta,
        optimization_info = list(
            value = result$value,
            counts = result$counts
        )
    )
}

#' @title Predict Probabilities for Logistic Regression
#' 
#' @description
#' Calculates prediction probabilities for new data using estimated beta coefficients
#' from a logistic regression model.
#' 
#' @param X A matrix of predictor variables (features) for new data
#' @param beta A vector of estimated beta coefficients (including intercept)
#' 
#' @return A vector of predicted probabilities between 0 and 1
#' 
#' @examples
#' X_new <- matrix(rnorm(50 * 2), ncol = 2)
#' predictions <- predict_prob(X_new, model$coefficients)
#'
#' @export
predict_prob <- function(X, beta) {
    # Add intercept
    X <- cbind(1, X)
    1 / (1 + exp(-X %*% beta))
}

## Sources
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/solve
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list
#https://roxygen2.r-lib.org/articles/roxygen2.html
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optim
