# This file estimates beta coefficients for logistic regression
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
        convergence = result$convergence,
        starting_beta = starting_beta,
        optimization_info = list(
            value = result$value,
            hessian = result$hessian,
            counts = result$counts
        )
    )
}

# This function calculates prediction probabilities for new data
predict_prob <- function(X, beta) {
    # Add intercept
    X <- cbind(1, X)
    1 / (1 + exp(-X %*% beta))
}