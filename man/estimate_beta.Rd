% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Initial_Values.r
\name{estimate_beta}
\alias{estimate_beta}
\title{Estimate Beta Coefficients}
\usage{
estimate_beta(X, y)
}
\arguments{
\item{X}{A matrix of predictor variables (features)}

\item{y}{A vector of binary response variables (0 or )1}
}
\value{
A list containing:
  \item{coefficients}{The estimated beta coefficients}
  \item{fitted_values}{The predicted probabilities for the training data}
  \item{starting_beta}{Initial beta values calculated using least squares}
  \item{optimization_info}{A list containing additional optimization details:
    \itemize{
      \item{value}{The value of the loss function at the optimum}
      \item{counts}{Number of function and gradient evaluations}
    }
  }
}
\description{
estimates the coefficient vector βwhich includes the independent variables/predictors plus the intercept
}
\examples{
X <- matrix(rnorm(100 * 2), ncol = 2)
y <- rbinom(100, 1, 0.5)
model <- estimate_beta(X, y)

}
