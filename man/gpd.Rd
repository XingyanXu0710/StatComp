% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SA24204168R.R
\name{gpd}
\alias{gpd}
\title{Perform group-based panel data regression}
\usage{
gpd(x, y, G, kappa = 3)
}
\arguments{
\item{x}{Independent variable (dataframe)}

\item{y}{Dependent variable (dataframe)}

\item{G}{group number (int)}

\item{kappa}{Hyperparameters for calculating the IC value}
}
\value{
a list including coefficient, group for each member and IC value
}
\description{
Perform group-based panel data regression
}
\examples{
\dontrun{
set.seed(42)
n <- 10; t <- 10
x <- matrix(rnorm(n * t), nrow = n, ncol = t)
alpha <- runif(n, 0, 1)
epsilon <- matrix(rnorm(n * t, mean = 0, sd = sqrt(0.2)), nrow = n, ncol = t)
y <- matrix(NA, nrow = n, ncol = t)
for (i in 1:n) {
  if (i <= n / 2) {
    y[i, ] <- x[i, ] + epsilon[i, ] + alpha[i]
  } else {
      y[i, ] <- 2 * x[i, ] + epsilon[i, ] + alpha[i]
    }
}
gpd(x, y, 2)
}
}
