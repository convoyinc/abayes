% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_priors.R
\name{calc_normal_gamma_dist}
\alias{calc_normal_gamma_dist}
\title{Calculate Parameters For Normal Gamma Distribution}
\usage{
calc_normal_gamma_dist(mu, tau, sigma_mu, sigma_tau)
}
\arguments{
\item{mu}{The expected value of x (see details)}

\item{tau}{The expected value of T (see details)}

\item{sigma_mu}{The standard deviation of x (see details)}

\item{sigma_tau}{The standard deviation of T (see details)}
}
\value{
An object of class \code{normal_gamma_dist}
}
\description{
Calculate the parameters for a normal gamma distribution parameterized
             by the expected value and standard deviation.
}
\details{
If (x, T) ~ NormalGamma(mu, lambda, alpha, beta), then \code{mu} = E[x],
         \code{tau} = E[T], \code{sigma_mu^2} = Var(x), and \code{sigma_tau^2} = Var(T)
}
