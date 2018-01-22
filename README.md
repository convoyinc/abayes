# abayes

## What is this package?

abayes is an R package that implements Bayesian methods for A/B testing. You can use this package to perform simulations of A/B tests in order to evaluate the Bayesian methods. You can also use this package to evaluate the results of a particular A/B test.

### Other Packages on Bayesian A/B Testing

I wrote most of this code before investigating the existence of open source alternatives. However, it is important to recognize them now. In order to evaluate A/B tests, a popular option is to use the [bayesAB](https://github.com/FrankPortman/bayesAB) package created by Frank Portman. In order to run simulations, David Robinson wrote code to perform simulations for [this blog post](http://varianceexplained.org/r/bayesian-ab-testing/). abayes lies somewhere in between these two packages. bayesAB can handle more complicated experiment settings and performs inferences faster, but is not designed for simulating data to analyze the performance of Bayesian methods. David's code is only designed for evaluating tests of binomial data and makes stronger assumptions about the simulation procedure than this package.

## Why did I write this package?

As a data scientist at Convoy, I investigated the performance of Bayesian A/B testing compared to Convoy's existing experimentation framework. After initial simulations showed that the speed and accuracy of Bayesian methods were promising, I wrote more simulation code and expanded the set of functions to support various experimental settings. As Bayesian A/B testing began to be used more and more throughout Convoy, I put all of the functions into a package and built a shiny app on top of that package. This allowed my co-workers to use my functions easily.

## Why did I open source this package?

As part of the blog post about Bayesian A/B testing, I included a visualization of the guarantees about controlling the expected loss. In the spirit of full transparency, I wanted to provide the script and the code that produced the visualization.

## How to install

This package is not on CRAN, and there is no plan on submitting it to CRAN. In order to install this package, you can use `devtools::install_github('convoyinc/abayes')` or you can clone the package locally and use `devtools::install_local('abayes')`.

## What can this package do?

For a detailed demonstration of the simulation capabilities, please see the vignette included in this repo. There, I provide an example of the simulations that demonstrate the guarantees that A/B testing makes.

However, this package can also be used to evaluate the results of a single A/B test. Below is some code that demonstrates how to use the package for that purpose.

``` r
library(abayes)
library(purrr)
library(data.table)
n <- 1000
prior_dist <- list(a = beta_dist(alpha = 1, beta = 1), b = beta_dist(alpha = 1, beta = 1))
data_dist <- list(a = bernoulli_dist(rate = 0.1), b = bernoulli_dist(rate = 0.12))
evidence_dt <- data.table::as.data.table(purrr::map(data_dist, function(x) simulate_data(x, n)))
posterior_dist <- update_priors(prior_dist, evidence_dt)

b_gt_a(posterior_dist[['a']], posterior_dist[['b']])
expected_loss_b(posterior_dist[['a']], posterior_dist[['b']])
plot_beta(posterior_dist)
```

## We welcome other open source contributions!

There are many ways that we can make this package better. If you have any ideas, please fork this repo and submit a pull request. Thanks for your interest in Bayesian A/B testing.
