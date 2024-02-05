#' Generate random values from standard parameterization of the Ex-Gaussian distribution
#'
#' @inheritParams brms::rexgaussian
#' @param tau Vector of scales of the exponential component.
#'
#' @export
rexgaussian2 <- function(n, mu, sigma, tau) {
  stats::rnorm(n, mean = mu, sd = sigma) +
    stats::rexp(n, rate = 1/tau)
}
