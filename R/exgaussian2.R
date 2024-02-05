#' `brms` family for the standard parameterization of the Ex-Gaussian distribution
#'
#' @param mu_link,sigma_link,tau_link Character of the type of link used to
#'   model the ex-gaus parameter.
#' @param mu_positive Should `mu` be strictly positive? For reaction times, it
#'   makes sense to set to `TRUE` (default).
#'
#' @return An object of class `c("customfamily", "brmsfamily", "family")` to be
#'   used in `brms(family=)`.
#'
#' @export
exgaussian2 <- function(mu_link = "identity", sigma_link = "log", tau_link = "log",
                        mu_positive = TRUE) {
  mu_lb <- NA
  if (isTRUE(mu_positive)) {
    mu_lb <- 0
  }

  brms::custom_family(
    name = "exgaussian2",
    dpars = c("mu", "sigma", "tau"),
    links = c(mu_link, sigma_link, tau_link),
    type = "real",
    lb = c(mu_lb, 0, 0),
    ub = c(NA, NA, NA),
    loop = FALSE,

    log_lik = .log_lik_exgaussian2,
    posterior_predict = .posterior_predict_exgaussian2,
    posterior_epred = .posterior_epred_exgaussian2
  )
}


# functions -----------------

#' @keywords internal
.log_lik_exgaussian2 <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  tau <- brms::get_dpar(prep, "tau", i = i)

  y <- prep$data$Y[i]

  (1/tau) *
    exp(mu/tau + (sigma^2)/(2 * tau^2) - y/tau) *
    pnorm(y, mu + (1/tau) * sigma^2, sigma)
}

#' @keywords internal
.posterior_predict_exgaussian2 <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  tau <- brms::get_dpar(prep, "tau", i = i)

  n <- length(mu)

  if (isTRUE(any(sigma < 0))) {
    stop2("sigma must be non-negative.")
  }
  if (isTRUE(any(tau < 0))) {
    stop2("tau must be non-negative.")
  }

  rexgaussian2(n, mu, sigma, tau)
}

#' @keywords internal
.posterior_epred_exgaussian2 <- function(prep) {
  mu <- brms::get_dpar(prep, "mu")
  tau <- brms::get_dpar(prep, "tau")

  mu + tau
}
