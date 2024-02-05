#' `brms` `stanvars`  for the standard parameterization of the Ex-Gaussian distribution
#'
#' @param fixed_mu Is `mu` fixed (i.e., **not** modeled?)
#' @param fixed_sigma Is `sigma` fixed (i.e., **not** modeled?)
#' @param fixed_tau Is `tau` fixed (i.e., **not** modeled?)
#'
#' @return An object of class `c("stanvars")` to be used in `brms(stanvars=)`.
#'
#' @export
exgaussian2_stancode <- function(fixed_mu = FALSE, fixed_sigma = FALSE, fixed_tau = FALSE) {
  mu_type <- sigma_type <- tau_type <- "vector"
  if (fixed_mu) mu_type <- "real"
  if (fixed_sigma) sigma_type <- "real"
  if (fixed_tau) tau_type <- "real"

  exgaussian2_lpdf <- "
  real exgaussian2_lpdf(vector y, %s mu, %s sigma, %s tau) {
    return exp_mod_normal_lpdf(y | mu, sigma, inv(tau));
  }
"
  scode <- sprintf(exgaussian2_lpdf, mu_type, sigma_type, tau_type)

  stan_funs <- brms::stanvar(scode = scode, block = "functions")
}
