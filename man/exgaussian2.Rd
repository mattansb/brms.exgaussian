% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exgaussian2.R
\name{exgaussian2}
\alias{exgaussian2}
\title{\code{brms} family for the standard parameterization of the Ex-Gaussian distribution}
\usage{
exgaussian2(
  mu_link = "identity",
  sigma_link = "log",
  tau_link = "log",
  mu_positive = TRUE
)
}
\arguments{
\item{mu_link, sigma_link, tau_link}{Character of the type of link used to
model the ex-gaus parameter.}

\item{mu_positive}{Should \code{mu} be strictly positive? For reaction times, it
makes sense to set to \code{TRUE} (default).}
}
\value{
An object of class \code{c("customfamily", "brmsfamily", "family")} to be
used in \code{brms(family=)}.
}
\description{
\code{brms} family for the standard parameterization of the Ex-Gaussian distribution
}
