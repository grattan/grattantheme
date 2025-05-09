% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chart_helpers.R
\name{fct_case_when}
\alias{fct_case_when}
\title{Case when that orders the factors as you order the case when}
\usage{
fct_case_when(...)
}
\arguments{
\item{...}{a series of formulas to pass to dplyr::case_when}
}
\value{
a factor
}
\description{
This function is a wrapper around dplyr::case_when that ensures that
factor levels are ordered in the same way as the case_when statement.
}
