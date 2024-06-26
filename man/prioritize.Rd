% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{prioritize}
\alias{prioritize}
\title{Calculate the apparent relative risk for all covariates}
\usage{
prioritize(dt, pid, expo, outc, correction = TRUE)
}
\arguments{
\item{dt}{Dataset after reccurent assessment in data.table format}

\item{pid}{The column name for patient ID}

\item{expo}{The column name for exposure}

\item{outc}{The column name for outcome}

\item{correction}{When the outcome is rare, one of the 2 by 2 table could be zero. In this case, the algorithm will add 0.1 to the cell.}
}
\value{
A data.table including the apparent relative risk for all covariates
}
\description{
Prioritise the large pool of covariates generated in the previous step is prioritised. This is typically achieved using the Bross formula, which uses univariate associations of covariates with treatment and outcome, to identify those with the highest potential tobias the treatment-outcome relationship
}
