% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formultivariate.R
\name{formultivariate}
\alias{formultivariate}
\title{Format tab generated by Chloe to be used for the metrics correlations test.}
\usage{
formultivariate(chloe, dist, metrics)
}
\arguments{
\item{chloe}{Your tab of results produced by your analysis on Chloe}

\item{dist}{Vector of scales you choosed in Chloe when you created the ascii whith Cloe}

\item{metrics}{Vector of metrics you choosed in Chloe when you created the ascii whith Cloe}
}
\value{
Return a tab containing the metrics computed by Chloe formated to be be used by others functions
of this package
}
\description{
`formultivariate()` Return the metrics computed by Chloe in a way that others function of this package
can run the different models.
}
\details{
This function is made to be used to generate a tab used after in RV_Cor().
}
\author{
Pierre-Gilles Lemasle <pg.lemasle@gmail.com>
}
