% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_spatial.R
\name{cor_spatial}
\alias{cor_spatial}
\title{Produce a tab containing the value selected each x pixel on the map generated by Chloe per metric.}
\usage{
cor_spatial(directory, metrics, dist)
}
\arguments{
\item{directory}{The directory where your ascii generated with Chloe are stored}

\item{metrics}{Vector of metrics you choosed in Chloe when you created the ascii whith Cloe}

\item{dist}{Vector of scales you choosed in Chloe when you created the ascii whith Cloe}
}
\value{
Return a tab containing a the value selected each x pixel on the map generated by Chloe per metric
}
\description{
`cor_spatial()` Return a tab containing a the value selected each x pixel on the map generated by Chloe per metric.
}
\details{
This fonction is to used when you search to find correlation between metrics
at multiple scales and when your sampling is randomly selected across
the the landscape and patches. To test the correlation, you must used after that the
'temp()' function.
}
\section{Warning}{

This process can take some time depending of the size of your ascii and the numbers of your metrics
}

\author{
Pierre-Gilles Lemasle <pg.lemasle@gmail.com>
}
