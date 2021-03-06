% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keeponly.R
\name{keeponly}
\alias{keeponly}
\title{Eliminate Consecutive Extreme Values}
\usage{
keeponly(x, extremes = c(0, 100), nconsec = 2)
}
\arguments{
\item{x}{A numeric vector, with no missing values.}

\item{extremes}{A numeric vector of length two giving the boundary limits for \code{x},
default c(0, 100).}

\item{nconsec}{An integer scalar, the maximum number of consecutive extreme values
allowed, default 2.}
}
\value{
A logical vector for selecting all elements of \code{x} without
    exceeding \code{nconsec} consecutive extreme values.
}
\description{
Generate the index for eliminating values beyond a given maximum number of
  consecutive extremes allowed.
}
\examples{
vec <- c(0, 0, 0, 4, 4, 4, 100, 100, 100, 100)
vec[keeponly(vec)]
# the original vector need not be ordered
vec <- c(100, 4, 100, 4, 0, 100, 0, 4, 0, 100)
keeponly(vec)
}
