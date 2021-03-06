% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazyraster.R
\name{as_raster}
\alias{as_raster}
\title{Convert to in-memory raster}
\usage{
as_raster(
  x,
  dim = NULL,
  resample = "NearestNeighbour",
  native = FALSE,
  band = 1L
)
}
\arguments{
\item{x}{a \link{lazyraster}}

\item{dim}{dimensions, pixel size in rows and columns}

\item{resample}{resampling method, see \link[vapour:vapour_read_raster]{vapour::vapour_read_raster}}

\item{native}{return raster at native resolution, default is \code{FALSE}}

\item{band}{set to 1-based band numbers to return (independently of those set with \code{\link[=lazyraster]{lazyraster()}})}
}
\value{
a regular raster 'BasicRaster' in-memory object
}
\description{
Create an actual \link[raster:raster]{raster::raster} object by breaking the lazy
contract and demanding pixel values at a given resolution.
}
\details{
Control the dimensions and method for resampling with the 'dim' and
'resample' arguments.

If \code{dim} is non-integer it is rounded according to what raster does with the
dimensions.

When \code{native = TRUE} the \code{dim} argument is ignored, and no resampling is performed.
}
