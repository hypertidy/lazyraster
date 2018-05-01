#' Lazy raster
#'
#' Read metadata only from a raster source, for later use with plotting and
#' conversion to raster.
#' @param gdalsource a file name or other source string
#' @param x a lazyraster
#' @param dim dimensions of data to return
#' @param resample resample method to use, see `vapour::raster_io`
#' @export
#' @examples
#' sstfile <- system.file("extdata/sst.tif", package = "vapour")
#' lazyraster(sstfile)
#' as_raster(lazyraster(sstfile))
lazyraster <- function(gdalsource) {
  structure(list(source = gdalsource,
                 info = vapour::raster_info(gdalsource)), class = "lazyraster")
}
#' @name lazyraster
#' @export
as_raster <- function(x, ...) {
  UseMethod("as_raster")
}
#' @name lazyraster
#' @export
as_raster <- function(x, dim = NULL, resample = "NearestNeighbour") {
  pull_lazyraster(x, pulldim = dim, resample = resample)
}


lazy_to_raster <- function(x, dim = NULL) {
  ## assume lazyraster
  if (is.null(dim)) dim <- x$info$dimXY
  proj <- x$info$projection
  #if (!substr(proj, 1, 1) == "+") {
    proj <- NA_character_
   #warning("projstring doesn't look like a PROJ string")
  #}
  #if (! (grepl("\\+proj", proj) && grepl("^\\+init", proj))) proj <- NA_character_
  raster::raster(raster::extent(to_xy_minmax(x)), nrows = dim[2], ncols = dim[1], crs = proj)
}
#' Lazy raster S3 methods
#'
#' @param x a `lazyraster`
#' @param ... reserved
#' @name lazyraster-methods
#' @export
print.lazyraster <- function(x, ...) {
  print(summary(x))
}
#' @name lazyraster-methods
#' @export
summary.lazyraster <- function(object, ...) {
  ex <- to_xy_minmax(object)

  structure(list(classname = "LazyRaster",
                 dimensions = object$info$dimXY,
                 resolution = abs(object$info$geotransform[c(2, 6)]),
                 extent = ex,
                 crs = object$info$projection,
                 source = object$source,
                 values = object$info$minmax), class = "summary_lazyraster")
}

#' @name lazyraster-methods
#' @export
print.summary_lazyraster <- function(x, ...) {
  cat(sprintf("class      : %s\n", x$classname))
  cat(sprintf("dimensions : %s (nrow, ncol)\n", paste(x$dimension, collapse = ",")))
  cat(sprintf("resolution : %s (x, y)\n", paste(format(x$resolution, nsmall = 4), collapse = ",")))
  cat(sprintf("extent     : %s (xmin, xmax, ymin, ymax)\n", paste(format(x$extent, nsmall = 4), collapse = ",")))
  cat(sprintf("crs        : %s\n", x$crs))
  cat(sprintf("values     : %s (min, max)\n", paste(format(x$values, nsmall = 4), collapse = ",")))
  invisible(NULL)
}
#' @importFrom raster plot
#' @name lazyraster-methods
#' @export
plot.lazyraster <- function(x, ...) {
  raster::plot(as_raster(x), ...)
}
#' @name lazyraster-methods
#' @export
raster.lazyraster <- function(x, ...) {
  as_raster(x, ...)
}

to_xy_minmax <- function(x) {
  xmin <- x$info$geotransform[1]
  xmax <- xmin + x$info$dimXY[1] * x$info$geotransform[2]
  ymax <- x$info$geotransform[4]
  ymin <- ymax + x$info$dimXY[2] * x$info$geotransform[6]
  c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
#' @importFrom grDevices dev.size
pull_lazyraster <- function(x, pulldim = NULL, resample = "NearestNeighbour") {
  ## TODO: this needs to account for the "usr" bounds, the current
  ## bounds that will be plotted to
  if (is.null(pulldim)) pulldim <- grDevices::dev.size("px")

  if (length(pulldim) != 2L) {
    warning("dim will be replicated or shortened to length 2")
    pulldim <- rep(pulldim, length = 2L)
  }
  ## TODO raster from ssraster, override with dim
  r <- lazy_to_raster(x, dim = pulldim)
  ## TODO pull window spec from info/plotdim, allow choice of resampling
  vals <- vapour::raster_io(x$source, window = c(0, 0, x$info$dimXY[1], x$info$dimXY[2],
                                         pulldim[1], pulldim[2]),
                    resample = resample)
  ## TODO clamp values to info$minmax - set NA
  vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA
  raster::setValues(r, vals)
}

