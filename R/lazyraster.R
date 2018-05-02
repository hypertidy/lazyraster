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
#' as_raster(lazycrop(lazyraster(sstfile), extent(142, 143, -50, -45)))
lazyraster <- function(gdalsource) {
  structure(list(source = gdalsource,
                 info = vapour::raster_info(gdalsource),
                 window = list(window = NULL, windowextent = NULL)), class = "lazyraster")
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

lazy_window <- function(x) {

}
lazy_to_raster <- function(x, dim = NULL) {
  ## assume lazyraster
  if (is.null(dim)) dim <- x$info$dimXY
  ext <- to_xy_minmax(x)
  if (!is.null(x$window$window)) {
    window <- x$window$window
    if (is.null(dim)) dim <- c(window[2] - window[1], window[4] - window[3]) + 1
    ext <- x$window$windowextent
  }
  proj <- x$info$projection
  #if (!substr(proj, 1, 1) == "+") {
  proj <- NA_character_
  #warning("projstring doesn't look like a PROJ string")
  #}
  #if (! (grepl("\\+proj", proj) && grepl("^\\+init", proj))) proj <- NA_character_

  raster::raster(raster::extent(ext), nrows = dim[2], ncols = dim[1], crs = proj)
}

#' Lazy crop
#'
#' Set an active window of data that will be pulled by conversion
#' to an actual raster.
#' @inheritParams raster::crop
#' @export
#' @name lazycrop
lazycrop <- function(x, y, ...) {
  UseMethod("lazycrop")
}
lazycrop.BasicRaster <- function(x, y, ...) {
  ## hmm, what if there's no file, we end up with
  ## carrying around the original data?
}
#' @export
#' @name lazycrop
lazycrop.lazyraster <- function(x, y, ...) {
  ex <- extent(y)
  ## make sure our window extent reflects the parent
  rx <- lazy_to_raster(x)
  ex <- extent(crop(rx, ex, snap = "out"))
  xr <- raster::xres(rx)
  yr <- raster::yres(rx)
  col1 <- colFromX(rx, ex@xmin)# + 0.5*xr)
  col2 <- colFromX(rx, ex@xmax)# - 0.5*xr)
  row1 <- rowFromY(rx, ex@ymax)# + 0.5*yr)
  row2 <- rowFromY(rx, ex@ymin)# - 0.5*yr)
  x$window <- list(window = c(col1 - 1, col2 - 1, row1 - 1, row2 - 1), windowextent = c(ex@xmin, ex@xmax, ex@ymin, ex@ymax))
  x
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
#' @importFrom raster extent
#' @export extent
#' @name lazyraster-methods
extent.lazyraster <- function(x, ...) {
  ## TODO logic if x is a raster and ... is the r1, r2, c1, c2
  extent(to_xy_minmax(x), ...)
}
#' @importFrom raster crop
#' @export crop
#' @name lazyraster-methods
crop.lazyraster <- function(x, y, ...) {
  stop("cannot crop a lazyraster, use lazycrop")
}
setOldClass("lazyraster")

if (!isGeneric("crop")) {
  setGeneric("crop", function(x, y, ...)
    standardGeneric("crop"))
}
if (!isGeneric("extent")) {
  setGeneric("extent", function(x, y, ...)
    standardGeneric("extent"))
}

setMethod("crop", "lazyraster", crop.lazyraster)
setMethod("extent", "lazyraster", extent.lazyraster)

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
                 values = object$info$minmax,
                 window = object$window), class = "summary_lazyraster")
}

#' @name lazyraster-methods
#' @export
print.summary_lazyraster <- function(x, ...) {
  cat(sprintf("class         : %s\n", x$classname))
  cat(sprintf("dimensions    : %s (nrow, ncol)\n", paste(x$dimension[2:1], collapse = ", ")))
  cat(sprintf("resolution    : %s (x, y)\n", paste(format(x$resolution, nsmall = 4), collapse = ", ")))
  cat(sprintf("extent        : %s (xmin, xmax, ymin, ymax)\n", paste(format(x$extent, nsmall = 4), collapse = ", ")))
  cat(sprintf("crs           : %s\n", x$crs))
  cat(sprintf("values        : %s (min, max - range from entire extent)\n", paste(format(x$values, nsmall = 4), collapse = ", ")))
  windowdescription <- if (is.null(x$window$windowextent)) "<whole extent>" else paste(format(x$window$windowextent, nsmall = 4), collapse = ", ")
  windowindex <- if (is.null(x$window$windowextent)) "<->" else paste(as.integer(x$window$window), collapse = ", ")

  cat(sprintf("window extent : %s\n", windowdescription))
  cat(sprintf("window index  : %s\n", windowindex))
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
  window <- c(0, 0, x$info$dimXY[1], x$info$dimXY[2]) ## the global window
  if (!is.null(x$window$window))  window <- x$window$window

  ## convert window to offset/dim
  window_odim <- c(window[c(1, 3)], (window[2] - window[1])+ 1, (window[4] - window[3]) + 1)
  print(window)
  print(window_odim)
  vals <- vapour::raster_io(x$source, window = c(window_odim, pulldim[1], pulldim[2]),
                            resample = resample)
  ## TODO clamp values to info$minmax - set NA
  vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA

  raster::setValues(r, vals)
}

