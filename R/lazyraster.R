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
  col1 <- raster::colFromX(rx, ex@xmin)
  col2 <- raster::colFromX(rx, ex@xmax)
  row1 <- raster::rowFromY(rx, ex@ymax)
  row2 <- raster::rowFromY(rx, ex@ymin)
  if (is.na(row1)) row1 <- 1
  if (is.na(row2)) row2 <- rx@nrows

  x$window <- list(window = c(col1 - 1, col2 - 1, row1 - 1, row2 - 1),
                   windowextent = c(ex@xmin, ex@xmax, ex@ymin, ex@ymax))
  x
}

#' Lazy raster S3 methods
#'
#' @param x a `lazyraster`
#' @param ... reserved
#' @name lazyraster-methods
#' @rawNamespace S3method(print,lazyraster)
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
#' # importFrom raster crop
#' # export crop
#' # name lazyraster-methods
#' crop.lazyraster <- function(x, y, ...) {
#'   stop("cannot crop a lazyraster, use lazycrop")
#' }

#
# if (!isGeneric("crop")) {
#   setGeneric("crop", function(x, y, ...)
#     standardGeneric("crop"))
# }
#' @name lazyraster
setOldClass("lazyraster")
if (!isGeneric("extent")) {
  setGeneric("extent", function(x, y, ...)
    standardGeneric("extent"))
}

#setMethod("crop", "lazyraster", crop.lazyraster)
setMethod("extent", "lazyraster", extent.lazyraster)

#' @name lazyraster-methods
#' @export
format.lazyraster <- function(object, ...) {
  ex <- to_xy_minmax(object)
  windowdescription <- if (is.null(object$window$windowextent)) "<whole extent>" else paste(format(object$window$windowextent, nsmall = 4), collapse = ", ")
  windowindex <- if (is.null(object$window$window)) "<->" else paste(as.integer(object$window$window), collapse = ", ")

  x <- list(   classname = "LazyRaster",
                 dimensions = object$info$dimXY,
                 resolution = abs(object$info$geotransform[c(2, 6)]),
                 extent = ex,
                 crs = "<placeholder>", ##object$info$projection,
                 source = object$source,
                 values = object$info$minmax,
               windowdescription = windowdescription,
                 window = windowindex)


  fmt <- list(sprintf("class         : %s\n", x$classname),
              sprintf("dimensions    : %s (nrow, ncol)\n", paste(x$dimension[2:1], collapse = ", ")),
              sprintf("resolution    : %s (x, y)\n", paste(format(x$resolution, nsmall = 4), collapse = ", ")),
              sprintf("extent        : %s (xmin, xmax, ymin, ymax)\n", paste(format(x$extent, nsmall = 4), collapse = ", ")),
              sprintf("crs           : %s\n", x$crs),
              sprintf("values        : %s (min, max - range from entire extent)\n", paste(format(x$values, nsmall = 4), collapse = ", ")),
              sprintf("window extent : %s\n", windowdescription), sprintf("window index  : %s\n", windowindex))
  fmt
}

#' @name lazyraster-methods
#' @export
print.lazyraster <- function(x, ...) {
  junk <- lapply(format(x), cat)
  invisible(x)
}
#' @importFrom raster plot
#' @name lazyraster-methods
#' @importFrom graphics plot
#' @export
#' @rawNamespace S3method(plot,lazyraster)
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
  window_odim <- c(0, 0, x$info$dimXY[1], x$info$dimXY[2]) ## the global window
  if (!is.null(x$window$window)) {
    window <- x$window$window


    ## convert window to offset/dim
    window_odim <- c(window[c(1, 3)], (window[2] - window[1])+ 1, (window[4] - window[3]) + 1)
  }

  vals <- vapour::raster_io(x$source, window = c(window_odim, pulldim[1], pulldim[2]),
                            resample = resample)
  ## TODO clamp values to info$minmax - set NA
  vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA

  raster::setValues(r, vals)
}

