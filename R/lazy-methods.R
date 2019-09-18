#' Lazy raster S3 methods
#'
#' Print and format for lazyraster.
#' @name lazyraster-methods
#' @export
#' @usage \method{print}{lazyraster}(x, ...)
#' @examples
#' fl <- system.file("images/ga_srtm.png", package = "lazyraster")
#' print(lazyraster(fl))
#' plot(lazyraster(fl))
print.lazyraster <- function(x, ...) {
  junk <- lapply(format(x), cat)
  invisible(x)
}

#' @name lazyraster-methods
#' @usage \method{format}{lazyraster}(x, ...)
#' @export
#' @export
format.lazyraster <- function(x, ...) {
  object <- x
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

#' Lazy raster S3 plot method
#'
#' Plot for lazyraster, data pulled on-demand at a reasonable level-of-detail.
#'
#' Data is pulled from the GDAL source at a resolution suited for
#' the currently open graphics device.
#' @param x a [lazyraster]
#' @param y ignored
#' @param ... passed to [raster::plot]
#' @importFrom raster plot
#' @importFrom graphics plot
#' @export plot
#' @rawNamespace S3method(plot,lazyraster)
#' @usage \method{plot}{lazyraster}(x, y, ...)
#' @name lazyraster-methods
#' @export
plot.lazyraster <- function(x, y, ...) {
  raster::plot(as_raster(x), ...)
}
