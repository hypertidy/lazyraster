#' Lazy raster S3 methods
#'
#' Print and format for lazyraster.
#' @name lazyraster-methods
#' @export
#' @usage \method{print}{lazyraster}(x, ...)
#' @examples
#' fl <- system.file("images/ga_srtm.png", package = "lazyraster")
#' print(lazyraster(fl))
#' ## won't work with dumb images with gdalwarp, need rethink
#' #plot(lazyraster(fl))
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
  ex <- lazy_to_extent(object)
  windowdescription <- if (is.null(object$window$windowextent)) "<whole extent>" else paste(format(object$window$windowextent, nsmall = 4), collapse = ", ")
  windowindex <- if (is.null(object$window$window)) "<->" else paste(as.integer(object$window$window), collapse = ", ")
  windowdim <-   if (is.null(object$window$window)) "(full)" else paste(as.integer(c(diff(object$window$window[1:2]),
                                                                                     diff(object$window$window[3:4])) + 1L), collapse = ", ")

  projection <- object$info$projstring
  if (is.null(projection) || nchar(projection) < 1 || is.na(projection) ) {
    projection <- object$info$projection
  }
  x <- list(   classname = "LazyRaster",
               dimensions = object$info$dimXY,
               resolution = abs(object$info$geotransform[c(2, 6)]),
               extent = ex,
               crs = projection,
               source = object$source,
               values = object$info$minmax,
               windowdescription = windowdescription,
               window = windowindex)


  fmt <- list(sprintf("class            : %s\n", x$classname),
              sprintf("dimensions       : %s (nrow, ncol)\n", paste(x$dimension[2:1], collapse = ", ")),
              sprintf("resolution       : %s (x, y)\n", paste(format(x$resolution, nsmall = 4), collapse = ", ")),
              sprintf("extent           : %s (xmin, xmax, ymin, ymax)\n", paste(format(x$extent, nsmall = 4), collapse = ", ")),
              sprintf("crs              : %s\n", x$crs),
              sprintf("values           : %s (min, max - range from entire extent)\n", paste(format(x$values, nsmall = 4), collapse = ", ")),
              sprintf("window extent    : %s\n", windowdescription),
              sprintf("window index     : %s\n", windowindex),
              sprintf("window dimension : %s (ncol, nrow)\n", windowdim))
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
#' @method plot lazyraster
#' @name lazyraster-methods
#' @aliases plot
#' @rdname lazyraster-methods
#' @export
plot.lazyraster <- function(x, y,  ...) {
  raster::plot(as_raster(x),  ...)
}
