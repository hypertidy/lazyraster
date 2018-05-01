#' @examples
lazyraster <- function(gdalsource) {
  structure(list(source = gdalsource,
                 info = vapour::raster_info(gdalsource)), class = "lazyraster")
}
lazy_to_raster <- function(x, dim = NULL) {
  ## assume lazyraster
  if (is.null(dim)) dim <- x$info$dimXY
  proj <- x$info$projection
  if (! (grepl("\\+proj", proj) && grepl("^\\+init", proj))) proj <- NA_character_
  raster::raster(raster::extent(to_xy_minmax(x)), nrows = dim[2], ncols = dim[1], crs = proj)
}
print.lazyraster <- function(x, ...) {
  print(summary(x))
}
summary.lazyraster <- function(x, ...) {
  ex <- to_xy_minmax(x)

  structure(list(classname = "LazyRaster",
                 dimensions = x$info$dimXY,
                 resolution = abs(x$info$geotransform[c(2, 6)]),
                 extent = ex,
                 crs = x$info$projection,
                 source = x$source,
                 values = x$info$minmax), class = "summary_lazyraster")
}
print.summary_lazyraster <- function(x, ...) {
  cat(sprintf("class      : %s\n", x$classname))
  cat(sprintf("dimensions : %s (nrow, ncol)\n", paste(x$dimension, collapse = ",")))
  cat(sprintf("resolution : %s (x, y)\n", paste(x$resolution, collapse = ",")))
  cat(sprintf("extent     : %s (xmin, xmax, ymin, ymax)\n", paste(x$extent, collapse = ",")))
  cat(sprintf("crs        : %s\n", x$crs))
  cat(sprintf("values     : %s (min, max)\n", paste(format(x$values, nsmall = 4), collapse = ",")))
  invisible(NULL)
}
#' @importFrom raster plot
plot.lazyraster <- function(x, ...) {
  raster::plot(as_raster(x), ...)
}
raster.lazyraster <- function(x, ...) {
  as_raster(x, ...)
}
as_raster <- function(x, dim = NULL, resample = "NearestNeighbour") {
  pull_lazyraster(x, pulldim = dim, resample = resample)
}


to_xy_minmax <- function(x) {
  xmin <- x$info$geotransform[1]
  xmax <- xmin + x$info$dimXY[1] * x$info$geotransform[2]
  ymax <- x$info$geotransform[4]
  ymin <- ymax + x$info$dimXY[2] * x$info$geotransform[6]
  c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
pull_lazyraster <- function(x, pulldim = NULL, resample = "NearestNeighbour") {
  ## TODO: this needs to account for the "usr" bounds, the current
  ## bounds that will be plotted to
  if (is.null(pulldim)) pulldim <- dev.size("px")
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

