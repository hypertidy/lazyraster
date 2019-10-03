lazycrop <- function(...) {
  .Defunct("lazyraster::crop")
}


#' Raster methods (S4) for lazyraster.
#'
#' These methods are generics from the raster package, extended to work for lazyrasters.
#'
#' For [crop()] this set an active window of data using the same [crop()]
#' function as the raster package. This is the data window that will be pulled
#' by conversion to an actual raster by [as_raster()].
#' @param x various (raster, extent)
#' @param y an object with extent (for crop)
#' @param ... arguments passed to underlying raster function
#' @seealso [raster::raster()], [raster::extent()], [raster::crop()]
#' @aliases extent crop
#' @rdname lazyraster-raster-S4
#' @name lazyraster-raster-S4
#' @importFrom methods setOldClass
#' @return [lazyraster()] and [crop()] return a lazyraster object, [extent()] returns
#' a regular raster extent
#' @examples
#' sstfile <- system.file("extdata/sst.tif", package = "vapour")
#' lr <- lazyraster(sstfile)
#' ## crop and stay as lazyraster
#' crop(lazyraster(sstfile), raster::extent(142, 143, -50, -45))
methods::setOldClass("lazyraster")
if (!isGeneric("extent")) {
  setGeneric("extent", function(x, ...)
    standardGeneric("extent"))
}
if (!isGeneric("raster")) {
  setGeneric("raster", function(x, ...)
    standardGeneric("raster"))
}
if (!isGeneric("crop")) {
  setGeneric("crop", function(x, ...)
    standardGeneric("crop"))
}

lazycrop_lazyraster <- function(x, y, ...) {
  ex <- extent(y)
  ## make sure our window extent reflects the parent
  rx <- lazy_to_raster(x)
  ex <- raster::extent(raster::crop(rx, ex, snap = "out"))

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
#' @rdname lazyraster-raster-S4
#' @importFrom raster crop
#' @export crop
setMethod("crop", "lazyraster", lazycrop_lazyraster)

#' @name lazyraster-raster-S4
#' @importFrom raster raster
raster_lazyraster <- function(x, ...) {
  as_raster(x, ...)
}
setMethod("raster", "lazyraster", raster_lazyraster)

#' @rdname lazyraster-raster-S4
#' @importFrom raster extent
extent_lazyraster <- function(x, ...) {
  ## TODO logic if x is a raster and ... is the r1, r2, c1, c2
  extent(to_xy_minmax(x), ...)
}
setMethod("extent", "lazyraster", extent_lazyraster)
