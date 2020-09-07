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
  r0 <- raster::crop(rx, ex, snap = "out")
  ex <- raster::extent(r0)
  col1 <- raster::colFromX(rx, raster::xFromCell(r0, 1))
  col2 <- raster::colFromX(rx, raster::xFromCell(r0, raster::ncell(r0)))

  row1 <- raster::rowFromY(rx, raster::yFromCell(r0, 1))
  row2 <- raster::rowFromY(rx, raster::yFromCell(r0, raster::ncell(r0)))

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
NULL
setMethod("raster", "lazyraster", function(x, ...) { as_raster(x, ...)})

#' @rdname lazyraster-raster-S4
#' @importFrom raster extent
NULL
setMethod("extent", "lazyraster", function(x, ...) { extent(lazy_to_extent(x), ...)})
