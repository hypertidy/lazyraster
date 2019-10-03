
#' Raster methods (S4) for lazyraster.
#'
#' These methods are generics from the raster package, extended to work for lazyrasters.
#'
#' @param x various (raster, extent)
#' @param ... arguments passed to underlying raster function
#' @seealso [raster::raster()], [raster::extent()]
#' @aliases extent crop
#' @rdname lazyraster-raster-S4
#' @name lazyraster-raster-S4
setOldClass("lazyraster")
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
#' @name lazyraster-raster-S4
#' @importFrom raster raster
raster_lazyraster <- function(x, ...) {
  as_raster(x, ...)
}
setMethod("raster", "lazyraster", raster_lazyraster)

#' @importFrom raster extent
extent_lazyraster <- function(x, ...) {
  ## TODO logic if x is a raster and ... is the r1, r2, c1, c2
  extent(to_xy_minmax(x), ...)
}
setMethod("extent", "lazyraster", extent_lazyraster)
