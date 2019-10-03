lazycrop <- function(...) {
  .Defunct("lazyraster::crop")
}

lazycrop.lazyraster <- function(x, y, ..., verbose = FALSE) {
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
  if (verbose) print(x$window)
  x
}

#' Lazy crop
#'
#' Set an active window of data using the same [crop()] function as the
#' raster package.
#'
#' This is the data window that will be pulled by conversion
#' to an actual raster by [as_raster()].
#' @inheritParams raster::crop
#' @name crop
#' @export crop
#' @seealso [lazyraster()], and [raster::crop()]
#' @importFrom raster crop
#' @examples
#' sstfile <- system.file("extdata/sst.tif", package = "vapour")
#' lr <- lazyraster(sstfile)
#' ## crop and stay as lazyraster
#' crop(lazyraster(sstfile), raster::extent(142, 143, -50, -45))
NULL

setMethod("crop", "lazyraster", lazycrop.lazyraster)
