#' Lazy raster
#'
#' Read metadata only from a raster source, for later use with plotting and
#' conversion to raster.
#' @param gdalsource a file name or other source string
#' @param x a lazyraster
#' @param dim dimensions of data to return
#' @param resample resample method to use, see `vapour::raster_io`
#' @param sds which subdataset to use, set to 1 if in doubt (see `vapour::vapour_sds_names`)
#' @export
#' @examples
#' sstfile <- system.file("extdata/sst.tif", package = "vapour")
#' lazyraster(sstfile)
#' as_raster(lazyraster(sstfile))
#' as_raster(lazycrop(lazyraster(sstfile), raster::extent(142, 143, -50, -45)))
lazyraster <- function(gdalsource, sds = NULL) {
  vars <- as.data.frame(vapour::vapour_sds_names(gdalsource), stringsAsFactors = FALSE)
  if (is.null(sds)) sds <- 1
  stopifnot(sds > 0)
  stopifnot(sds <= nrow(vars))
  gdalsource <- vars$subdataset[sds]
  structure(list(source = gdalsource,
                 info = vapour::vapour_raster_info(gdalsource),
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

  vals <- vapour::vapour_read_raster(x$source, window = c(window_odim, pulldim[1], pulldim[2]),
                            resample = resample)
  ## TODO clamp values to info$minmax - set NA
  vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA

  raster::setValues(r, vals)
}

