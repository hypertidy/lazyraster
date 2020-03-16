#' Lazy raster
#'
#' A lazyraster is a metadata-only shell around a raster file source. Only metadata is read
#' and and used for extent and resolution logic. A lazyraster may be cropped lazily
#' and if plotted or converted to in-memory raster only a reasonable level-of-detail is actually
#' used.
#'
#' See [crop()] for cropping - it works the same but returns a lazyraster,
#' and [as_raster()] for converting to an in-memory raster.
#' @section Warning:
#' If the inferred Y extents appear to be reversed (ymax > ymin) then they are
#' reversed, with a warning. This occurs for any GDAL data source that does not have
#' a geotransform and so is required for use with raster. This might not be the right interpretation,
#' geotransforms are very general and it might mean the data is meant to be oriented that way.
#' (I don't know why GDAL sets a positive Y pixel height as the default, it's a bit of a pain -
#' should the data be flipped, or should Y be interpreted negative - no way to know!).
#'
#' @param gdalsource a file name or other source string
#' @param band which band to use, defaults to all present
#' @param sds which subdataset to use, set to 1 if in doubt (see `vapour::vapour_sds_names`)
#' @param ... ignored for now
#' @return a lazyraster object, a simple shell around a GDAL raster source
#' @export
#' @examples
#' sstfile <- system.file("extdata/sst.tif", package = "vapour")
#' lazyraster(sstfile)
#' ## convert to raster (in memory, but not all of the source)
#' as_raster(lazyraster(sstfile))
#' ## crop and stay as lazyraster
#' crop(lazyraster(sstfile), raster::extent(142, 143, -50, -45))
#' ## crop and convert to raster
#' as_raster(crop(lazyraster(sstfile), raster::extent(142, 143, -50, -45)))
lazyraster <- function(gdalsource, band = NULL, sds = NULL, ...) {
  vars <- as.data.frame(vapour::vapour_sds_names(gdalsource), stringsAsFactors = FALSE)
  if (is.null(sds)) sds <- 1
  stopifnot(sds > 0)
  stopifnot(sds <= nrow(vars))
  ## vapour #34
  if (nrow(vars) > 1) gdalsource <- vars$subdataset[sds]

  info <- vapour::vapour_raster_info(gdalsource)
  if (is.null(band)) band <- seq_len(info$bands)
  if (all(band < 1)) stop("band must be 1 or greater")
  if (all(band > info$bands)) stop(sprintf("band greater than total number of bands (%i)", info$bands))
  raster <- list(band = band)

  if (info$geotransform[6L] > 0) {
     mess <- "ymin is greater than ymax, switching"
     if (!isTRUE(all.equal(info$geotransform[4L], 0))) paste0(mess, ", even though ymax not equal to 0")
     warning(mess)

     info$geotransform[6L] <- -info$geotransform[6L]
     info$geotransform[4L] <- info$dimXY[2L]
  }
  structure(list(source = gdalsource,
                 info = info,
                 window = list(window = NULL, windowextent = NULL),
                 raster = raster), class = "lazyraster")
}


#' Convert to in-memory raster
#'
#' Create an actual [raster::raster] object by breaking the lazy
#' contract and demanding pixel values at a given resolution.
#'
#' Control the dimensions and method for resampling with the 'dim' and
#' 'resample' arguments.
#'
#' If `dim` is non-integer it is rounded according to what raster does with the
#' dimensions.
#'
#' When `native = TRUE` the `dim` argument is ignored, and no resampling is performed.
#' @param x a [lazyraster]
#' @param dim dimensions, pixel size in rows and columns
#' @param resample resampling method, see [vapour::vapour_read_raster]
#' @param native return raster at native resolution, default is `FALSE`
#' @return a regular raster 'BasicRaster' in-memory object
#' @name as_raster
#' @export
as_raster <- function(x, dim = NULL, resample = "NearestNeighbour", native = FALSE) {
  UseMethod("as_raster")
}
#' @export
as_raster.lazyraster <- function(x, dim = NULL, resample = "NearestNeighbour", native = FALSE) {
  pull_lazyraster(x, pulldim = dim, resample = resample, native = native)
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





to_xy_minmax <- function(x) {
  xmin <- x$info$geotransform[1L]
  xmax <- xmin + x$info$dimXY[1L] * x$info$geotransform[2L]
  ymax <- x$info$geotransform[4L]
  ymin <- ymax + x$info$dimXY[2L] * x$info$geotransform[6L]


  c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
#' @importFrom grDevices dev.size
pull_lazyraster <- function(x, pulldim = NULL, resample = "NearestNeighbour", native = FALSE) {
  ## TODO: this needs to account for the "usr" bounds, the current
  ## bounds that will be plotted to
  if (native) {
    # fix https://github.com/hypertidy/lazyraster/issues/9
    if (is.null(x$window$window)) {
      dd <- x$info$dimXY
    } else {
      dd <- c(diff(x$window$window[1:2]) + 1,
            diff(x$window$window[3:4]) + 1)
    }
    if (!is.null(pulldim)) {
      warning("when 'native = TRUE' the 'dim' argument is ignored")
      message(sprintf("using native dimension %s", paste(dd, collapse = ", ")))
    }
   pulldim <- dd
  }
  if (is.null(pulldim)) pulldim <- grDevices::dev.size("px")

  if (length(pulldim) != 2L) {
    warning("dim will be replicated or shortened to length 2")
    pulldim <- rep(pulldim, length = 2L)
  }


  ## TODO raster from ssraster, override with dim
  r <- lazy_to_raster(x, dim = pulldim)
  ## GDAL rounds down, but raster has already rounded up (snap = out)
  ## so we should derive the pulldim from the raster
  pulldim <- dim(r)[c(2L, 1L)]  ##https://github.com/hypertidy/lazyraster/issues/13

  ## TODO pull window spec from info/plotdim, allow choice of resampling
  window_odim <- c(0, 0, x$info$dimXY[1], x$info$dimXY[2]) ## the global window
  if (!is.null(x$window$window)) {
    window <- x$window$window


    ## convert window to offset/dim
    window_odim <- c(window[c(1, 3)], (window[2] - window[1])+ 1, (window[4] - window[3]) + 1)
  }

  vals <- lapply(x$raster$band,
                function(bnd) {
                  vapour::vapour_read_raster(x$source,
                                             band = bnd,
                                             window = c(window_odim, pulldim[1], pulldim[2]),
                            resample = resample, set_na = TRUE)[[1L]]})  ## no multibands in vapour yet

  ## TODO clamp values to info$minmax - no longer needed with vapour set_na
  #vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA

  out <-
  if (length(vals) > 1L) {
    raster::setValues(raster::brick(replicate(length(vals), r, simplify = FALSE)),
                      do.call(cbind, vals))
  } else {
   raster::setValues(r, unlist(vals))
  }
  out
}

