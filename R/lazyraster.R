#' Lazy raster
#'
#' Get a quick-view of a raster source, there are several levels of interrogation,
#' decimated read, optional crop, and control of level-of-detail. Each of these
#' is described further below.
#'
#' First is a no-data view, we can see the extent, dimensions, projection, and
#' other details; second is a *decimated* view, we read only enough data to make
#' a picture of the entire raster; third, we may first spatially crop the raster
#' (nothing is done, only our intention to crop is recorded), if we plot or
#' reques the data we get the decimated view of this smaller region; fourth,
#' specify the actual dimension of the output. We can control exactly what we
#' get in the output, in terms of spatial extent and number of pixel values.
#'
#' A lazyraster is a metadata-only shell around a *GDAL raster* source. Only
#' metadata is read and used for extent and dimension logic. A lazyraster may be
#' cropped lazily and if plotted or converted to in-memory raster only a
#' reasonable level-of-detail is actually used by default.
#'
#' See [crop()] for cropping - it works the same but returns a lazyraster, and
#' [as_raster()] for converting to an in-memory raster.
#' @section Warning: If the inferred Y extents appear to be reversed (ymax >
#'   ymin) then they are reversed, with a warning. This occurs for any GDAL data
#'   source that does not have a geotransform and so is required for use with
#'   raster. This might not be the right interpretation, geotransforms are very
#'   general and it might mean the data is meant to be oriented that way. (I
#'   don't know why GDAL sets a positive Y pixel height as the default, it's a
#'   bit of a pain - should the data be flipped, or should Y be interpreted
#'   negative - no way to know!).
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
#'
#' @param x a [lazyraster]
#' @param dim dimensions, pixel size in rows and columns
#' @param resample resampling method, see [vapour::vapour_read_raster]
#' @param native return raster at native resolution, default is `FALSE`
#' @param band set to 1-based band numbers to return (independently of those set with [lazyraster()])
#' @return a regular raster 'BasicRaster' in-memory object
#' @name as_raster
#' @export
as_raster <- function(x, dim = NULL, resample = "NearestNeighbour", native = FALSE, band = 1L) {
  UseMethod("as_raster")
}
#' @export
as_raster.lazyraster <- function(x, dim = NULL, resample = "NearestNeighbour", native = FALSE, band = 1L) {
  pull_lazyraster(x, pulldim = dim, resample = resample, native = native, band = band)
}

raster_info_to_raster <- function(x) {
  raster::raster(raster::extent(x$extent), nrows = x$dimXY[2L], ncols= x$dimXY[1L], crs = x$projection)
}

lazy_to_raster <- function(x, dim = NULL) {

  rr <- raster_info_to_raster(x$info)

  ext <- lazy_to_extent(x)
  if (!is.null(x$window$window)) {
    window <- x$window$window
    ext <- x$window$windowextent

    if (is.null(dim)) dim <- c(diff(window[1:2]), diff(window[3:4])) + 1L
    #print(dim)
  }
  if (is.null(dim)) dim <- x$info$dimXY

  if (inherits(dim, "BasicRaster")) {
    template <- dim
    proj <- raster::projection(template)
    dim <- dim(template)
    ext <- raster::extent(template)
  } else {
    proj <- x$info$projection
  }
  #proj <- NA_character_
  raster::raster(raster::extent(ext), nrows = dim[2], ncols = dim[1], crs = proj)
}

lazy_to_extent <- function(x) {
  xmin <- x$info$geotransform[1L]
  xmax <- xmin + x$info$dimXY[1L] * x$info$geotransform[2L]
  ymax <- x$info$geotransform[4L]
  ymin <- ymax + x$info$dimXY[2L] * x$info$geotransform[6L]
  c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
#' @importFrom grDevices dev.size
pull_lazyraster <- function(x, pulldim = NULL, resample = "NearestNeighbour", native = FALSE, band = 1L) {
  ## TODO: this needs to account for the "usr" bounds, the current
  ## bounds that will be plotted to
  if (native) {
    if (!is.null(pulldim)) {
      warning("when 'native = TRUE' the 'dim' argument is ignored")
    }
   pulldim <- NULL
  } else {
    if (is.null(pulldim)) pulldim <- grDevices::dev.size("px")
  }
  if (!inherits(pulldim, "BasicRaster") && !is.null(pulldim) && length(pulldim) != 2L) {
    warning("dim will be replicated or shortened to length 2")
    pulldim <- rep(pulldim, length = 2L)
  }
#print(pulldim)
## TODO raster from ssraster, override with dim
  r <- lazy_to_raster(x, dim = pulldim)
 #print(dim(r)); print(res(r))
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

  bands <- x$raster$band
  if (!is.null(band)) bands <- band
  if (is.null(band)) bands <- seq_len(x$info$bands)

  ext <- raster::extent(r) # x$window$windowextent
  if (is.null(ext)) ext <- x$info$extent
  proj <- comment(raster::crs(r))
  if (is.null(proj)) proj <- raster::crs(r)@projargs
  if (packageVersion("vapour") <= "0.8.0") {
    vals <- vapour::vapour_warp_raster(x$source, band = bands,
                                       extent = ext, dimension = pulldim, wkt = proj, band_output_type = "Float64")
  } else {
    vals <- vapour::vapour_warp_raster(x$source, band = bands,
                                     extent = ext, dimension = pulldim, projection = proj, band_output_type = "Float64")
  }
  ## TODO clamp values to info$minmax - no longer needed with vapour set_na
  #vals[vals < x$info$minmax[1] | vals > x$info$minmax[2]] <- NA
  for (i in seq_along(vals)) {
    ## set_na not working
    vals[[i]][vals[[i]] <= x$info$nodata_value] <- NA
  }
  op <- options(warn = -1)
  on.exit(options(op), add = TRUE)
  out <-
  if (length(vals) > 1L) {
    raster::setValues(raster::brick(replicate(length(vals), r, simplify = FALSE)),
                      do.call(cbind, vals))
  } else {
   raster::setValues(r, vals[[1]])
  }
  out
}

