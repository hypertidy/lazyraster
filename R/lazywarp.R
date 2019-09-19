extdim_to_geotransform <- function(ext, dim) {
  c(xmin = raster::xmin(ext),
    xres = (raster::xmax(ext) - raster::xmin(ext))/ dim[2],
    xsh = 0,
    ymax = raster::ymax(ext),
    ysh = 0,
    yres = -(raster::ymax(ext) - raster::ymin(ext))/ dim[1])
}



#' Simple raster warping in-memory
#'
#' Warp a GDAL data source to in in-memory raster target
#'
#' Currently only works as if 64-bit real band values, but GDAL's RasterIO
#' does sort out conversions. We set a missing value as if it's very low, but
#' it might be zero (as in the example), so WIP.
#' @param gdalsource a file name or other source string
#' @param target raster target object
#' @param a_srs PROJ4 string to apply to gdalsource in case it is wrong or missing
#' @noRd
#' @examples
#' laea_srs <- "+proj=laea +lon_0=147 +lat_0=-52 +datum=WGS84"
#' ex <- c(-1e6, 1e6, -1e6, 1e6)
#' library(raster)
#' ras <- raster(extent(ex), crs = laea_srs, nrows= 100, ncols = 100)
#' f <- system.file("extdata/sst.tif", package = "vapour", mustWork = TRUE)
#' r <- projectRaster(raster(f), ras)
#' r2 <- vapour:::lazywarp(f, ras)
#' r2[r2 < 1] <- NA
#' plot(r2)
lazywarp <- function(gdalsource, target, a_srs = "") {

  ## FIXME:: needs an exported function to do this part in vapour
  vals <- vapour:::warp_memory_cpp(gdalsource, a_srs,
                                   target_WKT = proj_to_wkt_cpp(raster::projection(target)),
                                   extdim_to_geotransform(extent(target), dim(target)[1:2]),
                                   dim(target)[1:2])
  ##print(target)
  ## TODO: deal with missing value
  vals[vals < -3.0000e+38 ] <- NA
  setValues(target, vals)
}

