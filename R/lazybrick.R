#' Lazy brick
#'
#' Read a multi-band lazyraster source. This is aimed at RGB Byte type images.
#'
#' Bands are assumed to be 1, 2, 3 (Red, Green, Blue) and of type Byte (values between
#' 0-255). For use with [raster::plotRGB()]. The band/s can be re-specified using the `band`
#' argument, including order and number of them. There's nothing here to stop you creating the lazy brick
#' you want.
#' @inheritParams lazyraster
#'
#' @return a raster brick object
#' @noRd
#'
#' @examples
#' e <- raster::extent(153.00795, 153.04041, -27.49034, -27.45917)
#' qldimg_wms0 <- 'https://spatial-img.information.qld.gov.au/"
#' wms1 <- "arcgis/services/Basemaps/LatestSatelliteWOS_AllUsers/ImageServer/WMSServer?"
#' u <-   paste0(qldimg_wms, "service=WMS", "&version=1.3.0", "&request=GetMap",
#'               "&layers=LatestSatelliteWOS_AllUsers", "&styles=", "&crs=CRS%3A84")
lazy_rgb  <- function(gdalsource, band = c(1L, 2L, 3L), sds = NULL, ...) {
  lrs <- lapply(band, function(bnd) lazyraster(gdalsource, band = bnd, sds = sds))
 structure(lrs, class = "lazybrick")
}

print.lazybrick <- function(x, ...) {
  cat(sprintf("lazyrgb: %s bands", format(length(x))))
  print(x[[1]])
}

