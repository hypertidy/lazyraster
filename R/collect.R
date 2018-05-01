# library(raadtools)
# r <- readtopo("etopo1")
# system.time(collect(r, nrows = 768, ncols = 768))
# f <- raadfiles:::get_raw_raad_filenames()
# f <- mutate(f, fullname = file.path(root, file))
# r <- raster(f$fullname[25])
# r2 <- collect(r, nrows = 768, ncols = 768)


#if (!exists("collect") && is.function(collect)) {


#' Raster methods for collect
#' @param x raster object
#'
#' @param ... ignored
#' @param nrows number of rows in the output
#' @param ncols number of columns in the output
#' @param uselazy use lazy read via vapour package
#'
#' @export
#'
#' @name collect
#' @examples
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @name collect
#' @export
collect.BasicRaster <- function(x, ..., nrows = 512, ncols = nrows, uselazy = TRUE) {

  dm <- c(raster::nrow(x), raster::ncol(x))

  if (nrows != dm[1] || ncols != dm[2]) {
    message(sprintf("%s is %ix%i, collecting output of %ix%i \n %s",
                    deparse(substitute(x)), dm[1], dm[2], nrows, ncols,
                    "use 'nrows = , ncols = ' to customize output size"))
  }
  outdim <- c(nrows, ncols)
  outdim <- c(min(dm[1], nrows), min(dm[2], ncols))
  offs <- c(0, 0)
  if (raster::inMemory(x)) {
    warning(sprintf("%s is already in-memory ... any resampling will be approximate", deparse(substitute(x))))
    rs <- raster::aggregate(x, fact = dm/outdim)
  } else {
    if (uselazy) {
        rs <- as_raster(lazyraster(x@file@name), dim = c(ncols, nrows))
    } else {
      ## use rgdal
   rs <- rgdal::readGDAL(x@file@name, offset = offs, region.dim = dm, output.dim = outdim, silent = TRUE)
    rs <- raster::raster(rs)
    }
  }
  rs
}



