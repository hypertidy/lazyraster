# library(raadtools)
# r <- readtopo("etopo1")
# system.time(collect(r, nrows = 768, ncols = 768))
# f <- raadfiles:::get_raw_raad_filenames()
# f <- mutate(f, fullname = file.path(root, file))
# r <- raster(f$fullname[25])
# r2 <- collect(r, nrows = 768, ncols = 768)


#if (!exists("collect") && is.function(collect)) {


#' Raster methods for collect
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
collect.BasicRaster <- function(x, ..., nrows = 512, ncols = nrows) {

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
    rs <- rgdal::readGDAL(x@file@name, offset = offs, region.dim = dm, output.dim = outdim, silent = TRUE)
    rs <- raster::raster(rs)
  }
  rs
}



