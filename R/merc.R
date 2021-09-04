radians <- function(x) x * pi/180
m_clamp <- function(x) {
  MAXEXTENT <- 20037508
  if (x > MAXEXTENT) x <- MAXEXTENT
  if (x < - MAXEXTENT) x <- -MAXEXTENT
  x
}
#' Mercator extent
#'
#' Create an extent in Mercator projection from a longitude, latitude, and a width,height.
#' @param x longitude
#' @param y latitude
#' @param wh width and height, in metres - if only one vaue provided it is also used for height
#'
#' @return four values, xmin, xmax, ymin, ymax in global Mercator
#' @export
#'
#' @examples
#' merc()  ## a parochial default
#' library(lazyraster)
#' vearth <- '<GDAL_WMS> <Service name="VirtualEarth">
#' <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl></Service>
#' <MaxConnections>4</MaxConnections>    <Cache/>    </GDAL_WMS>'
#' lr <- lazyraster(vearth)
#' \dontrun{\donttest{
#' raster::plotRGB(as_raster(crop(lr, merc(-90, 52, 256e4)), band = 1:3))
#' }}
merc <- function(x = 146.7, y = -42, wh = 256e3) {
  wh <- rep(wh, length.out = 2L)
  A <- 6378137
  ll <- do.call(cbind, xy.coords(x, y))[1,,  drop = TRUE]
  xy <- cbind(A * radians(ll[1]), A * log(tan((pi * 0.25) +
                                                (0.5 * radians(ll[2])))))
  xy[1] <- m_clamp(xy[1])
  xy[2] <- m_clamp(xy[2])
  c(xy[1] - wh[1], xy[1] + wh[1], xy[2] - wh[2], xy[2] + wh[2])
}
