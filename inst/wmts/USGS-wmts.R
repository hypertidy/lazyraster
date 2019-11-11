
## lazyraster approach, but better done with slippymath+raster
library(lazyraster)
u <- "WMTS:https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/WMTS/1.0.0/WMTSCapabilities.xml,layer=USGSTopo,tilematrixset=default028mm,zoom_level={zoom_level}"

centre <- c(-9004415, 3806965)
radius <- 4000

x <-  slippymath::merc_to_lonlat(matrix(centre, ncol = 2L))
buffer <- radius
zoom <- NULL
max_tiles <- 16
bbox_pair <- ceramic:::spatial_bbox(x, buffer)
my_bbox <- bbox_pair$tile_bbox
bb_points <- bbox_pair$user_points
## all we need is zoom
tile_grid <- slippymath::bbox_to_tile_grid(my_bbox, max_tiles = max_tiles,
                                           zoom = zoom)
zoom_level <- tile_grid$zoom + 1
lr <- lazyraster(glue::glue(u))

## avoid extra calls outside
l <- lapply(1:3, function(i) {out <- lr; out$raster$band <- i; out})
#l <- purrr::map(1:3, ~lazyraster(glue::glue(u), band = .x))


plot_get_wmts <- function(lx, ext = NULL, dim = NULL, ..., native = FALSE, plot = TRUE, alpha = FALSE) {
  if (alpha) idx <- 1:4 else idx <- 1:3
  if (is.null(ext)) ext <- raster::extent(lx[[1]])
  rgb <- raster::brick(lapply(idx, function(xx) as_raster(crop(lx[[xx]], ext), dim = dim, native = native)))
  if (plot) raster::plotRGB(rgb)
  invisible(rgb)
}

ex <- raster::extent(centre[c(1, 1, 2, 2)] + c(-1, 1, -1, 1) * buffer)
rgb_map <- plot_get_wmts(l, ex, native = TRUE)

library(ggplot2)

tab <- tibble::as_tibble(as.data.frame(rgb_map, xy = TRUE))
names(tab) <- c("x", "y", "red", "green", "blue")
#tab <- dplyr::filter(tab, !is.na(red))

## ... when we have missing values, we should drop them or rgb() will error
tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
ggplot(tab, aes(x, y, fill = hex)) +
  geom_raster() +
  coord_equal() +
  scale_fill_identity()
