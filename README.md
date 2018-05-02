
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazyraster

The goal of lazyraster is to get raster data on demand at the right
resolution. This means that you can define a graphics device and then
stream just the right amount of pixels to fill it from a GDAL data
source.

If you are interesed or have problems installing this package please
just let me know\! It’s still early days and very WIP.

There are functions `lazyraster` to act like the `raster::raster`
function and provide information but no data, and `lazycrop` to act like
`raster::crop` and then `as_raster` to actually break the lazy chain and
build an actual raster object. The size of the currently open (or
latent-default) device is used as a reasonable size for the output grid,
but can be controlled by argument `dim`.

When the data is read `lazyraster` can specify the exact dimensions of
the output raster, and by default a reasonable guess at the number of
pixels required to fill the current device is used.

A variety of resampling algorithms are available (nearest neighbour is
the default, [see this list for
more](https://github.com/hypertidy/vapour/blob/master/R/raster-input.R#L9))
and will be applied to reduce or increase the resolution.

## Limitations

We can’t utilize the RasterIO level-of-detail functionality for non-GDAL
sources.

We can only read the first band.

The only really useable output is a raster layer. You cannot yet specify
that return is at native resolution WIP.

We can’t control the details of the data type.

The projection string is not coming through properly.

The plot-size logic should work on the current “usr” world coordinates,
not the size of the device (if it’s different).

## GDAL

This uses a standard internal functionality of GDAL, the [RasterIO
function of the
GDALRasterBand](http://www.gdal.org/classGDALRasterBand.html#a30786c81246455321e96d73047b8edf1).
This is used in a lot of different software, and is obviously pretty
robust and well tested by the GDAL community, but I only really have
experience with one product (commercial, now defunct) that used it
extensively for live interactive visualization and data streaming. I
haven’t found any problems with it at all using it in R, but the suport
for it is very minimal. You can access it indirectly using
`rgdal::readGDAL` and its underlying functions, as the `raster` package
does.

## vapour

To make this work we use the GDAL package
[vapour](https://github.com/hypertidy/vapour). All of the ease-of-use
code is in this package, `vapour` is pointedly bare-bones and provides
very little interpretation of a data source because it is designed for
use in development.

## Example

Connect lazily to a GeoTIFF, see details of what’s there, crop to a
section and then read it in and plot.

This is not a huge file, but is easily accessible and demonstrates the
idea.

``` r
sstfile <- system.file("extdata/sst.tif", package = "vapour")
library(lazyraster)
lazy <- lazyraster(sstfile)
lazy ## stay lazy

## be only so lazy
as_raster(lazy, dim = c(12, 24))
#> class       : RasterLayer 
#> dimensions  : 24, 12, 288  (nrow, ncol, ncell)
#> resolution  : 0.8341667, 0.834213  (x, y)
#> extent      : 140, 150.01, -60.01833, -39.99722  (xmin, xmax, ymin, ymax)
#> coord. ref. : NA 
#> data source : in memory
#> names       : layer 
#> values      : 271.602, 289.478  (min, max)

## note how we actually resample up because this data is not very large
as_raster(lazy)
#> class       : RasterLayer 
#> dimensions  : 480, 672, 322560  (nrow, ncol, ncell)
#> resolution  : 0.01489583, 0.04171065  (x, y)
#> extent      : 140, 150.01, -60.01833, -39.99722  (xmin, xmax, ymin, ymax)
#> coord. ref. : NA 
#> data source : in memory
#> names       : layer 
#> values      : 271.35, 289.843  (min, max)

## what do we get if we set up a bitmap device
tf <- tempfile(fileext = "png")
png(tf, height = 50, width = 40)
as_raster(lazy)
#> class       : RasterLayer 
#> dimensions  : 50, 40, 2000  (nrow, ncol, ncell)
#> resolution  : 0.25025, 0.4004222  (x, y)
#> extent      : 140, 150.01, -60.01833, -39.99722  (xmin, xmax, ymin, ymax)
#> coord. ref. : NA 
#> data source : in memory
#> names       : layer 
#> values      : 271.41, 289.815  (min, max)
#plot(as_raster(lazy))
dev.off()
#> png 
#>   2
unlink(tf)
```

It does work on really big files.

(This example can’t work on your computer probably given use of local
raadtools, but try it on your favourite big file).

This takes a fairly large grid and plots just enough detail by reading
just enough detail for the plot space. That’s all that happens.

``` r
library(raadtools)
#> Loading required package: raster
#> Loading required package: sp
f <- raadtools::topofile("gebco_14")
lazyraster(f)
library(raster)
op <- par(mar = rep(0, 4))
system.time({
  r <- lazyraster(f)
plot(r, col = grey(seq(0, 1, length = 100)), axes = FALSE, xlab = "", ylab = "", asp = "", legend = FALSE)
})
```

<img src="man/figures/README-raadtools-1.png" width="100%" />

    #>    user  system elapsed 
    #>   0.801   0.036   0.840
    par(op)

We’ve successfully used it to plot a DEM of Australia from a 67 Gb ESRI
binary grid (ADF) supplied by GeoScience Australia in less than a minute
(the grid is more than 1e5 pixels each dimension).

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
