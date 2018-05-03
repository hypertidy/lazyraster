
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
`raster::crop` and then `as_raster` to break the lazy chain and build an
actual raster object. The size of the currently open (or latent-default)
device is used as a reasonable size for the output grid, but can be
controlled by argument `dim`.

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

The projection string is not coming through properly, this is a problem
in vapour.

The plot-size logic should work on the current “usr” world coordinates,
not the size of the device (if it’s different).

The vapour package isn’t yet on CRAN.

Subdataset support is very new and needs checking.

Using online sources is not easy, it needs a particular GDAL connection
string to work properly.

## GDAL

This uses a standard internal functionality of GDAL, the [RasterIO
function of the
GDALRasterBand](http://www.gdal.org/classGDALRasterBand.html#a30786c81246455321e96d73047b8edf1).
This is used in a lot of different software, and is obviously pretty
robust and well tested by the GDAL community, but I only really have
experience with one product (commercial, now defunct) that used it
extensively for live interactive visualization and data streaming. I
haven’t found any problems with it at all using it in R, but the support
for it is very minimal. You can access it indirectly using
`rgdal::readGDAL` for the underlying function, as the `raster` package
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

First we connect to a source and show two versions, the first is
information about the data in its native form (286 rows and 143
columns), and then an actual RasterLayer but at a very small requested
size (24 rows by 12 columns).

``` r
sstfile <- system.file("extdata/sst.tif", package = "vapour")
library(lazyraster)
lazy <- lazyraster(sstfile)
lazy ## stay lazy
#> class         : LazyRaster
#> dimensions    : 286, 143 (nrow, ncol)
#> resolution    : 0.07000000, 0.07000389 (x, y)
#> extent        : 140.00000, 150.01000, -60.01833, -39.99722 (xmin, xmax, ymin, ymax)
#> crs           : <placeholder>
#> values        : 271.3500, 289.8590 (min, max - range from entire extent)
#> window extent : <whole extent>
#> window index  : <->

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
```

The call to `as_raster` read actual data from the file, hence the
difference between the range of data values reported first from the
whole extent, and then reported by raster itself for the resample data
read in.

Now let `lazyraster` make its own choice about the size of the output.
This will be based on the return value of `dev.size("px")`.

``` r
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
```

More concretely, if we open a graphics device at a given size the raster
data read in will match it. (This is not the best choice but works fine
for demonstration and experimenting.)

``` r
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

This will work on really big files.

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
#> class         : LazyRaster
#> dimensions    : 21600, 43200 (nrow, ncol)
#> resolution    : 0.008333333, 0.008333333 (x, y)
#> extent        : -180.0000,  180.0000,  -90.0000,   90.0000 (xmin, xmax, ymin, ymax)
#> crs           : <placeholder>
#> values        : -10486.0000,   7729.0000 (min, max - range from entire extent)
#> window extent : <whole extent>
#> window index  : <->
library(raster)
op <- par(mar = rep(0, 4))
system.time({
  rworld <- lazyraster(f)
plot(rworld, col = grey(seq(0, 1, length = 100)), axes = FALSE, xlab = "", ylab = "", asp = "", legend = FALSE)
})
```

<img src="man/figures/README-raadtools-1.png" width="100%" />

    #>    user  system elapsed 
    #>   0.808   0.065   0.878
    par(op)

Now, plot the same kind of image but zoom in on a region purposefully.
The resolution provided has adapted to the context asked for.

``` r
rtas <- lazycrop(rworld, extent(143.4, 149, -44, -39.1))
plot(rtas, col = grey(seq(0, 1, length.out = 64)), zlim = c(0, 1550))
rbath <- as_raster(rtas)
rbath[rbath > 0] <- NA
contour(rbath, add = TRUE, levels = quantile(rbath, prob = seq(0, 1, length.out = 8)))
title("Tasmania topography + bathymetric contours, from Gebco 2014", cex.main = 0.85)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

## How useful is this, really?

This is not just to plot big rasters, it’s potentially useful for
streaming gridded data to a device that is resizing the view port
interactively. We also use it to explore a data set for useability and
general coverage, and designing sensible resampling workflows for very
large data models.

We’ve successfully used it to plot a DEM of Australia from a 67 Gb ESRI
binary grid (ADF) supplied by GeoScience Australia in *a few minutes*
(the grid is more than 1e5 pixels each dimension, so I’m not having this
document build do the job but here’s a figure I prepared earlier).

``` r
gafile <- raadtools::topofile("ga_srtm")
ga <- lazyraster(gafile)
ga
#> class         : LazyRaster
#> dimensions    : 122400, 147600 (nrow, ncol)
#> resolution    : 0.0002777778, 0.0002777778 (x, y)
#> extent        : 112.99986, 153.99986, -44.00014, -10.00014 (xmin, xmax, ymin, ymax)
#> crs           : <placeholder>
#> values        :  -68.05186, 2223.24780 (min, max - range from entire extent)
#> window extent : <whole extent>
#> window index  : <->
## plot(ga)
```

![GeoScience SRTM](inst/images/ga_srtm.png "GeoScience SRTM")

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
