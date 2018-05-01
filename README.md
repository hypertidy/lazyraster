
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazyraster

The goal of lazyraster is to get raster data on demand at the right
resolution.

We can specify the exact dimensions of the output raster, and by default
a reasonable guess at the number of pixels required to fill the current
device will be used.

We can specify a variety of resampling algorithms (nearest neighbour is
the default) and the resampling can be applied to reduce or increase the
resolution.

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

## Limitations

We can’t utilize the RasterIO level-of-detail functionality for non-GDAL
sources.

We can’t yet easily specify a crop, and level-of-detail - but that’s not
hard - mostly I need to figure out what set of functions should exist
for this.

We can only read the first band.

The only really useable output is a raster layer. You cannot specify
easily that the return is at native resolution (just use raster for
that).

We can’t control the details of the data type.

The projection string is not coming through properly.

The plot-size logic should work on the current “usr” world coordinates,
not the size of the device (if it’s different).

## Example

This is not a huge file, but is easily accessible and demonstrates the
idea.

``` r
sstfile <- system.file("extdata/sst.tif", package = "vapour")
library(lazyraster)
lazy <- lazyraster(sstfile)
lazy ## stay lazy
#> class      : LazyRaster
#> dimensions : 143,286 (nrow, ncol)
#> resolution : 0.07000000,0.07000389 (x, y)
#> extent     : 140.00000,150.01000,-60.01833,-39.99722 (xmin, xmax, ymin, ymax)
#> crs        : 
#> values     : 271.3500,289.8590 (min, max)

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

Does it work on really big files?

(This can’t work on your computer probably, but try it on your favourite
big file).

``` r
library(raadtools)
#> Loading required package: raster
#> Loading required package: sp
f <- raadtools::topofile("gebco_14")
lazyraster(f)
#> class      : LazyRaster
#> dimensions : 43200,21600 (nrow, ncol)
#> resolution : 0.008333333,0.008333333 (x, y)
#> extent     : -180.0000, 180.0000, -90.0000,  90.0000 (xmin, xmax, ymin, ymax)
#> crs        : 
#> values     : -10486.0000,  7729.0000 (min, max)
library(raster)
op <- par(mar = rep(0, 4))
system.time({
  r <- lazyraster(f)
plot(r, col = grey(seq(0, 1, length = 100)), axes = FALSE, xlab = "", ylab = "", asp = "", legend = FALSE)
})
```

<img src="man/figures/README-raadtools-1.png" width="100%" />

    #>    user  system elapsed 
    #>   0.827   0.064   0.895
    par(op)

An earlier example, should be made a WMS-specific function …

Make a TMS source and read at the desired resolution.

More here: <http://rpubs.com/cyclemumner/358029>

``` r
library(lazyraster)
gibs_xml <- function(date, level = 3) {
  date <- format(date, "%Y-%m-%d")
sprintf('<GDAL_WMS>
         <Service name="TMS">
         <ServerUrl>
         https://gibs.earthdata.nasa.gov/wmts/epsg3413/best/MODIS_Terra_CorrectedReflectance_TrueColor/default/%s/250m/${z}/${y}/${x}.jpg</ServerUrl>
         </Service>
         <DataWindow>
         <UpperLeftX>-4194304</UpperLeftX>
         <UpperLeftY>4194304</UpperLeftY>
         <LowerRightX>4194304</LowerRightX>
         <LowerRightY>-4194304</LowerRightY>
         <TileLevel>%i</TileLevel>
         <TileCountX>2</TileCountX>
         <TileCountY>2</TileCountY>
         <YOrigin>top</YOrigin>
         </DataWindow>
         <Projection>EPSG:3413</Projection>
         <BlockSizeX>512</BlockSizeX>
         <BlockSizeY>512</BlockSizeY>
         <BandsCount>3</BandsCount>
         </GDAL_WMS>
         ', date, level)
}

s <- gibs_xml(Sys.Date()-10)
r <- raster::raster(s)
r2 <- collect(r, nrows= 150, ncols = 150)
#> r is 8192x8192, collecting output of 150x150 
#>  use 'nrows = , ncols = ' to customize output size
library(raster)
plot(r2, col = head(palr::sstPal(64), 45))
```

<img src="man/figures/README-example-1.png" width="100%" />
