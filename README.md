
<!-- README.md is generated from README.Rmd. Please edit that file -->
lazyraster
==========

The goal of lazyraster is to get raster data on demand at the right resolution.

Example
-------

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
#> Loading required package: sp
plot(r2, col = head(palr::sstPal(64), 45))
```

<img src="man/figures/README-example-1.png" width="100%" />
