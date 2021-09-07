# lazyraster dev

* New function `merc()` to give an easy extent in Mercator from longitude,latitude and a width,height. 

* Fixed plot method documentation, thanks to @brownag https://github.com/hypertidy/lazyraster/issues/20. 

* lazyraster now uses `vapour_warp_raster()` to do the work, no more need for RasterIO. 

* Speed up `as_raster()` by avoiding unlist(). 

* Nodata values now respected for `as_raster()`. 

* The underlying projection string is maintained by `as_raster()`

* Really fixed crop to as_`raster()` when `native = TRUE`, thanks to @Blundys in https://github.com/hypertidy/lazyraster/issues/19. 

* Projection string now displayed in print, prefers projstring over WKT but prints the latter if projstring not present. 

* Now importing vapour >= 0.7.6. 

* Allow multiple bands, defaults to all available. 

* Avoid issue of non-integer dimensions, with different rounding schemes #13. 

* Added window dimension to lazyraster print out (boilerplate for no crop). 

# lazyraster 0.5.0

* fix default window for native when no crop applied, https://github.com/hypertidy/lazyraster/issues/9

* clarify DESCRIPTION thanks to CRAN feedback

## BREAKING CHANGES

* lazycrop is now removed, we can use `crop()` directly. 

## CHANGES

* S4 method `crop()` is now available for lazyraster. 

* Function `as_raster()` gains a `native` argument to override dimension. 

* Depend on vapour 0.4.0, because change in list return from read raster. 

# lazyraster 0.0.0.9001

* Depend on vapour 0.2.0. 

* Added a `quadmesh` method for lazyraster. 

