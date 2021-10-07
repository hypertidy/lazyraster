# lazyraster 0.5.1

Patch fix for release of vapour 0.8.5. Previously, `vapour_read_raster()` always returned numeric values, but now may
be numeric, integer, or raw. 

* Removed unused LazyData property. 

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

