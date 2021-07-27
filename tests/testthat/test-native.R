sstfile <- system.file("extdata/sst.tif", package = "vapour")
lr <- lazyraster(sstfile)




test_that("native works", {
  expect_equal(dim(as_raster(lr, native = TRUE)), c(286L, 143L, 1L))
  expect_equal(dim(as_raster(lr, dim = c(10, 10))), c(10L, 10L, 1L))
  expect_equal(dim(as_raster(crop(lr, raster::extent(145, 149, -58, -55)), dim = c(10, 10))), c(10L, 10L, 1L))
})



library(raster)

dummycroparea <- extent(0.2,0.8,0.2,0.8)

r1 <- raster(matrix(1:25, nrow = 5), xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, template=NULL)
#writeRaster(r1, "inst/images/dummyraster_sml.tif")
lr1 <- lazyraster(system.file("images", "dummyraster_sml.tif", package = "lazyraster", mustWork = TRUE))


test_that("native also works", {
          r1_crop <-  raster::crop(r1, dummycroparea)
          lr_crop <- lazyraster::crop(lr1, dummycroparea)
          lr_crop_to_rast <- as_raster(lr_crop, native = TRUE)

      expect_equal(dim(r1_crop), dim(lr_crop_to_rast))
}
)
