sstfile <- system.file("extdata/sst.tif", package = "vapour")
lr <- lazyraster(sstfile)
test_that("native works", {
  expect_equal(dim(as_raster(lr, native = TRUE)), c(286L, 143L, 1L))
  expect_equal(dim(as_raster(lr, dim = c(10, 10))), c(10L, 10L, 1L))
  expect_equal(dim(as_raster(crop(lr, raster::extent(145, 149, -58, -55)), dim = c(10, 10))), c(10L, 10L, 1L))
})
