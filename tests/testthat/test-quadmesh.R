
sstfile <- system.file("extdata/sst.tif", package = "vapour")
lr <- lazyraster(sstfile)

test_that("quadmesh from lazyraster works", {
  expect_s3_class(lr, "lazyraster")
})
