
sstfile <- system.file("extdata/sst.tif", package = "vapour")
lr <- lazyraster(sstfile)

test_that("print works", {
  expect_output(print(lr)) %>% expect_s3_class("lazyraster")
})

test_that("format works", {
  expect_that(unlist(format(lr)), is_a("character"))
})

test_that("plot works", {
  plot_lr <- function() {
    plot(lr)
  }
  expect_silent(plot_lr)
  plot_lr_zlim <- function() {
    plot(lr, zlim = c(270, 291))
  }
  expect_silent( plot_lr_zlim)

})

test_that("out of bounds snaps to the region", {
  expect_equal(crop(lr, extent(lr) + 1)$window$window, c(0, 142, 0, 285))
})

test_that("raster lazyraster", {
  expect_s4_class(raster(lr, native = TRUE), "BasicRaster")
})

test_that("error handling", {
  expect_error(lazyraster(sstfile, band = 0))
  expect_error(lazyraster(sstfile, band = 2))

  expect_equal(dim(lazy_to_raster(lr, dim = NULL)), c(286, 143, 1))

  expect_warning(pull_lazyraster(lr, pulldim = 10))
  expect_message(expect_warning(pull_lazyraster(lr, pulldim = 10, native = TRUE)))

  expect_s4_class(pull_lazyraster(lr), "BasicRaster")
})
