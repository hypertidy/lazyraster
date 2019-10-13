
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
  #vdiffr::expect_doppelganger("plot-lazraster", plot_lr)
})
