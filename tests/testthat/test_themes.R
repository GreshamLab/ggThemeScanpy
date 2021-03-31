plt <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, color = Petal.Length)) +
  ggplot2::geom_point()

test_that("Apply theme to scatter plot", {
  scanpy.plt <- theme_scanpy_scatter(plt)
  expect_s3_class(scanpy.plt$theme$axis.ticks, "element_blank")
})
#> Test passed
