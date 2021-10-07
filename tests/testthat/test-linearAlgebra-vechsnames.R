## ---- test-linearAlgebra-vechsnames
testthat::test_that("test-linearAlgebra-vechsnames 1d", {
  testthat::expect_error(
    vechsnames("x1")
  )
})
testthat::test_that("test-linearAlgebra-vechsnames 2d", {
  testthat::expect_equal(
    "x1.x2",
    vechsnames(c("x1", "x2"))
  )
})
testthat::test_that("test-linearAlgebra-vechsnames 3d", {
  testthat::expect_equal(
    c("x1.x2", "x1.x3", "x2.x3"),
    vechsnames(c("x1", "x2", "x3"))
  )
})
