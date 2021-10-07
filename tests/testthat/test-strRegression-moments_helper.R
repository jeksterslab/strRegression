## ---- test-strRegression-moments_helper
k_i <- sample(x = 2:10, size = 1)
vech_x_i <- toeplitz((k_i:1) / k_i)
vech_x_i <- vech_x_i[lower.tri(vech_x_i, diag = TRUE)]
theta_i <- moments_helper(
  c(
    rep(x = 0, times = k_i),
    vech_x_i
  )
)
testthat::test_that("test-strRegression-moments_helper means", {
  testthat::expect_equal(
    theta_i$mu,
    rep(x = 0, times = k_i)
  )
})
testthat::test_that("test-strRegression-moments_helper covariances", {
  testthat::expect_equal(
    theta_i$sigmacap,
    toeplitz((k_i:1) / k_i)
  )
})
# expect_error
testthat::test_that("test-strRegression-moments_helper error", {
  testthat::expect_error(
    moments_helper(1:6)
  )
})
# clean environment
rm(
  k_i,
  theta_i,
  vech_x_i
)
