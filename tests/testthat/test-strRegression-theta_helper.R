## ---- test-strRegression-theta_helper
theta_i <- theta_helper(
  c(
    0.5, 0.5, # beta
    1, # sigmasq
    1, 0.5, 1, # vechsigmacapx
    0, # alpha
    0, 0 # mux
  ),
  mean_structure = TRUE
)
testthat::test_that("strRegression-theta_helper beta", {
  testthat::expect_equal(
    theta_i$beta,
    c(0.5, 0.5)
  )
})
testthat::test_that("strRegression-theta_helper sigmasq", {
  testthat::expect_equal(
    theta_i$sigmasq,
    1
  )
})
testthat::test_that("strRegression-theta_helper sigmacapx", {
  testthat::expect_equal(
    theta_i$sigmacapx,
    matrix(
      data = c(
        1,
        0.5,
        0.5,
        1
      ),
      nrow = 2
    )
  )
})
testthat::test_that("strRegression-theta_helper alpha", {
  testthat::expect_equal(
    theta_i$alpha,
    0
  )
})
testthat::test_that("strRegression-theta_helper mux", {
  testthat::expect_equal(
    theta_i$mux,
    c(0, 0)
  )
})
# expect_error
testthat::test_that("strRegression-theta_helper error", {
  testthat::expect_error(
    theta_helper(1:4)
  )
})
testthat::test_that("strRegression-theta_helper error2", {
  testthat::expect_error(
    theta_helper(1:6, mean_structure = TRUE)
  )
})
# coverage
theta_helper(
  c(
    0.5, 0.5, # beta
    1, # sigmasq
    1, 0.5, 1 # vechsigmacapx
  ),
  mean_structure = FALSE
)
# clean environment
rm(
  theta_i
)
