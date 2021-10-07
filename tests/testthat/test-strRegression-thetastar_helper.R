## ---- test-strRegression-thetastar_helper
thetastar_i <- thetastar_helper(
  c(
    0.5, 0.5, # betastar
    1, # sigmay
    1, 1, # sigmax
    0.5 # vechsrhocapx
  )
)
testthat::test_that("test-strRegression-thetastar_helper betastar", {
  testthat::expect_equal(
    thetastar_i$betastar,
    c(0.5, 0.5)
  )
})
testthat::test_that("test-strRegression-thetastar_helper sigmay", {
  testthat::expect_equal(
    thetastar_i$sigmay,
    1
  )
})
testthat::test_that("test-strRegression-thetastar_helper sigmax", {
  testthat::expect_equal(
    thetastar_i$sigmax,
    c(1, 1)
  )
})
testthat::test_that("test-strRegression-thetastar_helper rhocapx", {
  testthat::expect_equal(
    thetastar_i$rhocapx,
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
# expect_error
testthat::test_that("test-strRegression-thetastar_helper error", {
  testthat::expect_error(
    thetastar_helper(1:4)
  )
})
thetastar_i <- thetastar_helper(
  c(
    0.5, # betastar
    1, # sigmay
    1 # sigmax
  )
)
testthat::test_that("test-strRegression-thetastar_helper betastar simple", {
  testthat::expect_equal(
    thetastar_i$betastar,
    0.5
  )
})
testthat::test_that("test-strRegression-thetastar_helper sigmay simple", {
  testthat::expect_equal(
    thetastar_i$sigmay,
    1
  )
})
testthat::test_that("test-strRegression-thetastar_helper sigmax simple", {
  testthat::expect_equal(
    thetastar_i$sigmax,
    1
  )
})
# clean environment
rm(
  thetastar_i
)
