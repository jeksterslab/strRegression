## ---- test-strRegression-beta
tol_i <- 0.01
n_i <- 100000
k_i <- sample(
  2:10,
  size = 1
)
mu_i <- rep(
  x = 0,
  times = k_i
)
sigmacap_i <- matrix(
  runif(
    n = 1,
    min = 0,
    max = 1
  ),
  nrow = k_i,
  ncol = k_i
)
diag(sigmacap_i) <- 1
x_i <- rmvn_chol(
  n = n_i,
  mu = mu_i,
  sigmacap = sigmacap_i,
  varnames = c(
    "y",
    paste0("x", seq_len(k_i - 1))
  ),
  data_frame = TRUE
)
answer_i <- unname(
  stats::coef(
    lm(
      y ~ .,
      data = x_i
    )
  )[-1]
)
result_i <- unname(
  beta(
    sigmacapx = cov(x_i)[2:k_i, 2:k_i, drop = FALSE],
    sigmayx = cov(x_i)[1, 2:k_i]
  )
)
testthat::test_that("test-strRegression-beta", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
    )
  )
})
testthat::test_that("test-strRegression-beta singular", {
  testthat::expect_true(
    all(
      is.na(
        beta(
          sigmacapx = matrix(
            data = 1,
            nrow = 2,
            ncol = 2
          ),
          sigmayx = c(1, 1)
        )
      )
    )
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i,
  x_i,
  answer_i,
  result_i
)
