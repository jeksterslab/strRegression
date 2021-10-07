## ---- test-strRegression-theta_wrt_vechsigmacap
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
answer_i <- numDeriv::jacobian(
  func = theta_of_vechsigmacap,
  x = vech(stats::cov(x_i))
)
result_i <- unname(
  theta_wrt_vechsigmacap(
    vech(cov(x_i))
  )
)
testthat::test_that("test-strRegression-theta_wrt_vechsigmacap", {
  testthat::expect_true(
    all(
      abs(
        as.vector(
          answer_i
        ) - as.vector(
          result_i
        )
      ) <= tol_i
    )
  )
})
testthat::test_that("test-strRegression-theta_wrt_vechsigmacap error", {
  testthat::expect_error(
    theta_wrt_vechsigmacap(c(1, 1))
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
