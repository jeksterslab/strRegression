## ---- test-strRegression-beta_of_vechsigmacap
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
vech_i <- cov(x_i)
vech_i <- vech_i[lower.tri(vech_i, diag = TRUE)]
answer_i <- unname(
  stats::coef(
    lm(
      y ~ .,
      data = x_i
    )
  )[-1]
)
result_i <- unname(
  beta_of_vechsigmacap(
    vech_i
  )
)
testthat::test_that("strRegression-beta_of_vechsigmacap", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
    )
  )
})
testthat::test_that("strRegression-beta_of_vechsigmacap error", {
  testthat::expect_error(
    beta_of_vechsigmacap(c(1, 1))
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
  vech_i,
  answer_i,
  result_i
)
