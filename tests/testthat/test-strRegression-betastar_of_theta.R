## ---- test-strRegression-betastar_of_theta
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
obj_i <- lm(
  y ~ .,
  data = x_i
)
answer_i <- stats::coef(
  lm(
    y ~ .,
    data = as.data.frame(scale(x_i))
  )[-1]
)
theta_i <- c(
  stats::coef(obj_i)[-1],
  summary.lm(obj_i)$sigma^2,
  vech(cov(x_i[, -1, drop = FALSE]))
)
result_i <- unname(
  betastar_of_theta(
    theta_i
  )
)
testthat::test_that("strRegression-betastar_of_theta", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
    )
  )
})
testthat::test_that("strRegression-betastar_of_theta error", {
  testthat::expect_error(
    betastar_of_theta(c(1, 1))
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i,
  theta_i,
  obj_i,
  x_i,
  answer_i,
  result_i
)
