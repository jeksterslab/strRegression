## ---- test-strRegression-theta_of_sigmacap
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
vech_x_i <- stats::var(x_i[, -1, drop = FALSE])
vech_x_i <- vech_x_i[lower.tri(vech_x_i, diag = TRUE)]
answer_i <- unname(
  c(
    stats::coef(obj_i)[-1],
    summary.lm(obj_i)$sigma^2,
    vech_x_i
  )
)
result_i <- unname(
  theta_of_sigmacap(
    cov(x_i)
  )
)
testthat::test_that("strRegression-theta_of_sigmacap", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
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
  vech_x_i,
  x_i,
  obj_i,
  answer_i,
  result_i
)
