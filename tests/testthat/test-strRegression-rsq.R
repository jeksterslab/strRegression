## ---- test-strRegression-rsq
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
obj_i <- lm(y ~ ., data = x_i)
beta_i <- stats::coef(obj_i)[-1]
sigmasq_i <- summary.lm(obj_i)$sigma^2
sigmacap_i <- stats::cov(x_i)
sigmayx_i <- sigmacap_i[2:k_i, 1]
sigmacapx_i <- sigmacap_i[2:k_i, 2:k_i, drop = FALSE]
answer_i <- unname(
  round(
    summary.lm(obj_i)$r.squared,
    digits = 3
  )
)
result_i <- unname(
  round(
    rsq(
      beta = beta_i,
      sigmasq = sigmasq_i,
      sigmayx = sigmayx_i,
      sigmacapx = sigmacapx_i
    ),
    digits = 3
  )
)
testthat::test_that("strRegression-rsq", {
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
  x_i,
  obj_i,
  beta_i,
  sigmasq_i,
  sigmayx_i,
  sigmacapx_i,
  answer_i,
  result_i
)
