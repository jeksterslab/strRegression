## ---- test-strRegression-thetastar_of_sigmacap
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
  data = as.data.frame(scale(x_i))
)

if ((k_i - 1) == 1) {
  answer_i <- unname(
    c(
      stats::coef(obj_i)[-1],
      sd(x_i$y),
      sd(x_i[, -1])
    )
  )
} else {
  vechs_x_i <- stats::cor(x_i[, -1, drop = FALSE])
  vechs_x_i <- vechs_x_i[lower.tri(vechs_x_i, diag = FALSE)]
  answer_i <- unname(
    c(
      stats::coef(obj_i)[-1],
      sd(x_i$y),
      sqrt(diag(stats::var(x_i[, -1, drop = FALSE]))),
      vechs_x_i
    )
  )
  rm(vechs_x_i)
}
result_i <- unname(
  thetastar_of_sigmacap(
    cov(x_i)
  )
)
testthat::test_that("strRegression-thetastar_of_sigmacap", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
    )
  )
})
# coverage
thetastar_of_sigmacap(cov(x_i[, c(1, 2)]))
# clean environment
rm(
  tol_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i,
  x_i,
  obj_i,
  answer_i,
  result_i
)
