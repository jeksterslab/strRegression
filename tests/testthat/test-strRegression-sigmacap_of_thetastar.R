## ---- test-strRegression-sigmacap_of_thetastar
tol_i <- 0.01
n_i <- 100000
k_i <- sample(
  3:10,
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
answer_i <- unname(
  vech(
    cov(x_i)
  )
)
result_i <- unname(
  vech(
    sigmacap_of_thetastar(
      c(
        stats::coef(obj_i)[-1],
        sd(x_i[, 1]),
        sqrt(diag(cov(x_i[, -1]))),
        vechs(cor(x_i[, -1]))
      )
    )
  )
)
testthat::test_that("test-strRegression-sigmacap_of_thetastar", {
  testthat::expect_true(
    all(
      abs(
        answer_i - result_i
      ) <= tol_i
    )
  )
})
# k = 2
tol_i <- 0.01
n_i <- 100000
k_i <- 2
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
answer_i <- unname(
  vech(
    cov(x_i)
  )
)
result_i <- unname(
  vech(
    sigmacap_of_thetastar(
      c(
        stats::coef(obj_i)[-1],
        sd(x_i[, 1]),
        sqrt(diag(var(x_i[, -1, drop = FALSE])))
      )
    )
  )
)
testthat::test_that("test-strRegression-sigmacap_of_thetastar simple", {
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
  answer_i,
  result_i
)
