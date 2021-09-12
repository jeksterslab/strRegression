## ---- test-linearAlgebra-sym_of_vechs
x_i <- c(0.5, 0.4, 0.6)
answer_i <- matrix(
  data = c(
    1.0, 0.5, 0.4,
    0.5, 1.0, 0.6,
    0.4, 0.6, 1.0
  ),
  ncol = 3
)
result_i1_i <- sym_of_vechs(
  x_i,
  diags = 1
)
result_i2_i <- sym_of_vechs(
  x_i,
  diags = diag(answer_i)
)
testthat::test_that("linearAlgebra-sym_of_vechs 3 by 3", {
  testthat::expect_equal(
    result_i1_i,
    result_i2_i,
    answer_i
  )
})
x_i <- "b"
answer_i <- matrix(
  c("a", "b", "b", "c"),
  ncol = 2
)
result_i <- sym_of_vechs(
  x_i,
  diags = diag(answer_i)
)
testthat::test_that("linearAlgebra-sym_of_vechs 2 by 2", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
n_i <- 100
k_i <- sample(
  c(1, 3, 6, 10),
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
x_i <- matrix(
  data = rnorm(
    n = n_i * k_i
  ),
  nrow = n_i,
  ncol = k_i
) %*% (
  chol(sigmacap_i)
) + (
  matrix(
    data = 1,
    nrow = n_i,
    ncol = 1
  ) %*% mu_i
)
answer_i <- cov(x_i)
result_i <- sym_of_vechs(
  answer_i[lower.tri(answer_i, diag = FALSE)],
  diags = diag(answer_i)
)
testthat::test_that("linearAlgebra-sym_of_vechs random cov", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
# expect_error
testthat::test_that("linearAlgebra-sym_of_vechs error", {
  testthat::expect_error(
    sym_of_vechs(as.matrix(1:5))
  )
})
testthat::test_that("linearAlgebra-sym_of_vechs error", {
  testthat::expect_error(
    sym_of_vechs(rnorm(n = 4), diags = 1)
  )
})
# clean environment
rm(
  x_i,
  result_i1_i,
  result_i2_i,
  answer_i,
  result_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i
)
