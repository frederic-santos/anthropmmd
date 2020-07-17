library(AnthropMMD)

### Generate artifical matrices:
m <- matrix(1:9, ncol = 3, nrow = 3)
n <- matrix(10:18, ncol = 3, nrow = 3)
res <- matrix(c(5, 11, 12, 4, 5, 15, 7, 8, 5),
              ncol = 3, nrow = 3)

## Test:
test_that("mix_matrices gives correct answer", {
  expect_equal(mix_matrices(m, n, diag_value = 5), res)
})
