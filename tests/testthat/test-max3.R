library(AnthropMMD)
data(toyMMD)

test_that("max3 gives correct answer", {
  expect_equal(max3(binary_to_table(toyMMD)), 20)
})
