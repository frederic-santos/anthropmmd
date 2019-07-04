library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)
## Compute MMD results with AnthropMMD:
mmd.ans <- mmd(tab, angular = "Anscombe")
## Compute classical (metric) MDS:
coor_mmds <- cmdscale(mmd.ans$MMDSym)

test_that("mds_rho gives correct answer", {
  expect_equal(mds_rho(mmd = mmd.ans$MMDSym, coor = coor_mmds), 0.988)
})
