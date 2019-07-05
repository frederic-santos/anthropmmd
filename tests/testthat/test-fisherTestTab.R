library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)
## Keep only three groups and three traits to simplify:
tab <- tab[c(1:3, 6:8), 2:4]
## Results (p-values) from fisherTestTab:
results <- round(fisherTestTab(tab)$pval, 2)
## Expected result:
matpval <- matrix(c(0.35, 0.51, 1, 0.24, 0.09, 0.01, 1, 0.80, 1),
                  ncol = 3, nrow = 3)
colnames(matpval) <- c("Trait2", "Trait3", "Trait4")
rownames(matpval) <- c("GroupA - GroupB", "GroupA - GroupC", "GroupB - GroupC")

## Check if the groups names can be extracted:
test_that("Group names OK for binary data:", {
    expect_equal(results, matpval)
})
