library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)
# Filter this dataset to keep only those traits that have at
# least k=10 individuals in each group:
k10 <- select_traits(tab, k = 10)$filtered
# Only Trait1 should be excluded.

# Filter this dataset to keep only those traits that have at
# least k=11 individuals in each group, and show significant
# differences at Fisher's exact test:
fisher <- select_traits(tab, k = 11, strategy = "keepFisher")$filtered
# Traits 1, 5 and 8 should be excluded.

## Tests for MMD values:
test_that("The criterion 'k' works well", {
    expect_equal(colnames(k10), paste("Trait", 2:9, sep=""))
})

test_that("The strategy by Fisher's tests works well", {
    expect_equal(colnames(fisher), paste("Trait", c(2:4, 6:7, 9), sep=""))
})
