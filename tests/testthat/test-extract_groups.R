library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = FALSE)
expected <- paste("Group", LETTERS[1:5], sep = "")

## Check if the groups names can be extracted:
test_that("Group names OK for binary data:", {
    expect_equal(extract_groups(toyMMD, type = "raw"), expected)
})
test_that("Group names OK for tables:", {
    expect_equal(extract_groups(tab, type = "table"), expected)
})

