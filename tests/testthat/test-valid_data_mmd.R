library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = FALSE)

## Check if those two datasets are OK (they should be!):
test_that("True toyMMD is OK:", {
    expect_equal(valid_data_mmd(toyMMD, type = "raw"), TRUE)
})
test_that("Table derived from true toyMMD is OK:", {
    expect_equal(valid_data_mmd(tab, type = "table"), TRUE)
})

## Stupid checks: provide the wrong type of data:
test_that("A binary dataset defined as a table triggers a message:", {
    expect_warning(valid_data_mmd(toyMMD, type = "table"))
})
test_that("A table defined as a binary dataset triggers a message:", {
    expect_warning(valid_data_mmd(tab, type = "raw"))
})

## Here we add a '2' in the raw dataset and we want to check this is detected:
raw_2 <- toyMMD
raw_2[1, "Trait1"] <- 2
test_that("Non-binary data is detected:", {
    expect_warning(valid_data_mmd(raw_2, type = "raw"), "At least one of your columns does not contain only zeroes and ones. Please check your data.")
})
