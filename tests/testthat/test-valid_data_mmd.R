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

## Here we check that incorrect labels in tables are detected:
tab_mod <- tab
rownames(tab_mod)[1:5] <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
test_that("Incorrect labels in tables are detected:", {
    expect_warning(valid_data_mmd(tab_mod, type = "table"), "If there are k groups in your data file, the first k lines should be something like 'N_Group1', 'N_Group2', ..., with a mandatory 'N_' prefix.")
})

## Here we check that a file with less than two columns is discarded:
foo <- apply(toyMMD, MARGIN = 1, paste0, collapse = ";")
foo <- as.data.frame(foo) # emulates a wrong field sep
test_that("Wrong field sep is detected:", {
    expect_warning(valid_data_mmd(foo, type = "raw"), "Only one column read in the data file. Please check the field separator.")
})
