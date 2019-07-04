library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)
## Make a copy, with Trait8 as quasi non-polymorphic:
tab_QNPT <- tab
tab_QNPT[6:10, 8] <- c(0.1, 0, 0, 0, 0)
## Make a copy, with Trait8 as non-polymorphic:
tab_NPT <- tab
tab_NPT[6:10, 8] <- c(1, 1, 1, 1, 1)

## Filter this dataset to keep only those traits that have at
## least k=10 individuals in each group:
k10 <- select_traits(tab, k = 10, strategy = "none")$filtered
## Only Trait1 should be excluded.

## Filter the dataset to keep only those traits that have at
## least k=10 individuals in the groups A, C, D and E:
groupsACDE <- select_traits(tab, k = 10, groups = c("GroupA", "GroupC", "GroupD", "GroupE"))$filtered
## No trait should be excluded

## Filter this dataset to keep only those traits that are non QNP:
qnpt <- select_traits(tab_QNPT, k = 1, strategy = "excludeQNPT")$filtered
## Only Trait8 should be excluded.

## Filter this dataset to keep only those traits that are non QNP:
npt <- select_traits(tab_NPT, k = 1, strategy = "excludeNPT")$filtered
## Only Trait8 should be excluded.

## Filter the dataset to keep only those traits that have at
## least k=11 individuals in each group, and show significant
## differences at Fisher's exact test:
fisher <- select_traits(tab, k = 11, strategy = "keepFisher")$filtered
## Traits 1, 5 and 8 should be excluded.

## Filter the dataset to keep only those traits that have at
## least k=11 individuals in each group, and show significant
## differences at Fisher's exact test:
fisher <- select_traits(tab, k = 11, strategy = "keepFisher")$filtered
## Traits 1, 5 and 8 should be excluded.

## Filter the dataset to keep only those traits that have at
## least k=10 individuals in each group, and exhibit OMD > 0.95:
omd09ft <- select_traits(tab, k = 10, strategy = "excludeNOMD", OMDvalue = 0.9, angular = "Freeman")$filtered
## Traits 1, 4, 5, 8 and 9 should be excluded.
omd09ans <- select_traits(tab, k = 10, strategy = "excludeNOMD", OMDvalue = 0.9, angular = "Anscombe")$filtered
## Traits 1, 4, 5, and 9 should be excluded.

### Tests for correct exclusion:
test_that("The criterion 'k' works well", {
    expect_equal(colnames(k10), paste("Trait", 2:9, sep=""))
})

test_that("The strategy by Fisher's tests works well", {
    expect_equal(colnames(fisher), paste("Trait", c(2:4, 6:7, 9), sep=""))
})

test_that("The strategy by OMD threshold works well (FT)", {
    expect_equal(colnames(omd09ft), paste("Trait", c(2:3, 6:7), sep=""))
})

test_that("The strategy by OMD threshold works well (Anscombe)", {
    expect_equal(colnames(omd09ans), paste("Trait", c(2:3, 6:8), sep=""))
})

test_that("The strategy by QNPT works well", {
    expect_equal(colnames(qnpt), paste("Trait", c(1:7, 9), sep=""))
})

test_that("The strategy by NPT works well", {
    expect_equal(colnames(npt), paste("Trait", c(1:7, 9), sep=""))
})

test_that("The argument groups works well", {
    expect_equal(colnames(groupsACDE), paste("Trait", c(1:9), sep=""))
})
