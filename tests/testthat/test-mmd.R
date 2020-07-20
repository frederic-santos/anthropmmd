library(AnthropMMD)

########################
### Data preparation ###
########################
## Import dataset and transform it into a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)

## Compute MMD results with AnthropMMD:
mmd_ans <- mmd(tab, angular = "Anscombe")
mmd_ft <- mmd(tab, angular = "Freeman")

## Expected result when using Anscombe transformation:
res_ans <- matrix(c(0.000, 0.255, 0.045, 0.303,	0.069,
                    0.255, 0.000, 0.218, 0.365, 0.241,
                    0.045, 0.218, 0.000, 0.088, 0.034,
                    0.303, 0.365, 0.088, 0.000,	0.168,
                    0.069, 0.241, 0.034, 0.168,	0.000),
                  byrow = TRUE, ncol = 5)
colnames(res_ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result when using Freeman-Tukey transformation:
res_ft <- matrix(c(0.000, 0.271, 0.042,	0.298, 0.064,
                   0.271, 0.000, 0.225,	0.365, 0.251,
                   0.042, 0.225, 0.000,	0.090, 0.031,
                   0.298, 0.365, 0.090,	0.000, 0.168,
                   0.064, 0.251, 0.031,	0.168, 0.000),
                 byrow = TRUE, ncol = 5)
colnames(res_ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result for MMD significance:
res_sig <- matrix(c(NA, "0.255", "0.045", "0.303", "0.069",
                    "*", NA, "0.218", "0.365", "0.241",
                    "NS", "*", NA, "0.088", "0.034",
                    "*", "*", "*", NA, "0.168",
                    "NS", "*", "NS", "*", NA),
                  byrow = TRUE, ncol = 5)
colnames(res_sig) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_sig) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

############################
### Tests for MMD values ###
############################
test_that("MMD values are correct with Anscombe transformation", {
    expect_equal(round(mmd_ans$MMDSym, 3), res_ans)
})

test_that("MMD values are correct with Freeman-Tukey transformation", {
    expect_equal(round(mmd_ft$MMDSym, 3), res_ft)
})

## Tests for MMD significance:
test_that("MMD significance is OK", {
    expect_equal(mmd_ans$MMDSignif, res_sig)
})

## Tests for p-values:
test_that("p-value C-E is OK", {
    expect_equal(mmd_ans$MMDpval[5, 3], 0.0954)
})

test_that("p-value A-E is OK", {
    expect_equal(mmd_ans$MMDpval[5, 1], 0.2638)
})
