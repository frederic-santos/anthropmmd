library(AnthropMMD)

## Import dataset and transform it to a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)
## Compute MMD results with AnthropMMD:
mmd.ans <- mmd(tab, angular = "Anscombe")
mmd.ft <- mmd(tab, angular = "Freeman")
## Expected result when using Anscombe transformation:
res.ans <- matrix(c(0.000, 0.255, 0.045, 0.303,	0.069,
                    0.255, 0.000, 0.218, 0.365, 0.241,
                    0.045, 0.218, 0.000, 0.088, 0.034,
                    0.303, 0.365, 0.088, 0.000,	0.168,
                    0.069, 0.241, 0.034, 0.168,	0.000),
                  byrow = TRUE, ncol = 5)
colnames(res.ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res.ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
## Expected result when using Freeman-Tukey transformation:
res.ft <- matrix(c(0.000, 0.271, 0.042,	0.298, 0.064, 
                   0.271, 0.000, 0.225,	0.365, 0.251, 
                   0.042, 0.225, 0.000,	0.090, 0.031,
                   0.298, 0.365, 0.090,	0.000, 0.168,
                   0.064, 0.251, 0.031,	0.168, 0.000),
                 byrow = TRUE, ncol = 5)
colnames(res.ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res.ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Tests for MMD values:
test_that("MMD values are correct with Anscombe transformation", {
    expect_equal(round(mmd.ans$MMDSym, 3), res.ans)    
})

test_that("MMD values are correct with Freeman-Tukey transformation", {
    expect_equal(round(mmd.ft$MMDSym, 3), res.ft)    
})
