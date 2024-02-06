library(testthat)

test_that("computeHR function works correctly", {
    # Specify the path to your CSV file
    file_path <- "/Users/yg/Desktop/test.csv"

    # Run the computeHR function with your CSV file
    result <- computeHR(file_path = file_path)

    # Check that the result is a list
    expect_is(result, "list")

    # Check that the list has the expected names
    expected_names <- c("finalsubseq", "candidateHR", "results_ACF", "results_TI")
    expect_equal(names(result), expected_names)
})
