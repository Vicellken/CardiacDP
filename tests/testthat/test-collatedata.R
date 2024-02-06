# Load the necessary library
library(testthat)

# Define the tests
test_that("collatedata function tests", {
    # Test that the function stops if the file does not exist
    expect_error(collatedata("nonexistent_file.zip"), "File does not exist at the given path.")

    # Test that the function works with a valid zip file
    test_data <- collatedata("/Users/yg/Desktop/20210518A.zip")
    expect_is(test_data, "data.table")
})
