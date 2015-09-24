library(testthat)
library(yummlyr)

context("Misc")

test_that("Logging is correct", {
    require(futile.logger)
    yummlyr_options("log", "yummlyr")
    flog.threshold(INFO)
    expect_output(enable_caching(), "Caching is enabled")
    expect_output(disable_caching(), "Caching is disabled")
    yummlyr_options("log", NULL)
})

test_that("Caching works correctly", {
    require(futile.logger)
    yummlyr_options("log", "yummlyr")
    flog.threshold(INFO)
    expect_output(enable_caching(), "Caching is enabled")
    expect_output(res <- search_recipes("water"), "query")
    out <- capture.output(res <- search_recipes("water"))
    expect_equal(length(out), 0)
    expect_output(disable_caching(), "Caching is disabled")
    yummlyr_options("log", NULL)
})
