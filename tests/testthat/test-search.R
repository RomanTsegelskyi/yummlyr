library(testthat)
library(yummlyR)

context("Search Recipes")

test_that("Search is correct", {
    res <- search_recipes("bacon")
    expect_is(res, "list")
    expect_equal(length(res), 5)
})
