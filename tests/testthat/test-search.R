library(testthat)
library(yummlyr)

context("Search Recipes")

test_that("Search is correct", {
    res <- search_recipes("bacon")
    expect_is(res, "list")
    expect_equal(length(res), 5)
    res <- search_recipes("bacon", allowed_ingredients = "asparagus")
    expect_true(all(unlist(lapply(res$matches$ingredients, function(x) any(sapply(x, grepl, pattern="asparagus"))))))
    expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
    expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
    res <- search_recipes("bacon", excluded_ingredients = "asparagus")
    expect_false(any(unlist(lapply(res$matches$ingredients, function(x) any(sapply(x[[1]], grepl, pattern="asparagus"))))))
    expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
    expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
})
