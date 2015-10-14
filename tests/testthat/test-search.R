library(testthat)
library(yummlyr)

context("Search Recipes")

test_that("Search with actual http requests", {
#     res <- search_recipes("bacon")
#     expect_is(res, "list")
#     expect_equal(length(res), 5)
#     res <- search_recipes("bacon", allowed_ingredients = "asparagus")
#     expect_true(all(unlist(lapply(res$matches$ingredients, function(x) any(sapply(x, grepl, pattern="asparagus"))))))
#     expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
#     expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
#     res <- search_recipes("bacon", excluded_ingredients = "asparagus")
#     expect_false(any(unlist(lapply(res$matches$ingredients, function(x) any(sapply(x[[1]], grepl, pattern="asparagus"))))))
#     expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
#     expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
})


test_that("Search. Words work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        expect_equal(class(res <- search_recipes("bacon")), "character"),
        expect_true(grepl("q=bacon$", res)),
        expect_equal(class(res <- search_recipes(c("onion", "bacon"))), "character"),
        expect_true(grepl("q=onion%20bacon$", res))
    )
})

test_that("Search. RequirePictures work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        expect_equal(class(res <- search_recipes("bacon", require_picture = TRUE)), "character"),
        expect_true(grepl("q=bacon&requirePictures=true$", res))
    )
})

test_that("Search. Included ingredients work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        expect_equal(class(res <- search_recipes("bacon", allowed_ingredients="garlic")), "character"),
        expect_true(grepl("allowedIngredient\\[\\]=garlic$", res)),
        expect_equal(class(res <- search_recipes("bacon", allowed_ingredients=c("garlic", "asparagus"))), "character"),
        expect_true(grepl("allowedIngredient\\[\\]=garlic&allowedIngredient\\[\\]=asparagus", res)),
        expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2")),
        expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
    )
})

test_that("Search. Excluded ingredients work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        res <- search_recipes("bacon", excluded_ingredients = c("garlic","asparagus")),
        expect_true(grepl("excludedIngredient\\[\\]=garlic&excludedIngredient\\[\\]=asparagus$", res)),
        expect_error(search_recipes("bacon", excluded_ingredients = "asparagus2")),
        expect_warning(search_recipes("bacon", excluded_ingredients = "fried")),
        expect_equal(class(res <- search_recipes("bacon", excluded_ingredients=c("onion soup mix", "asparagus"))), "character"),
        expect_true(grepl("excludedIngredient\\[\\]=onion%20soup%20mix&excludedIngredient\\[\\]=asparagus$", res))
    )
})

test_that("Search. Allergy work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        expect_equal(class(res <- search_recipes("bacon", allowed_allergy =c("Dairy-Free", "Gluten-Free"))), "character"),
        expect_true(grepl("allowedAllergy\\[\\]=396%5EDairy-Free&allowedAllergy\\[\\]=393%5EGluten-Free$", res)),
        expect_error(search_recipes("bacon", allowed_allergy = "Dairy2")),
        expect_warning(search_recipes("bacon", allowed_allergy = "Dairy"))
    )
})

