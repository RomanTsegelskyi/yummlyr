library(testthat)
library(yummlyr)

context("Search Recipes")

test_that("Search with actual http requests", {
#     result <- search_recipes("bacon")
#     expect_is(result, "list")
#     expect_equal(length(result), 5)
#     result <- search_recipes("bacon", allowed_ingredients = "asparagus")
#     expect_true(all(unlist(lapply(result$matches$ingredients, function(x) any(sapply(x, grepl, pattern="asparagus"))))))
#     expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
#     expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
#     result <- search_recipes("bacon", excluded_ingredients = "asparagus")
#     expect_false(any(unlist(lapply(result$matches$ingredients, function(x) any(sapply(x[[1]], grepl, pattern="asparagus"))))))
#     expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2"))
#     expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
})


test_that("Search. Words work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon"),
        expect_true(grepl("q=bacon$", result)),
        result <- search_recipes(c("onion", "bacon")),
        expect_true(grepl("q=onion%20bacon$", result))
    )
})

test_that("Search. RequirePicturesult work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon", require_picture = TRUE),
        expect_true(grepl("q=bacon&requirePictures=true$", result))
    )
})

test_that("Search. Included ingredients work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon", allowed_ingredients="garlic"),
        expect_true(grepl("allowedIngredient\\[\\]=garlic$", result)),
        result <- search_recipes("bacon", allowed_ingredients=c("garlic", "asparagus")),
        expect_true(grepl("allowedIngredient\\[\\]=garlic&allowedIngredient\\[\\]=asparagus", result)),
        expect_error(search_recipes("bacon", allowed_ingredients = "asparagus2")),
        expect_warning(search_recipes("bacon", allowed_ingredients = "fried"))
    )
})

test_that("Search. Excluded ingredients work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon", excluded_ingredients = c("garlic","asparagus")),
        expect_true(grepl("excludedIngredient\\[\\]=garlic&excludedIngredient\\[\\]=asparagus$", result)),
        expect_error(search_recipes("bacon", excluded_ingredients = "asparagus2")),
        expect_warning(search_recipes("bacon", excluded_ingredients = "fried")),
        result <- search_recipes("bacon", excluded_ingredients=c("onion soup mix", "asparagus")),
        expect_true(grepl("excludedIngredient\\[\\]=onion%20soup%20mix&excludedIngredient\\[\\]=asparagus$", result))
    )
})

test_that("Search. Allergy work (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon", allowed_allergy =c("Dairy-Free", "Gluten-Free")),
        expect_true(grepl("allowedAllergy\\[\\]=396%5EDairy-Free&allowedAllergy\\[\\]=393%5EGluten-Free$", result)),
        expect_error(search_recipes("bacon", allowed_allergy = "Dairy2")),
        expect_warning(search_recipes("bacon", allowed_allergy = "Dairy"))
    )
})

test_that("Search. allowedDiet works (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- search_recipes("bacon", allowed_diet =c("Pescetarian", "Lacto vegetarian")),
        expect_true(grepl("allowedDiet\\[\\]=390%5EPescetarian&allowedDiet\\[\\]=388%5ELacto%20vegetarian$", result)),
        expect_error(search_recipes("bacon", allowed_diet = "Lacto2")),
        expect_warning(search_recipes("bacon", allowed_diet = "Lacto"))
    )
})
