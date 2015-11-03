library(testthat)
library(yummlyr)

context("Get Recipes")

test_that("Get with actual http requests", {
    if (Sys.getenv("TRAVIS") != "" || Sys.getenv("APPVEYOR") != "") {
        result <- get_recipe("French-Onion-Soup-The-Pioneer-Woman-Cooks-_-Ree-Drummond-41364")
        expect_is(result, "list")
        expect_equal(length(result), 14)
    }
})


test_that("Get (with mocks)", {
    with_mock(
        `yummlyr::perform_query` = function(query) query,
        `jsonlite::fromJSON` = function(content) content,
        result <- get_recipe("French-Onion-Soup-The-Pioneer-Woman-Cooks-_-Ree-Drummond-41364"),
        expect_true(grepl("http://api.yummly.com/v1/api/recipe/French-Onion-Soup-The-Pioneer-Woman-Cooks-_-Ree-Drummond-41364", result)),
        expect_error(get_recipe("aaa", NULL, NULL))
    )
})
