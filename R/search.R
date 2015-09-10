#' Search recipes on Yummly.com
#'
#' Query Yummly.com API to search for recipes with certain parameters
#' @param search_words search phrase, can be supplied in from of vector of words
#' @param allowed_ingredients ingredient that all search results must include
#' @param excluded_ingredients ingredient that all search results should not contain
#' @param app_id application ID
#' @param app_key application key
#' @note This function resembles search query to Yummly API
#' @references \itemize{
#'   \item Yummly Developer Guide \url{https://developer.yummly.com/documentation}
#' }
#' @export
search_recipes <- function(search_words, allowed_ingredients, excluded_ingredients,
                           app_id = auth_cache$APP_ID, app_key = auth_cache$APP_KEY) {
    if (!is.list(search_words) && !is.vector(search_words)) {
        stop("Wrong format of search lists, should be either list or vector")
    }
    search_words <- paste(search_words, collapse = "+")
    query <- sprintf("%s?_app_id=%s&_app_key=%s&q=%s", URL_SEARCH,
                     app_id, app_key, search_words)
    if (!missing(allowed_ingredients)) {
        check_ingredients(allowed_ingredients)
        allowed_ingredients <- prepare_array_parameter(allowed_ingredients, "allowedIngredient")
        query <- paste(query, "&", allowed_ingredients, sep = "")
    }
    if (!missing(excluded_ingredients)) {
        check_ingredients(excluded_ingredients)
        excluded_ingredients <- prepare_array_parameter(excluded_ingredients, "excludedIngredient")
        query <- paste(query, "&", excluded_ingredients, sep = "")        
    }
    if (is.null(app_id) || is.null(app_key)) {
        stop("APP_ID or APP_KEY is not set. Use setup_yummly_credentials or supply appropriate arguments")
    }
    
    response <- httr::GET(query)
    response_code <- response$status_code
    response_content <- rawToChar(response$content)
    if (response_code == 409) {
        error_massage <- ifelse(grepl("Permission denied", response_content),
                                "Wrong credentials", "API Rate Limit Exceeded")
        stop(error_massage)
    } else if (response_code == 500) {
        stop("Request returned with Internal Server Error, please try again later")
    } else if (response_code == 400) {
        stop("Request is not formatted correctly, please report this error to the developers")
    } else if (response_code != 200) {
        stop(sprintf("Request returned with the following error %s", response_content))
    }
    jsonlite::fromJSON(response_content)
}

#' Prepare search parameter
#'
#' Prepare search parameter from direction
#' @param param vector parameter to use
#' @param name name for parameter to use
#' @export
prepare_array_parameter <- function(param, name) {
    param <- sapply(param, gsub, pattern=" ", replacement="+")
    name <- paste(name, "[]", sep="")
    paste(name, param, sep= "=", collapse ="&")
}

#' Check ingredients
#'
#' Check ingredients list against predifined ingredients by Yummly
#' @param ingredients ingridients to check
#' @note Predifined list is downloaded from Metadata Dictionaries
#' @export
check_ingredients <- function(ingredients) {
    result <- sapply(ingredients, function(ingredient) {
        exact_match <- which(ingredient == available_ingredients[,1])
        possible_matches <- which(grepl(ingredient, available_ingredients[,1]))
        if (length(exact_match) || length(possible_matches)) {
            if (length(exact_match)) {
                ingredient
            } else {
                warning(sprintf("Multiple ingredients match %s (no exact match found), choosing %s",
                        ingredient, available_ingredients[,1][possible_matches[1]]))
                available_ingredients[,1][possible_matches[1]]
            }
        } else {
            stop(sprintf("%s ingredient is not found (directly or loosely)", ingredient))
        }
    })
}