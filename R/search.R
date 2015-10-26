#' Search recipes on Yummly.com
#'
#' Query Yummly.com API to search for recipes with certain parameters
#' @param search_words search phrase, can be supplied in from of vector of words
#' @param allowed_ingredient ingredient that all search results must include
#' @param excluded_ingredient ingredient that all search results should not contain
#' @param app_id application ID
#' @param app_key application key
#' @note This function resembles search query to Yummly API
#' @references \itemize{
#'   \item Yummly Developer Guide \url{https://developer.yummly.com/documentation}
#' }
#' @export
search_recipes <- function(search_words, require_pictures,
                           allowed_ingredient, excluded_ingredient,
                           allowed_diet, allowed_allergy,
                           allowed_cuisine, excluded_cuisine,
                           allowed_course, excluded_course,
                           allowed_holiday, excluded_holiday,
                           max_total_time,
                           max_results, start,
                           nutrition, flavor,
                           facet_field,
                           app_id = auth_cache$APP_ID, app_key = auth_cache$APP_KEY) {
    if (!is.list(search_words) && !is.vector(search_words)) {
        stop("Wrong format of search lists, should be either list or vector")
    }
    if (is.null(app_id) || is.null(app_key)) {
        stop("APP_ID or APP_KEY is not set. Use setup_yummly_credentials or supply appropriate arguments")
    }
    # add search words
    search_words <- paste(search_words, collapse = " ")
    query <- sprintf("%s?_app_id=%s&_app_key=%s&q=%s", URL_SEARCH,
                     app_id, app_key, search_words)
    # add pictures requirement
    if (!missing(require_pictures)) {
        if (is.logical(require_pictures)) {
            query <- sprintf("%s&requirePictures=%s", query, tolower(require_pictures[1]))  
        } else {
            warning("require_pictures argument is not logical, it will be discarded")
        }
    }
    # add different parameters
    query <- add_argument(allowed_ingredient, "allowedIngredient", "ingredient", query)
    query <- add_argument(excluded_ingredient, "excludedIngredient", "ingredient", query)
    query <- add_argument(allowed_allergy, "allowedAllergy", "allergy", query)
    query <- add_argument(allowed_diet, "allowedDiet", "diet", query)
    query <- add_argument(allowed_cuisine, "allowedCuisine", "cuisine", query)
    query <- add_argument(excluded_cuisine, "excludedCuisine", "cuisine", query)
    query <- add_argument(allowed_course, "allowedCourse", "course", query)
    query <- add_argument(excluded_course, "excludedCourse", "course", query)
    query <- add_argument(allowed_holiday, "allowedHoliday", "holiday", query)
    query <- add_argument(excluded_holiday, "excludedHoliday", "holiday", query)
    # add maxTotalTime, maxResult, start
    if (!missing(max_total_time) && is.numeric(max_total_time)) {
        query <- paste(query, "&maxTotalTimeInSeconds=", max_total_time[1], sep="")
    } 
    if (!missing(max_results) && is.numeric(max_results)) {
        query <- paste(query, "&maxResult=", max_results[1], sep="")
    }
    if (!missing(start) && is.numeric(start)) {
        query <- paste(query, "&start=", start[1], sep="")
    }
    # add NUTRITION attribute
    if (!missing(nutrition)) {
        nutrition_search_values <- check_arguments(names(nutrition), "nutrition")
        incorrect_value <- which(!sapply(nutrition, function(x) is.numeric(x[[1]])))
        if (length(incorrect_value)) {
            stop(sprintf("For %s nutrition arguments, value parameter is not correct",
                         paste(names(nutrition)[incorrect_value]), collapse = ", "))
        } 
        incorrect_type <- which(!sapply(nutrition, function(x) names(x) %in% c("max", "min")))
        if (length(incorrect_type)) {
            stop(sprintf("For %s nutrition arguments, type parameter is not correct",
                         paste(names(nutrition)[incorrect_type]), collapse = ", "))
        }
        nutrition_argument <- sapply(names(nutrition), 
                                     function(x) {
                                         min <- sprintf("nutrition.%s.%s=%s",
                                                        nutrition_search_values[x],
                                                        "min",
                                                        nutrition[[x]]$min)
                                         max <- sprintf("nutrition.%s.%s=%s",
                                                        nutrition_search_values[x],
                                                        "max",
                                                        nutrition[[x]]$max)
                                         c(min, max)
                                     })
        query <- add_argument(unlist(nutrition_argument), argument_name = "", check = FALSE, query = query)
    }
    # add flavor attribute
    if (!missing(flavor)) {
        flavor_search_values <- check_arguments(names(flavor), "flavor")
        incorrect_value <- which(!sapply(flavor, function(x) is.numeric(x[[1]])))
        if (length(incorrect_value)) {
            stop(sprintf("For %s flavor arguments, value parameter is not correct",
                         paste(names(flavor)[incorrect_value]), collapse = ", "))
        } 
        incorrect_type <- which(!sapply(flavor, function(x) names(x) %in% c("max", "min")))
        if (length(incorrect_type)) {
            stop(sprintf("For %s flavor arguments, type parameter is not correct",
                         paste(names(flavor)[incorrect_type]), collapse = ", "))
        }
        incorrect_value <- which(!sapply(flavor, function(x) unlist(x) >= 0 && unlist(x) <= 1.0))
        if (length(incorrect_value)) {
            stop(sprintf("For %s flavor arguments, value parameter is not correct. It should be between 0 and 1",
                         paste(names(flavor)[incorrect_value]), collapse = ", "))
        }
        flavor_argument <- sapply(names(flavor), 
                                     function(x) {
                                         min <- sprintf("flavor.%s.%s=%s",
                                                        flavor_search_values[x],
                                                        "min",
                                                        flavor[[x]]$min)
                                         max <- sprintf("flavor.%s.%s=%s",
                                                        flavor_search_values[x],
                                                        "max",
                                                        flavor[[x]]$max)
                                         c(min, max)
                                     })
        query <- add_argument(unlist(flavor_argument), argument_name = "", check = FALSE, query = query)
    }
    if (!missing(facet_field)) {
        if (!all(facet_field %in% c("ingredient", "diet"))) {
            stop("Wrong facetField argument. Only diet and ingredient are supported")
        }
        query <- add_argument(facet_field, "facetField", "", query, check = FALSE)
    }
    content <- perform_query(URLencode(query))
    jsonlite::fromJSON(content)
}

add_argument <- function(argument_values, argument_name, type, query, check = TRUE) {
    if (missing(argument_values)) {
        return(query)
    }
    if (check) {
        argument_values <- check_arguments(argument_values, type)
    }
    arg <- prepare_array_parameter(argument_values, argument_name)
    paste(query, arg, sep = "&")       
}

#' Prepare search parameter
#'
#' Prepare search parameter from direction
#' @param param vector parameter to use
#' @param name name for parameter to use
prepare_array_parameter <- function(param, name) {
    if (name != "") {
        name <- paste(name, "[]=", sep="")
    } else {
        name <- paste(name, sep="")
    }
    paste(name, param, sep= "", collapse ="&")
}

#' Check ingredients
#'
#' Check ingredients list against predifined ingredients by Yummly
#' @param ingredients ingridients to check
#' @note Predifined list is downloaded from Metadata Dictionaries
check_arguments <- function(arguments, type) {
    metadata <- metadata[[type]]
    field <- "description"
    available_arguments <- metadata[[field]]
    if (is.null(available_arguments)) {
        field <- "longDescription"
        available_arguments <- metadata[[field]]
    }
    result <- sapply(arguments, function(argument) {
        exact_match <- which(argument == available_arguments)
        possible_matches <- which(grepl(argument, available_arguments))
        if (length(exact_match) || length(possible_matches)) {
            if (length(exact_match)) {
                metadata[exact_match, ]$searchValue
            } else {
                warning(sprintf("Multiple arguments match %s (no exact match found), choosing %s",
                                argument, metadata[possible_matches[1], ][[field]]))
                metadata[possible_matches[1], ]$searchValue
            }
        } else {
            stop(sprintf("%s argument is not found (directly or loosely)", argument))
        }
    })
    result
}