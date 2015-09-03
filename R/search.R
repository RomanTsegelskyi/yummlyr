
#' @export
search_recipes <- function(word) {
    query <- sprintf("%s?_app_id=%s&_app_key=%s&q=%s", URL_SEARCH,
                     auth_cache$APP_ID, auth_cache$APP_KEY, word)
    x <- httr::GET(query)
    xc <- rawToChar(x$content)
    jsonlite::fromJSON(xc)
}
