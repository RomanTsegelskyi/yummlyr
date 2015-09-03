URL_BASE <- 'http://api.yummly.com/v1/api'
URL_GET <- paste(URL_BASE, 'recipe/', sep = "/")
URL_SEARCH <- paste(URL_BASE, 'recipes', sep = "/")
URL_META <- paste(URL_BASE, 'metadata', sep = "/")


search <- function(word) {
    query <- sprintf("%s?_app_id=%s&_app_key=%s&q=%s", URL_SEARCH, APP_ID, APP_KEY, word)
    x <- httr::GET(query)
    xc <- rawToChar(x)
    hash <- jsonlite::fromJSON(xc)
}
