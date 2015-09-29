allowed_metadata <- c("allergy", "diet", "ingredient", "cuisine", "course", "holiday")

#' @export
get_metadata <- function(metadata, app_id = auth_cache$APP_ID, app_key = auth_cache$APP_KEY) {
    if (!tolower(metadata) %in% allowed_metadata) {
        stop(sprintf("Yummly doesn't have any metadata about %s. Allowed metadata: %s",
                     metadata,
                     paste(allowed_metadata, collapse=", ")))
    }
    query <- sprintf("%s/%s?_app_id=%s&_app_key=%s", URL_META, metadata, app_id, app_key)
    parse_jsonp(perform_query(query))
}
