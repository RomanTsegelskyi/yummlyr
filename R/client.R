#' @export
setup_yummly_credentials <- function(app_id, app_key, check_correctness = FALSE) {
    if (app_id != "" && app_key !="") {
        assign("APP_ID", app_id, envir=auth_cache)
        assign("APP_KEY", app_key, envir=auth_cache)
    }
    if (check_correctness) {
        # do some simple request
    }
}