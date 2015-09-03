auth_cache <- new.env(hash=TRUE)

.onLoad <- function(libname, pkgname) {
    # try loading YUMMLY_APP_ID and YUMMLY_APP_KEY from environment variables for testing
    app_id <- Sys.getenv("YUMMLY_APP_ID")
    app_key <- Sys.getenv("YUMMLY_APP_KEY")
    if (app_id != "" && app_key !="") {
        assign("APP_ID", app_id, envir=auth_cache)
        assign("APP_KEY", app_id, envir=auth_cache)
    }
}
