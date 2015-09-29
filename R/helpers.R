#' Enable caching of querries
#'
#' This function enable package wide caching using R.cache
#' @export
enable_caching <- function() {
    func_cache$search_recipes <- search_recipes
    mem_call <- R.cache::addMemoization(search_recipes)
    reassign_env("search_recipes", mem_call, as.environment("package:yummlyr"))
    reassign_env("search_recipes", mem_call, getNamespace("yummlyr"))
    if (!is.null(yummlyr_options("log"))) flog.info("Caching is enabled", log=yummlyr_options("log"))
}

#' Disable caching of querries
#'
#' This function disables package wide caching using R.cache
#' @export
disable_caching <- function() {
    reassign_env("search_recipes", func_cache$search_recipes, as.environment("package:yummlyr"))
    reassign_env("search_recipes", func_cache$search_recipes, getNamespace("yummlyr"))
    if (!is.null(yummlyr_options("log"))) flog.info("Caching is disabled", log=yummlyr_options("log"))
}

#' Reassing object in the environment
#'
#' Wrapper to reassign function in namespace that deals with lockedBinding if needed
#' @param name name of an object to be replaced
#' @param obj object that will be put in the environment
#' @param env environment to be replaced in
reassign_env <- function(name, obj, env) {
    if (exists(name, env)) {
        if (bindingIsLocked(name, env)) {
            unlockBinding(name, env)
            assign(name, obj, envir = env)
            lockBinding(name, env)
        } else {
            assign(name, obj, envir = env)
        }
    } 
}

#' Querying/setting yummlyr option
#'
#' To list all \code{yummlyr} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{yummlyr} options are available:
#'
#' \itemize{
#'      \item \code{log}: \code{NULL} or  an optionally passed \emph{logger name} from \pkg{futile.logger} to record all info, trace, debug and error messages.
#'}
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
yummlyr_options <- function(o, value) {
    res <- getOption('yummlyr')
    ## just querying
    if (missing(value)) {
        if (missing(o))
            return(res)
        if (o %in% names(res))
            return(res[[o]])
        cat("Possible `yummlyr` options:")
        print(names(res))
        stop("Wrong option queried.")
    } else {
        if (!o %in% names(res))
            stop(paste("Invalid option name:", o))
        ## fix assigning NULL to a list element
        if (is.null(value)) {
            res[o] <- list(NULL)
        } else {
            res[[o]] <- value
        }
        options("yummlyr" = res)
    }
}

#' Process query
perform_query <- function(query) {
    response <- httr::GET(query)
    response_code <- response$status_code
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
    rawToChar(response$content)
}

#' Parse JSONP returned by Yummly for metadata
#' 
#' This function parses JSONP that yummly uses as a response.
#' It is based on assumption that list of elements is returned.
#' @export
parse_jsonp <- function(jsonp) {
    # remove function name and opening parenthesis
    jsonp <- sub('[^\\[|\\{]*', '', jsonp) 
    # remove closing parenthesis
    jsonp <- sub('\\);*$', '', jsonp)
    jsonlite::fromJSON(jsonp)
}