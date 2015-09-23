
#' @export
enable_caching <- function() {
    func_cache$search_recipes <- search_recipes
    mem_call <- R.cache::addMemoization(search_recipes)
    reassignInEnv("search_recipes", mem_call, as.environment("package:yummlyr"))
    reassignInEnv("search_recipes", mem_call, getNamespace("yummlyr"))
}

#' @export
disable_caching <- function() {
    reassignInEnv("search_recipes", func_cache$search_recipes, as.environment("package:yummlyr"))
    reassignInEnv("search_recipes", func_cache$search_recipes, getNamespace("yummlyr"))
}

reassignInEnv <- function(name, obj, env) {
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
