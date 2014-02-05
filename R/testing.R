.get.param.inputs <- function(param.names = c("port", "dbname"))
{
    values <- list()
    cat("\n")
    for (i in param.names)
        values[[i]] <- readline(paste(i, " : ", sep = ""))
    cat("\n")
    values
}

## ----------------------------------------------------------------------

.check.params <- function(param.names, env = environment())
{
    parent.vars <- ls(parent.env(env))
    if (any(! param.names %in% parent.vars))
        stop(paste(param.names, collapse = ", "),
             " must be defined and valid!")
    invisible()
}

## ----------------------------------------------------------------------

test <- function(filter = NULL,
                 reporter = c("summary", "tap", "minimal", "stop"))
{
    installed.pkgs <- .get.installed.pkgs()
    if (! "testthat" %in% installed.pkgs) {
        message("Package 'testthat' is going to be installed!")
        install.packages(pkgs = "testthat")
        if (! "testthat" %in% .get.installed.pkgs())
            stop("The package 'testthat' could not be installed!")
    }
    library(testthat)

    package <- "PivotalR"
    reporter <- match.arg(reporter)

    test_path <- paste(system.file("tests", package = package), "/internal")
    source(paste(test_path, "/envvars.R", sep = ""))
    param.lst <- .get.param.inputs(.tests.need.these)

    reporter <- eval(parse(text = "testthat:::find_reporter(reporter)"))
    env <- new.env(parent = getNamespace(package))
    for (i in names(param.lst)) env[[i]] <- param.lst[[i]]
    test_dir(test_path, reporter = reporter, env = env, filter = filter)
    if (reporter$failed) {
        stop("Test failures", call. = FALSE)
    }
    invisible()
}
