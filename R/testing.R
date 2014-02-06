.testing.env <- new.env(parent = getNamespace(.this.pkg.name))

## ----------------------------------------------------------------------

.get.param.inputs <- function(param.names = c("port", "dbname"))
{
    testing.vars <- ls(.testing.env)
    if (any(!param.names %in% testing.vars)) {
        cat("\n")
        for (i in param.names)
            if (!i %in% testing.vars) {
                .testing.env[[i]] <- readline(paste(i, "? ", sep = ""))
            }
    }
}

## ----------------------------------------------------------------------

test <- function(filter = NULL,
                 reporter = c("summary", "tap", "minimal", "stop"))
{
    test_path <- system.file("tests", package = .this.pkg.name)
    if (test_path == "")
        stop("You need to use --install-tests option to install ",
             "the tests when you are installing PivotalR!")

    reporter <- match.arg(reporter)

    installed.pkgs <- .get.installed.pkgs()
    if (! "testthat" %in% installed.pkgs) {
        message("Package 'testthat' is going to be installed!")
        install.packages(pkgs = "testthat")
        if (! "testthat" %in% .get.installed.pkgs())
            stop("The package 'testthat' could not be installed!")
    }
    library(testthat)

    ## source(paste(test_path, "/envvars.R", sep = ""))
    ## param.lst <- .get.param.inputs(.tests.need.these)

    reporter <- eval(parse(text = "testthat:::find_reporter(reporter)"))
    ## env <- new.env(parent = getNamespace(package))
    ## for (i in names(param.lst)) env[[i]] <- param.lst[[i]]
    test_dir(test_path, reporter = reporter, env = .testing.env, filter = filter)
    if (reporter$failed) {
        stop("Test failures", call. = FALSE)
    }
    invisible()
}
