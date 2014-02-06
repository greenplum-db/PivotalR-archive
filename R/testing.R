## The environment that contains all environment variables
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
                 reporter = c("summary", "tap", "minimal", "stop"),
                 env.file = NULL)
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

    reporter <- eval(parse(text = "testthat:::find_reporter(reporter)"))

    if (!is.null(env.file))
        .fill.testing.env(env.file)

    test_dir(test_path, reporter = reporter, env = .testing.env, filter = filter)
    if (reporter$failed) {
        stop("Test failures", call. = FALSE)
    }
    invisible()
}

## ----------------------------------------------------------------------

## Given a file, put all values in the file into testing.env
## Each line of the file should be a key : value pair
.fill.testing.env <- function(env.file)
{
    con <- file(env.file)
    params <- readLines(con)
    close(con)
    for (line in params) {
        words <- .strip(strsplit(line, ":")[[1]])
        assign(words[1], words[2], envir = .testing.env)
    }
}
