## get interactive input for testing environment variables
.get.param.inputs <- function(param.names = c(".port", ".dbname"),
                              reset = FALSE,
                              envir = parent.env(environment()))
{
    if (!all(grepl("^\\.", param.names)))
        stop("Testing environment variable names must start with '.' !")
    testing.vars <- ls(envir, all.names = TRUE)
    if (reset || any(!param.names %in% testing.vars)) {
        cat("\n")
        for (i in param.names)
            if (reset || !i %in% testing.vars) {
                envir[[i]] <- readline(paste(i, "? ", sep = ""))
            }
    }
}

## ----------------------------------------------------------------------

## skip an "expect_that" test if cond is TRUE
skip_if <- function(cond, test.expr)
{
    expr <- deparse(substitute(test.expr), width.cutoff = 500)
    l <- sum(sapply(gregexpr("expect_that\\(", expr), function(s) sum(s>0)))
    if (cond) {
        if (.localVars$test.reporter %in% c("summary", "minimal"))
            for (i in seq_len(l)) cat(testthat::colourise(",", fg = "purple"))
        else if (.localVars$test.reporter == "tap") {
            for (i in seq_len(l)) {
                .localVars$test.skip <- .localVars$test.skip + 1
                cmd <- paste("test_that(\"SKIP this test ", .localVars$test.skip,
                             "\", expect_that(TRUE, is_true()))", sep = "")
                eval(parse(text = cmd))
            }
        }
    } else {
        test.expr
    }
}

## ----------------------------------------------------------------------

test <- function(path = "tests", filter = NULL,
                 reporter = c("summary", "tap", "minimal", "stop"),
                 env.file = NULL, env.vars = list(),
                 clean.test.env = FALSE, run = c("tests", "examples", "both"))
{
    run <- match.arg(run)

    if (path == "tests")
        test_path <- system.file(path, package = .this.pkg.name)
    else
        test_path <- path

    if (test_path == "")
        stop("You need to use --install-tests option to install ",
             "the tests when you are installing PivotalR!")

    ## connections before testing
    ## so that we can close all un-closed connections after testing
    if (identical(.localVars$conn.id, integer(0)))
        origin.conn.id <- integer(0)
    else
        origin.conn.id <- .localVars$conn.id[,1]

    ## close unclosed connection opened during testing
    cleanup.conn <- function() {
        if (identical(.localVars$conn.id, integer(0)))
            curr.conn.id <- integer(0)
        else
            curr.conn.id <- .localVars$conn.id[,1]
        for (i in setdiff(curr.conn.id, origin.conn.id))
            db.disconnect(conn.id = i, verbose = FALSE, force = TRUE)
    }

    installed.pkgs <- .get.installed.pkgs()
    if (! "testthat" %in% installed.pkgs) {
        message("Package 'testthat' is going to be installed!")
        install.packages(pkgs = "testthat")
        if (! "testthat" %in% .get.installed.pkgs())
            stop("The package 'testthat' could not be installed!")
    }
    library(testthat)

    reporter <- match.arg(reporter)
    .localVars$test.reporter <- reporter
    .localVars$test.skip <- 0
    reporter <- eval(parse(text = "testthat:::find_reporter(reporter)"))

    if (!is.null(env.file))
        .fill.testing.env(env.file)
    else if (length(env.vars) > 0)
        for (var in names(env.vars))
            assign(var, env.vars[[var]], envir = .testing.env)

    if (clean.test.env)
        do.call(rm, c(ls(.testing.env, all.names = TRUE),
                      envir = .testing.env))

    if (run == "examples" || run == "both") {
        cat(testthat::colourise("Running examples in the user doc -----\n",
                                fg = "light blue"))
        tryCatch(.run.doc.example(reporter, filter),
                 interrupt = function(cond) {
                     cleanup.conn()
                     unlink(.localVars$example.tmppath, recursive = TRUE)
                     rm(example.tmppath, envir = .localVars)
                 })
        cleanup.conn()
        unlink(.localVars$example.tmppath, recursive = TRUE)
        rm(example.tmppath, envir = .localVars)
    }

    if (run == "tests" || run == "both") {
        cat(testthat::colourise("Running tests ------------------------\n",
                                fg = "light blue"))
        tryCatch(testthat::test_dir(test_path, reporter = reporter,
                                    env = .testing.env, filter = filter),
                 interrupt = function(cond) cleanup.conn())
        cleanup.conn()
    }

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
        if (grepl("^\\s*#", line)) next # allow '#' to start a comment
        words <- .strip(strsplit(line, ":")[[1]])
        if (length(words) >= 2) {
            key <- .strip(strsplit(words[1], "#")[[1]][1])
            value <- .strip(strsplit(words[2], "#")[[1]][1])
            assign(key, value, envir = .testing.env)
        }
    }
}

## ----------------------------------------------------------------------

## Used for tests, no error during execution
has_no_error <- function ()
{
    function(expr) {
        res <- try(force(expr), TRUE)
        has_error <- inherits(res, "try-error")
        if (has_error) {
            return (eval(parse(
                text = paste("expectation(FALSE,",
                paste("'code generated an error: ",
                      as.character(res), "'", sep = "")))))
        }
        eval(parse(text = "expectation(TRUE, '')"))
    }
}

## ----------------------------------------------------------------------

## run doc example
.run.doc.example <- function(reporter, filter, r_root = NULL,
                             envir = .testing.env)
{
    library(tools)
    if (is.null(r_root))
        x <- tools::Rd_db(.this.pkg.name)
    else
        x <- tools::Rd_db(dir = r_root)

    outpath <- paste("/tmp/", .unique.string(), "/", sep = "")
    dir.create(outpath, recursive = TRUE)
    .localVars$example.tmppath <- outpath

    ## loop over all Rd files
    for (i in seq_along(x)) {
        filename <- gsub(".*/man/(.*\\.Rd)", "\\1", names(x)[i])
        y <- eval(parse(text = as.character(x[i])))
        for (j in seq_along(y)) {
            z <- unlist(y[[j]])
            pa <- grepl("%%\\s+@test", z)
            if (sum(pa) == 0) next # not an example to run
            ## params needed
            params <- gsub("%%\\s+@test\\s+(\\S+)\\s+.*", "\\1", z[pa])

            outfile <- paste(outpath, "test-", filename, ".r", sep = "")
            con <- file(outfile, "w")
            cat("context('Doc example in ", filename, "')\n",
                sep = "", file = con)
            cat(".get.param.inputs(c(",
                paste("'", params, "'", collapse = ",", sep = ""),
                "))\n\n", sep = "", file = con)

            cat("test_that('Example in ", filename, "', {expect_that({",
                sep = "", file = con)
            for (line in z[!pa]) cat(line, file = con)
            cat("}, has_no_error())})", file = con)
            close(con)
        }
    }

    testthat::test_dir(outpath, reporter = reporter,
                       env = envir, filter = filter)

    invisible()
}

## ----------------------------------------------------------------------

## Continuous testing
continuous.test <- function(root, filter = NULL,
                            reporter = c("summary", "tap", "minimal", "stop"),
                            env.file = NULL, env.vars = list(),
                            clean.test.env = FALSE,
                            run = c("tests", "examples", "both"))
{
    root <- normalizePath(root) # package directory
    r_dir <- paste(root, "/R/", sep = "") # R code folder
    tests_dir <- paste(root, "/tests/", sep = "") # tests folder
    src_dir <- paste(root, "/src/", sep = "") # C/C++ source code folder
    man_dir <- paste(root, "/man/", sep = "") # manual examples folder
    des_file <- paste(root, "/DESCRIPTION", sep = "")

    ## connections before testing
    ## so that we can close all un-closed connections after testing
    if (identical(.localVars$conn.id, integer(0)))
        origin.conn.id <- integer(0)
    else
        origin.conn.id <- .localVars$conn.id[,1]

    ## close unclosed connection opened during testing
    cleanup.conn <- function() {
        if (identical(.localVars$conn.id, integer(0)))
            curr.conn.id <- integer(0)
        else
            curr.conn.id <- .localVars$conn.id[,1]
        for (i in setdiff(curr.conn.id, origin.conn.id))
            db.disconnect(conn.id = i, verbose = FALSE, force = TRUE)
    }

    installed.pkgs <- .get.installed.pkgs()
    if (! "testthat" %in% installed.pkgs) {
        message("Package 'testthat' is going to be installed!")
        install.packages(pkgs = "testthat")
        if (! "testthat" %in% .get.installed.pkgs())
            stop("The package 'testthat' could not be installed!")
    }
    library(testthat)

    reporter <- match.arg(reporter)
    reporter <- eval(parse(text = "testthat:::find_reporter(reporter)"))

    if (!is.null(env.file))
        .fill.testing.env(env.file)
    else if (length(env.vars) > 0)
        for (var in names(env.vars))
            assign(var, env.vars[[var]], envir = .continuous.env)

    if (clean.test.env)
        do.call(rm, c(ls(.continuous.env, all.names = TRUE),
                      envir = .continuous.env))
    else
        do.call(rm, c(ls(.continuous.env, all.names = FALSE),
                      envir = .continuous.env))

    is.first.run <- TRUE # first time to run the tests

    exclude <- c("onAttach.R", "testing.R")
    not.copy <- "onAttach.R"

    ans <- "no-choice"

    tryCatch(repeat {
        ## test_path
        test_r_path <- paste("/tmp/", .unique.string(), "/", sep = "")
        test_tests_path <- paste("/tmp/", .unique.string(), "/", sep = "")
        test_man_path <- paste("/tmp/", .unique.string(), "/", sep = "")
        dir.create(test_r_path, recursive = TRUE)
        dir.create(test_tests_path, recursive = TRUE)
        dir.create(test_man_path, recursive = TRUE)

        if (is.first.run || ans == "all") {
            file.copy(r_dir, test_r_path, recursive = TRUE)
            do.call(file.remove, as.list(paste(test_r_path, "R/", not.copy,
                                               sep = "")))
            file.copy(tests_dir, test_tests_path, recursive = TRUE)
            file.copy(man_dir, test_man_path, recursive = TRUE)
            file.copy(des_file, test_man_path)

            old.r.state <- .dir_state(r_dir)
            old.tests.state <- .dir_state(tests_dir)
            old.man.state <- .dir_state(man_dir)
            old.src.state <- .dir_state(src_dir)

            l.tests <- 1
            l.man <- 1
        } else {
            new.r.state <- .dir_state(r_dir)
            new.tests.state <- .dir_state(tests_dir)
            new.man.state <- .dir_state(man_dir)
            new.src.state <- .dir_state(src_dir)

            r.diff <- .compare_state(old.r.state, new.r.state)
            tests.diff <- .compare_state(old.tests.state, new.tests.state)
            man.diff <- .compare_state(old.man.state, new.man.state)
            src.diff <- .compare_state(old.src.state, new.src.state)

            old.r.state <- new.r.state
            old.tests.state <- new.tests.state
            old.man.state <- new.man.state
            old.src.state <- new.src.state

            ## Nothing has been changed
            if (r.diff$n == 0 && tests.diff$n == 0 && man.diff$n == 0 &&
                src.diff$n == 0) {
                unlink(c(test_r_path, test_tests_path, test_man_path),
                       recursive = TRUE, force = TRUE)
                cat(testthat::colourise("Nothing has been changed. No test will be run\n\n", fg = "light red"))
                ans <- .get.continue.choice()
                next
            }

            ## If some files changed, we need to stop
            if (any(exclude %in% r.diff$added) ||
                any(exclude %in% r.diff$deleted) ||
                any(exclude %in% r.diff$modified) ||
                src.diff$n > 0) {
                message("Some of onAttach.R, testing.R or C/C++ source code ",
                        "are changed, and you need to re-build ",
                        "and re-install the package.")
                unlink(c(test_r_path, test_tests_path, test_man_path),
                       recursive = TRUE, force = TRUE)
                break
            }

            if (r.diff$n > 0) {
                ## any R source changed, re-run all tests
                file.copy(r_dir, test_r_path, recursive = TRUE)
                do.call(file.remove, as.list(paste(test_r_path, "R/", exclude,
                                                   sep = "")))
                file.copy(tests_dir, test_r_path, recursive = TRUE)
                file.copy(man_dir, test_man_path, recursive = TRUE)
                file.copy(des_file, test_man_path)

                l.tests <- 1
                l.man <- 1
            } else {
                ## only some man or test files changed
                file.copy(r_dir, test_r_path, recursive = TRUE)
                do.call(file.remove, as.list(paste(test_r_path, "R/", exclude,
                                                   sep = "")))
                tests.changed <- c(tests.diff$added, tests$modified)
                l.tests <- length(tests.changed)
                if (l.tests > 0) {
                    dir.create(paste(test_tests_path, "tests/", sep = ""))
                    file.copy(tests.changed, paste(test_tests_path,
                                                   "tests/", sep = ""))
                }

                man.changed <- c(man.diff$added, man.diff$modified)
                l.man <- length(man.changed)
                if (l.man > 0) {
                    dir.create(paste(test_man_path, "man/", sep = ""))
                    file.copy(man.changed, paste(test_man_path,
                                                 "man/", sep = ""))
                    file.copy(des_file, test_man_path)
                }
            }

        }

        .source_dir(paste(test_r_path, "R/", sep = ""))

        if (run == "examples" || run == "both") {
            cat(testthat::colourise("Running examples in the user doc -----\n",
                                    fg = "light blue"))
            if (l.man > 0) {
                .run.doc.example(reporter, filter, test_man_path,
                                 .continuous.env)
                cleanup.conn()
            } else
                message("No manual file has been added or modified. ",
                        "No example to run!")
        }

        if (run == "tests" || run == "both") {
            cat(testthat::colourise("Running tests ------------------------\n",
                                    fg = "light blue"))
            if (l.tests > 0) {
                testthat::test_dir(paste(test_tests_path, "tests/", sep = ""),
                                   reporter = reporter,
                                   env = .continuous.env, filter = filter)
                cleanup.conn()
            } else
                message("No test file has been added or modified. ",
                        "No tests to run!")
        }

        unlink(c(test_r_path, test_tests_path, test_man_path),
               recursive = TRUE, force = TRUE)

        ans <- .get.continue.choice()

    }, interrupt = function(cond) {
        cleanup.conn()
        unlink(c(test_r_path, test_tests_path, test_man_path),
               recursive = TRUE, force = TRUE)
    })
}

## ----------------------------------------------------------------------

.get.continue.choice <- function()
{
    ans <- readline(testthat::colourise(
        "Want to re-run the tests (Yes/All/Stop or No) ?",
        fg = "red"))
    match.arg(tolower(ans), c('yes', 'all', 'stop', 'no'))
}

## ----------------------------------------------------------------------

## check directory state
.dir_state <- function(path)
{
    files <- dir(path, NULL, full.names = TRUE, recursive = TRUE)
    sapply(files, digest::digest, file = TRUE)
}

## ----------------------------------------------------------------------

## source files
.source_dir <- function (path)
{
    pattern = "\\.[rR]$"
    files <- sort(dir(path, pattern, full.names = TRUE, recursive = TRUE))
    lapply(files, sys.source, chdir = TRUE, envir = .continuous.env)
}

## ----------------------------------------------------------------------

## compare the states of one directory
.compare_state <- function (old, new)
{
    added <- setdiff(names(new), names(old))
    deleted <- setdiff(names(old), names(new))
    same <- intersect(names(old), names(new))
    modified <- names(new[same])[new[same] != old[same]]
    n <- length(added) + length(deleted) + length(modified)
    list(n = n, added = added, deleted = deleted, modified = modified)
}
