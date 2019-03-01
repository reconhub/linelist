
withWarnings <- function(expr) {
  myWarnings <- NULL
  myErrors   <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  eHandler <- function(e) {
    myErrors <<- c(myErrors, list(e))
    NULL
  }
  val <- withCallingHandlers(tryCatch(expr, error = eHandler), warning = wHandler)
  list(value = val, warnings = myWarnings, errors = myErrors)
}

collect_ya_errs <- function(e, fmt) {
  if (is.null(e)) return(NULL)
  warn <- vapply(e, "[[", character(1), "message")
  warn <- paste0(" ", warn, collapse = "\n  ....")
  paste(sprintf("  %s__:\n  ....%s", fmt, warn), collapse = "\n")
}

process_werrors <- function(warns, errs) {
  warns <- warns[lengths(warns) > 0]
  errs  <- errs[lengths(errs) > 0]
  warned <- length(warns) > 0
  errored <- length(errs) > 0
  if (warned || errored) {
  
    wrn <- "" -> err
    if (warned) {
      wngs <- do.call("paste", c(warns, sep = "\n"))
      wrn <- sprintf("The following warnings were found...\n%s", wngs)
    }
    if (errored) {
      errs <- do.call("paste", c(errs, sep = "\n"))
      err  <- sprintf("The following errors were found:\n%s", errs)
    }
    res <- sprintf("%s\n%s", wrn, err)
    if (res == "\n") NULL else res
  } else {
    NULL
  }
}
