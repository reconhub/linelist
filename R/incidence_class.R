# It should also have accessor functions for standard columns:
#   
#  - date of onset
#  - date of report
#  - date of x where x in {death, hospitalization, ...}
#  - sex
#  - age
#  - age group
#  - a function that determines which concepts are available
#  - arbitrary query function based on HXL?

# Convert a data frame to a linelist via as_linelist

as_linelist <- function(dat, ...) {
  UseMethod("as_linelist")
}

as_linelist.default <- function(dat, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(dat), collapse = ", ")))
}

as_linelist.data.frame <- function(dat, ...) {
  dots <- list(...)
  attr(dat, "epivars") <- dots
  class(dat) <- c("linelist", oldClass(dat))
  dat
}