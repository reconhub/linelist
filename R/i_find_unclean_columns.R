i_find_unclean_columns <- function(dat) {
  !vapply(dat, i_think_this_is_clean, logical(1))
}

i_think_this_is_clean <- function(i) {
  "<linelist>clean" %in% comment(i)
}
