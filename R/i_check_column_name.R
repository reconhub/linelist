i_check_column_name <- function(x, the_columns) {
  # Checks that it's a number within the number of columns
  is_number <- is.numeric(x) &&       # x is a number
    as.integer(x) == x       &&       # ... and an integer
    x <= length(the_columns) && x > 0 # ... and is within the bounds

  # check that it's a character column that matches a column names
  is_name   <- is.character(x) && # x is a name
    any(the_columns == x)         # ... in the the_columns

  # Return TRUE if it's a feasable column
  is_number || is_name
}

i_check_scalar <- function(x) {

  # Checks that the input is a single value
  is_scalar <- !is.null(x) && length(x) == 1

  is_scalar

}
