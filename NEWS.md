# linelist 0.0.40.9000


* Both `clean_variable_spelling()` and `clean_spelling()` have been migrated over
  to the {matchmaker} package and arguments from the aformentioned functions are
  passed to the {matchmaker} functions. Tests and documentation have been
  updated to reflect this.
* Remove {rlang} from imports (but is still imported by {matchmaker}).
* `guess_dates()` will now return entirely missing vectors unchanged instead of
  throwing an error. This fixes #108 via #109.

# linelist 0.0.39.9000

* `clean_variable_spelling()` and `clean_spelling()` gain the option to specify
  which columns contain the keys (`from`) and values (`to`). These default to
  1 and 2, respectively, which ensure that backwards compatibility is retained.
  (this fixes #99).
* `linelist_example()` is a new function that serves as an alias for 
  `system.file("extdata", thing, package = "linelist")`, which is much easier
  for new R users to understand.

# linelist 0.0.38.9000

* `top_values()` no longer throws a spurious warning when the levels in the 
  subset data are identical to the levels in the full data (#96)

# linelist 0.0.37.9000

* `top_values()` gains a new `subset` argument that allows the user to retain
  the top levels of a subset of a vector. This is particularly useful for
  retrospective analysis based on current trends (fixes #92 via #94 and #95, 
  @thibautjombart)

# linelist 0.0.36.9000

* `top_values()` gains the explicit ties.method parameter, which defaults to 
  "first" to fix issue #88 (thanks to @cwhittaker1000 for spotting the issue
  and providing a detailed explanation).
* `top_values()` issues a warning if one of the top values had a tied value
   that was not included. 
* `top_values()` issues a warning if the user uses a ties.method that is not
   guaranteed to return exactly n top values.

# linelist 0.0.35.9000

* `clean_spelling()` gains the `anchor_regex` argument, which will wrap all
  regex keyword entries in "^" and "$" before processing. 

# linelist 0.0.34.9000

## BREAKING CHANGES

* The linelist class and all associated epivars/dictionary functions have been
  removed as out of scope of this package. Without any validation, these 
  functions were no more than a fancy wrapper to `dplyr::rename()`, thus they
  are being removed after fda9e18b02f5853cd311ddcc513c427244b21dd7. If the
  linelist class is ressurrected, (e.g. to implement a hxl validator package),
  it can be taken from that commit. This is related to #29

* `clean_spelling()` now gains the `.regex ` keyword that allows the user to
  supply perl-style regular expressions to change words that may have similar
  spelling.

# linelist 0.0.33.9000

* `guess_dates()` now processes at double the speed of the previous version.
* `guess_dates()` will now properly constrain date vectors to the start and end
  dates. 
* `guess_dates()` correctly parses dates represented as integers from excel
  (#73).

# linelist 0.0.32.9000

* `print.data_comparison()` now sets `diff_only = TRUE` by default (#71)
 
# linelist 0.0.31.9000

* `compare_data()` gains the option `columns`, which allows users to 
  choose which columns they want to compare. Defaults to `TRUE`, which
  compares all columns (#58).

# linelist 0.0.30.9000

* `guess_dates()` can now handle dates that were imported from Excel as 
  integers (#66).
* `guess_dates()` gains the argument "modern_excel" to indicate how integer
  dates should be formatted.
* `getOption("linelist_guess_orders")` replaces the explicit list of orders in
  `guess_dates()` for easier access.
* `guess_dates()` no longer throws an error if passed a date class object (#65).
* `guess_dates()` has been better documented to reflect the above changes (#64).

# linelist 0.0.29.9000

* `clean_spelling()` gains a new keyword: `.na` (or should I say "valueword").
  When this keyword is in the values (second) column of the wordlist, the keys
  will be replaced with a missing (`<NA>`) value. This is useful for contrasting
  between presence of an absence and an absence of a presence with the `.missing`
  keyword. See #55 and #57 for details

# linelist 0.0.28.9000

* `print.data_comparison()` gains the logical arguments `common_values` and
  `diff_only` to control the length of print output (See #61).

# linelist 0.0.27.9000

* `compare_data()` now correctly accounts for different values in variables.
  Thanks to @ffinger for finding the bug (#56).
* pre-release in-development numbering scheme updated to only increment the
  patch version to indicate the ongoing WIP. Release to CRAN will shift to 0.1.0 

# linelist 0.0.26.9000

* `compare_data()` now returns list of variable classes instead of TRUE if the
  classes match. (See #53 for details).

# linelist 0.0.25.9000

* `clean_variable_spelling()` will now run global variables before processing
  named variables instead of in tandem. This allows the user to define
  misspellings in the `.global` variable.
  See https://github.com/reconhub/linelist/issues/51 for details.

# linelist 0.0.24.9000

* `clean_spelling()` will no longer throw a warning if there is no value for
  .default to replace.
* `clean_variable_spelling()`, `clean_variables()`, and `clean_data()` gain the
  `warn` and `warn_spelling` arguments which will capture all errors and
  warnings issued from `clean_spelling()` for each variable. 
  See https://github.com/reconhub/linelist/pull/48 for details).  

# linelist 0.0.23.9000

* `compare_data()` allows users to compare _structural changes_ to data frames
  This includes, names, classes, dimensions, and values in matching categorical
  variables. (See https://github.com/reconhub/linelist/pull/50 for details).
* `top_values()` will mask all but the top `n` values in a factor.
* the `crayon` package is added to imports

# linelist 0.0.22.9000

* `clean_spelling()` wordlists now allow the optional `.missing` keyword to
  replace both `NA` and blank ("") cells in the data. Values that are `NA` will
  be converted to "NA" (character) with a warning. 
  See https://github.com/reconhub/linelist/pull/44 and 
  https://github.com/reconhub/linelist/pull/45 for details.

# linelist 0.0.21.9000

* `guess_dates()` can once again parse date formats that are file names: 
  `example_format_2019-02-19.xlsx`. (See #43 for details)

# linelist 0.0.20.9000

* `clean_spelling()` gains a `quiet` argument to suppress warnings.
* `clean_variable_spelling()` will no longer error if there are variable 
  specifications that don't exist in the data. It will also suppress all 
  warnings from `clean_spelling()`. (see #41 for details)

# linelist 0.0.19.9000

* `clean_spelling()` will check the spelling of a vector against a wordlist
* `clean_variable_spelling()` will apply `clean_spelling()` to all specified
  columns in a data frame
* `clean_variables()` wraps `clean_variable_labels()` and `clean_variable_spelling()`
* `clean_data()` now can optionally check labels againt a wordlist.

(see #38 for details)

# linelist 0.0.18.9000

* `mask()` will temporarily replace column names with epivars
* `unmask()` reverses the effect of mask.
* New Imports: tidyselect and purrr
  (see #37 for details)

# linelist 0.0.17.9000

* `geo` epivar was replaced with `geo_lat` and `geo_lon` (see #35)

# linelist 0.0.16.9000

* add optional constraints for what columns can be manipulated and make 
  clean_data() faster (see #32)

# linelist 0.0.15.9000

* use lubridate package to parse dates (see #30)

# linelist 0.0.14.9000

* `lookup()` function can look up the column name corresponding to an epivar
   (see #28)

# linelist 0.0.13.9000

* `add_epivars()` adds epivars to the global dictionary
* `add_description()` updates the description of one of the epivars
  (see #26)

# linelist 0.0.12.9000

* add `template_linelist()` function (see #24)

# linelist 0.0.11.9000

* add rio to imports (see #23)

# linelist 0.0.10.9000

* rename all_dictionary argument to full_dict (see #22)

# linelist 0.0.9.9000

* re-instate validator of dots (see #21)

# linelist 0.0.8.9000

* re-instate data validation (see #20)

# linelist 0.0.7.9000

* restructure linelist class to make dictionary global (see #19)

# linelist 0.0.6.9000

* dictionary validation and tibble import (see #17)

# linelist 0.0.5.9000

* new functions to handle epivars (see #16)

# linelist 0.0.4.9000

* `get_vars()` can take multiple variables (see #15)

# linelist 0.0.3.9000

* adds linelist class (see #9)

# linelist 0.0.2.9000

* `guess_dates()` now throws an appropriate error if a vector is passed instead
  of a data frame. See https://github.com/reconhub/linelist/issues/4 for details

# linelist 0.0.1.9000

* Added a `NEWS.md` file to track changes to the package.


