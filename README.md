
# Welcome to the *linelist* package\!

[![Travis build
status](https://travis-ci.org/reconhub/linelist.svg?branch=master)](https://travis-ci.org/reconhub/linelist)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/linelist/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/linelist?branch=master)

This package is dedicated to simplifying the cleaning and
standardisation of linelist data. Considering a case linelist
`data.frame`, it aims to:

  - standardise the variables names, replacing all non-ascii characters
    with their closest latin equivalent, removing blank spaces and other
    separators, enforcing lower case capitalisation, and using a single
    separator between words

  - standardise the labels used in all variables of type `character` and
    `factor`, as above

  - set `POSIXct` and `POSIXlt` to `Date` objects

  - extract dates from a messy variable, automatically detecting
    formats, allowing inconsistent formats, and dates flanked by other
    text

## Installing the package

To install the current stable, CRAN version of the package, type:

``` r
install.packages("linelist")
```

To benefit from the latest features and bug fixes, install the
development, *github* version of the package using:

``` r
devtools::install_github("reconhub/linelist")
```

Note that this requires the package *devtools* installed.

# What does it do?

The main features of the package include:

  - `clean_data`: the main function, taking a `data.frame` as input, and
    doing all the variable names, internal labels, and date processing
    described above

  - `clean_variable_names`: like `clean_data`, but only the variable
    names

  - `clean_variable_labels`: like `clean_data`, but only the variable
    labels

  - `clean_dates`: like `clean_data`, but only the dates

  - `guess_dates`: find dates in various, unspecified formats in a messy
    `character` vector

# Worked example

Let us consider some messy `data.frame` as a toy example:

``` r

## make toy data
onsets <- as.Date("2018-01-01") + sample(1:10, 20, replace = TRUE)
discharge <- format(as.Date(onsets) + 10, "%d/%m/%Y")
genders <- c("male", "female", "FEMALE", "Male", "Female", "MALE")
gender <- sample(genders, 20, replace = TRUE)
case_types <- c("confirmed", "probable", "suspected", "not a case",
                "Confirmed", "PROBABLE", "suspected  ", "Not.a.Case")
messy_dates <- sample(
                 c("01-12-2001", "male", "female", "2018-10-18", "2018_10_17",
                   "2018 10 19", "// 24//12//1989", NA, "that's 24/12/1989!"),
                 20, replace = TRUE)
case <- factor(sample(case_types, 20, replace = TRUE))
toy_data <- data.frame("Date of Onset." = onsets,
                       "DisCharge.." = discharge,
                       "GENDER_ " = gender,
                       "Épi.Case_définition" = case,
                       "messy/dates" = messy_dates)
## show data
toy_data
#>    Date.of.Onset. DisCharge.. GENDER_. Épi.Case_définition
#> 1      2018-01-02  12/01/2018     male           confirmed
#> 2      2018-01-07  17/01/2018   Female            PROBABLE
#> 3      2018-01-02  12/01/2018   Female          not a case
#> 4      2018-01-09  19/01/2018     Male            probable
#> 5      2018-01-09  19/01/2018   FEMALE          not a case
#> 6      2018-01-06  16/01/2018   female         suspected  
#> 7      2018-01-08  18/01/2018   female            PROBABLE
#> 8      2018-01-09  19/01/2018     male            PROBABLE
#> 9      2018-01-11  21/01/2018   female          not a case
#> 10     2018-01-05  15/01/2018     MALE           Confirmed
#> 11     2018-01-09  19/01/2018   Female          Not.a.Case
#> 12     2018-01-03  13/01/2018   female            PROBABLE
#> 13     2018-01-10  20/01/2018     MALE         suspected  
#> 14     2018-01-05  15/01/2018   FEMALE           Confirmed
#> 15     2018-01-11  21/01/2018   FEMALE          Not.a.Case
#> 16     2018-01-11  21/01/2018     MALE          not a case
#> 17     2018-01-11  21/01/2018   Female         suspected  
#> 18     2018-01-06  16/01/2018     MALE          Not.a.Case
#> 19     2018-01-06  16/01/2018   female            PROBABLE
#> 20     2018-01-11  21/01/2018   Female           suspected
#>           messy.dates
#> 1          2018-10-18
#> 2          2018 10 19
#> 3                <NA>
#> 4     // 24//12//1989
#> 5              female
#> 6  that's 24/12/1989!
#> 7  that's 24/12/1989!
#> 8     // 24//12//1989
#> 9  that's 24/12/1989!
#> 10               male
#> 11         2018-10-18
#> 12               male
#> 13               male
#> 14               <NA>
#> 15               male
#> 16    // 24//12//1989
#> 17               male
#> 18         2018_10_17
#> 19 that's 24/12/1989!
#> 20    // 24//12//1989
```

``` r
## load library
library(linelist)

## clean data with defaults
x <- clean_data(toy_data)
x
#>    date_of_onset  discharge gender epi_case_definition messy_dates
#> 1     2018-01-02 2018-01-12   male           confirmed  2018-10-18
#> 2     2018-01-07 2018-01-17 female            probable  2018-10-19
#> 3     2018-01-02 2018-01-12 female          not_a_case        <NA>
#> 4     2018-01-09 2018-01-19   male            probable  1989-12-24
#> 5     2018-01-09 2018-01-19 female          not_a_case        <NA>
#> 6     2018-01-06 2018-01-16 female           suspected  1989-12-24
#> 7     2018-01-08 2018-01-18 female            probable  1989-12-24
#> 8     2018-01-09 2018-01-19   male            probable  1989-12-24
#> 9     2018-01-11 2018-01-21 female          not_a_case  1989-12-24
#> 10    2018-01-05 2018-01-15   male           confirmed        <NA>
#> 11    2018-01-09 2018-01-19 female          not_a_case  2018-10-18
#> 12    2018-01-03 2018-01-13 female            probable        <NA>
#> 13    2018-01-10 2018-01-20   male           suspected        <NA>
#> 14    2018-01-05 2018-01-15 female           confirmed        <NA>
#> 15    2018-01-11 2018-01-21 female          not_a_case        <NA>
#> 16    2018-01-11 2018-01-21   male          not_a_case  1989-12-24
#> 17    2018-01-11 2018-01-21 female           suspected        <NA>
#> 18    2018-01-06 2018-01-16   male          not_a_case  2018-10-17
#> 19    2018-01-06 2018-01-16 female            probable  1989-12-24
#> 20    2018-01-11 2018-01-21 female           suspected  1989-12-24

## lower tolerance for unconverted dates
clean_data(toy_data, error_tolerance = 0.05)
#>    date_of_onset  discharge gender epi_case_definition       messy_dates
#> 1     2018-01-02 2018-01-12   male           confirmed        2018_10_18
#> 2     2018-01-07 2018-01-17 female            probable        2018_10_19
#> 3     2018-01-02 2018-01-12 female          not_a_case              <NA>
#> 4     2018-01-09 2018-01-19   male            probable        24_12_1989
#> 5     2018-01-09 2018-01-19 female          not_a_case            female
#> 6     2018-01-06 2018-01-16 female           suspected that_s_24_12_1989
#> 7     2018-01-08 2018-01-18 female            probable that_s_24_12_1989
#> 8     2018-01-09 2018-01-19   male            probable        24_12_1989
#> 9     2018-01-11 2018-01-21 female          not_a_case that_s_24_12_1989
#> 10    2018-01-05 2018-01-15   male           confirmed              male
#> 11    2018-01-09 2018-01-19 female          not_a_case        2018_10_18
#> 12    2018-01-03 2018-01-13 female            probable              male
#> 13    2018-01-10 2018-01-20   male           suspected              male
#> 14    2018-01-05 2018-01-15 female           confirmed              <NA>
#> 15    2018-01-11 2018-01-21 female          not_a_case              male
#> 16    2018-01-11 2018-01-21   male          not_a_case        24_12_1989
#> 17    2018-01-11 2018-01-21 female           suspected              male
#> 18    2018-01-06 2018-01-16   male          not_a_case        2018_10_17
#> 19    2018-01-06 2018-01-16 female            probable that_s_24_12_1989
#> 20    2018-01-11 2018-01-21 female           suspected        24_12_1989
```

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue*](http://github.com/reconhub/linelist/issues) system. All other
questions should be posted on the **RECON forum**: <br>
<http://www.repidemicsconsortium.org/forum/>

Contributions are welcome via **pull requests**.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
