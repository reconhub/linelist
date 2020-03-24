
# Welcome to the *linelist* package\!

[![Travis build
status](https://travis-ci.org/reconhub/linelist.svg?branch=master)](https://travis-ci.org/reconhub/linelist)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/linelist/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/linelist?branch=master)

This package is dedicated to simplifying the cleaning and
standardisation of [line
list](https://outbreaktools.ca/background/line-lists/) data. Considering
a case line list `data.frame`, it aims to:

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

# Quick example

Let us consider a messy `data.frame` as an example:

``` r
library(linelist)
example_data <- messy_data(10)
example_data
#>       'ID Date of Onset. DisCharge.. GENDER_  Épi.Case_définition
#> 1  cmfvdr     2018-01-07  17/01/2018     male         suspected  
#> 2  opwfwa     2018-01-07  17/01/2018   FEMALE          not a case
#> 3  zsjyva     2018-01-09  19/01/2018     MALE            PROBABLE
#> 4  exqmoe     2018-01-09  19/01/2018   female         suspected  
#> 5  vxvrqq     2018-01-09  19/01/2018     male            probable
#> 6  yrtvea     2018-01-02  12/01/2018     Male            probable
#> 7  tnnlfm     2018-01-10  20/01/2018     male           suspected
#> 8  ihpqxz     2018-01-10  20/01/2018     Male            PROBABLE
#> 9  feabyd     2018-01-06  16/01/2018   female           confirmed
#> 10 bpfkiu     2018-01-03  13/01/2018     male          not a case
#>           messy/dates      lat      lon
#> 1          2018_10_17 13.34652 48.84905
#> 2                <NA> 14.87742 46.98764
#> 3  that's 24/12/1989! 13.66359 49.67178
#> 4          2018_10_17 12.67856 49.25075
#> 5          2018-10-18 12.66741 47.02325
#> 6     // 24//12//1989 13.66111 48.93061
#> 7          2018-10-18 13.60725 47.34525
#> 8                <NA> 10.87389 47.50097
#> 9          2018-10-18 15.94802 48.30479
#> 10 that's 24/12/1989! 14.25785 48.17981
```

We then use the `clean_data()` command to get nice, clean data\!

``` r
clean_data(example_data, guess_dates = TRUE)
#>        id date_of_onset  discharge gender epi_case_definition messy_dates
#> 1  cmfvdr    2018-01-07 2018-01-17   male           suspected  2018-10-17
#> 2  opwfwa    2018-01-07 2018-01-17 female          not_a_case        <NA>
#> 3  zsjyva    2018-01-09 2018-01-19   male            probable  1989-12-24
#> 4  exqmoe    2018-01-09 2018-01-19 female           suspected  2018-10-17
#> 5  vxvrqq    2018-01-09 2018-01-19   male            probable  2018-10-18
#> 6  yrtvea    2018-01-02 2018-01-12   male            probable  1989-12-24
#> 7  tnnlfm    2018-01-10 2018-01-20   male           suspected  2018-10-18
#> 8  ihpqxz    2018-01-10 2018-01-20   male            probable        <NA>
#> 9  feabyd    2018-01-06 2018-01-16 female           confirmed  2018-10-18
#> 10 bpfkiu    2018-01-03 2018-01-13   male          not_a_case  1989-12-24
#>         lat      lon
#> 1  13.34652 48.84905
#> 2  14.87742 46.98764
#> 3  13.66359 49.67178
#> 4  12.67856 49.25075
#> 5  12.66741 47.02325
#> 6  13.66111 48.93061
#> 7  13.60725 47.34525
#> 8  10.87389 47.50097
#> 9  15.94802 48.30479
#> 10 14.25785 48.17981
```

# What does it do?

Procedures to clean data, first and foremost aimed at `data.frame`
formats, include:

  - `clean_data()`: the main function, taking a `data.frame` as input,
    and doing all the variable names, internal labels, and date
    processing described above

  - `clean_variable_names()`: like `clean_data`, but only the variable
    names

  - `clean_variable_labels()`: like `clean_data`, but only the variable
    labels

  - `clean_variable_spelling()`: provided with a dictionary, will
    correct the spelling of values in a variable and can globally
    correct commonly mis-spelled words.

  - `clean_dates()`: like `clean_data`, but only the dates

  - `guess_dates()`: find dates in various, unspecified formats in a
    messy `character` vector

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue*](http://github.com/reconhub/linelist/issues) system. All other
questions should be posted on the **RECON forum**: <br>
<http://www.repidemicsconsortium.org/forum/>

Contributions are welcome via **pull requests**.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
