---
output: github_document
---






# Welcome to the *linelist* package!

[![Travis build status](https://travis-ci.org/reconhub/linelist.svg?branch=master)](https://travis-ci.org/reconhub/linelist)
[![Codecov test coverage](https://codecov.io/gh/reconhub/linelist/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/linelist?branch=master)

This package is dedicated to simplifying the cleaning and standardisation of
linelist data. Considering a case linelist `data.frame`, it aims to:

- standardise the variables names, replacing all non-ascii characters with their
  closest latin equivalent, removing blank spaces and other separators,
  enforcing lower case capitalisation, and using a single separator between
  words
  
- standardise the labels used in all variables of type `character` and `factor`,
  as above

- set `POSIXct` and `POSIXlt` to `Date` objects

- extract dates from a messy variable, automatically detecting formats, allowing
  inconsistent formats, and dates flanked by other text
  
- support data dictionary: `linelist` objects can store meta-data indicating
  which columns correspond to standard epidemiological variables, usually found
  in linelists such as a unique identifier, gender, or dates of onset
  


## Installing the package

To install the current stable, CRAN version of the package, type:

```r
install.packages("linelist")
```

To benefit from the latest features and bug fixes, install the development, *github* version of the package using:

```r
devtools::install_github("reconhub/linelist")
```

Note that this requires the package *devtools* installed.


# What does it do?

## Data cleaning

Procedures to clean data, first and foremost aimed at `data.frame` formats,
include:

- `clean_data()`: the main function, taking a `data.frame` as input, and doing all
  the variable names, internal labels, and date processing described above
  
- `clean_variable_names()`: like `clean_data`, but only the variable names

- `clean_variable_labels()`: like `clean_data`, but only the variable labels

- `clean_dates()`: like `clean_data`, but only the dates

- `guess_dates()`: find dates in various, unspecified formats in a messy
  `character` vector
  
- `as_linelist()`: create a new `linelist` object from a `data.frame`
  

## Dictionary

*linelist* also handles a *dictionary* of pre-defined, standard epidemiological
variables, referred to as `epivars` throughout the package. Meta-information can
be attached to `linelist` objects to define which columns of the dataset
correspond to specific `epivars`. 

The main functions to handle the `epivars` of a `linelist` object include:

- `list_epivars()`: lists the `epivars` of a dataset, with options to have more
  or less information

- `get_epivars()`: extract columns of a dataset corresponding to `epivars`

- `set_epivars()`: set the `epivars` of a dataset

- `as_linelist()`: creates a new `linelist` object, and can define `epivars` as
  extra arguments

In addition, several functions allow to interact with the dictionary of
recognised `epivars`, including:

- `default_dictionary()`: shows the default dictionary of `epivars`

- `get_dictionary()`: shows the current `epivars` dictionary

- `set_dictionary()`: set the current `epivars` dictionary; if arguments are
  empty, reset to the defaults
  
- `reset_dictionary()`: reset the current `epivars` dictionary to defaults
  


# Worked example

Let us consider some messy `data.frame` as a toy example: 


```r

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
                       "SeX_ " = gender,
                       "Épi.Case_définition" = case,
                       "messy/dates" = messy_dates)
## show data
toy_data
#>    Date.of.Onset. DisCharge..  SeX_. Épi.Case_définition
#> 1      2018-01-11  21/01/2018 Female           Confirmed
#> 2      2018-01-08  18/01/2018   male            probable
#> 3      2018-01-10  20/01/2018 FEMALE          not a case
#> 4      2018-01-03  13/01/2018 female           suspected
#> 5      2018-01-10  20/01/2018   MALE           Confirmed
#> 6      2018-01-08  18/01/2018   Male          Not.a.Case
#> 7      2018-01-02  12/01/2018   male         suspected  
#> 8      2018-01-02  12/01/2018 Female            PROBABLE
#> 9      2018-01-06  16/01/2018 female          Not.a.Case
#> 10     2018-01-09  19/01/2018 FEMALE         suspected  
#> 11     2018-01-06  16/01/2018 female           confirmed
#> 12     2018-01-05  15/01/2018 female         suspected  
#> 13     2018-01-02  12/01/2018 FEMALE           confirmed
#> 14     2018-01-08  18/01/2018   Male          Not.a.Case
#> 15     2018-01-07  17/01/2018   MALE          not a case
#> 16     2018-01-08  18/01/2018   MALE            probable
#> 17     2018-01-06  16/01/2018   MALE           confirmed
#> 18     2018-01-04  14/01/2018 FEMALE           confirmed
#> 19     2018-01-10  20/01/2018   Male           confirmed
#> 20     2018-01-07  17/01/2018 female           Confirmed
#>           messy.dates
#> 1  that's 24/12/1989!
#> 2     // 24//12//1989
#> 3  that's 24/12/1989!
#> 4              female
#> 5                male
#> 6                male
#> 7          2018 10 19
#> 8          2018_10_17
#> 9                <NA>
#> 10 that's 24/12/1989!
#> 11               male
#> 12         01-12-2001
#> 13         01-12-2001
#> 14               <NA>
#> 15         2018_10_17
#> 16               male
#> 17         01-12-2001
#> 18         2018 10 19
#> 19               <NA>
#> 20         01-12-2001
```

We start by cleaning these data:


```r
## load library
library(linelist)

## clean data with defaults
x <- clean_data(toy_data)
x
#>    date_of_onset  discharge    sex epi_case_definition messy_dates
#> 1     2018-01-11 2018-01-21 female           confirmed  1989-12-24
#> 2     2018-01-08 2018-01-18   male            probable  1989-12-24
#> 3     2018-01-10 2018-01-20 female          not_a_case  1989-12-24
#> 4     2018-01-03 2018-01-13 female           suspected        <NA>
#> 5     2018-01-10 2018-01-20   male           confirmed        <NA>
#> 6     2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 7     2018-01-02 2018-01-12   male           suspected  2018-10-19
#> 8     2018-01-02 2018-01-12 female            probable  2018-10-17
#> 9     2018-01-06 2018-01-16 female          not_a_case        <NA>
#> 10    2018-01-09 2018-01-19 female           suspected  1989-12-24
#> 11    2018-01-06 2018-01-16 female           confirmed        <NA>
#> 12    2018-01-05 2018-01-15 female           suspected  2001-12-01
#> 13    2018-01-02 2018-01-12 female           confirmed  2001-12-01
#> 14    2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 15    2018-01-07 2018-01-17   male          not_a_case  2018-10-17
#> 16    2018-01-08 2018-01-18   male            probable        <NA>
#> 17    2018-01-06 2018-01-16   male           confirmed  2001-12-01
#> 18    2018-01-04 2018-01-14 female           confirmed  2018-10-19
#> 19    2018-01-10 2018-01-20   male           confirmed        <NA>
#> 20    2018-01-07 2018-01-17 female           confirmed  2001-12-01
```

We can now define some `epivars` for `x`, i.e. identify which columns correspond
to typical epidemiological variables:


```r
## see what the dictionary is
get_dictionary()
#> [1] "id"          "date_onset"  "date_report" "gender"      "age"        
#> [6] "age_group"   "geo"

## see current names of variables
names(x)
#> [1] "date_of_onset"       "discharge"           "sex"                
#> [4] "epi_case_definition" "messy_dates"

## some variables are known epivars; let's create a linelist object and register
## this information at the same time
x <- as_linelist(x, date_onset = "date_of_onset", gender = "sex")
x
#>    date_of_onset  discharge    sex epi_case_definition messy_dates
#> 1     2018-01-11 2018-01-21 female           confirmed  1989-12-24
#> 2     2018-01-08 2018-01-18   male            probable  1989-12-24
#> 3     2018-01-10 2018-01-20 female          not_a_case  1989-12-24
#> 4     2018-01-03 2018-01-13 female           suspected        <NA>
#> 5     2018-01-10 2018-01-20   male           confirmed        <NA>
#> 6     2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 7     2018-01-02 2018-01-12   male           suspected  2018-10-19
#> 8     2018-01-02 2018-01-12 female            probable  2018-10-17
#> 9     2018-01-06 2018-01-16 female          not_a_case        <NA>
#> 10    2018-01-09 2018-01-19 female           suspected  1989-12-24
#> 11    2018-01-06 2018-01-16 female           confirmed        <NA>
#> 12    2018-01-05 2018-01-15 female           suspected  2001-12-01
#> 13    2018-01-02 2018-01-12 female           confirmed  2001-12-01
#> 14    2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 15    2018-01-07 2018-01-17   male          not_a_case  2018-10-17
#> 16    2018-01-08 2018-01-18   male            probable        <NA>
#> 17    2018-01-06 2018-01-16   male           confirmed  2001-12-01
#> 18    2018-01-04 2018-01-14 female           confirmed  2018-10-19
#> 19    2018-01-10 2018-01-20   male           confirmed        <NA>
#> 20    2018-01-07 2018-01-17 female           confirmed  2001-12-01
```

Note that the equivalent can be done using piping:

```r
library(magrittr)
x <- toy_data %>%
  clean_data %>%
  as_linelist(date_onset = "date_of_onset", gender = "sex")
x
#>    date_of_onset  discharge    sex epi_case_definition messy_dates
#> 1     2018-01-11 2018-01-21 female           confirmed  1989-12-24
#> 2     2018-01-08 2018-01-18   male            probable  1989-12-24
#> 3     2018-01-10 2018-01-20 female          not_a_case  1989-12-24
#> 4     2018-01-03 2018-01-13 female           suspected        <NA>
#> 5     2018-01-10 2018-01-20   male           confirmed        <NA>
#> 6     2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 7     2018-01-02 2018-01-12   male           suspected  2018-10-19
#> 8     2018-01-02 2018-01-12 female            probable  2018-10-17
#> 9     2018-01-06 2018-01-16 female          not_a_case        <NA>
#> 10    2018-01-09 2018-01-19 female           suspected  1989-12-24
#> 11    2018-01-06 2018-01-16 female           confirmed        <NA>
#> 12    2018-01-05 2018-01-15 female           suspected  2001-12-01
#> 13    2018-01-02 2018-01-12 female           confirmed  2001-12-01
#> 14    2018-01-08 2018-01-18   male          not_a_case        <NA>
#> 15    2018-01-07 2018-01-17   male          not_a_case  2018-10-17
#> 16    2018-01-08 2018-01-18   male            probable        <NA>
#> 17    2018-01-06 2018-01-16   male           confirmed  2001-12-01
#> 18    2018-01-04 2018-01-14 female           confirmed  2018-10-19
#> 19    2018-01-10 2018-01-20   male           confirmed        <NA>
#> 20    2018-01-07 2018-01-17 female           confirmed  2001-12-01
```

We now handle a clean dataset, with standardised labels and variable names, and
dates of onset and gender are now formally identifier:

```r
## check available epivars
list_epivars(x, simple = TRUE) # simple
#> [1] "date_onset" "gender"
list_epivars(x, epivars_only = TRUE) # more info
#>          column     epivar     class hxl
#> 1 date_of_onset date_onset      Date    
#> 3           sex     gender character


get_epivars(x, "gender", "date_onset")
#>       sex date_of_onset
#> 1  female    2018-01-11
#> 2    male    2018-01-08
#> 3  female    2018-01-10
#> 4  female    2018-01-03
#> 5    male    2018-01-10
#> 6    male    2018-01-08
#> 7    male    2018-01-02
#> 8  female    2018-01-02
#> 9  female    2018-01-06
#> 10 female    2018-01-09
#> 11 female    2018-01-06
#> 12 female    2018-01-05
#> 13 female    2018-01-02
#> 14   male    2018-01-08
#> 15   male    2018-01-07
#> 16   male    2018-01-08
#> 17   male    2018-01-06
#> 18 female    2018-01-04
#> 19   male    2018-01-10
#> 20 female    2018-01-07

## or equivalent:
get_epivars(x, c(2,1))
#>       sex date_of_onset
#> 1  female    2018-01-11
#> 2    male    2018-01-08
#> 3  female    2018-01-10
#> 4  female    2018-01-03
#> 5    male    2018-01-10
#> 6    male    2018-01-08
#> 7    male    2018-01-02
#> 8  female    2018-01-02
#> 9  female    2018-01-06
#> 10 female    2018-01-09
#> 11 female    2018-01-06
#> 12 female    2018-01-05
#> 13 female    2018-01-02
#> 14   male    2018-01-08
#> 15   male    2018-01-07
#> 16   male    2018-01-08
#> 17   male    2018-01-06
#> 18 female    2018-01-04
#> 19   male    2018-01-10
#> 20 female    2018-01-07
```



## Getting help online

Bug reports and feature requests should be posted on *github* using the [*issue*](http://github.com/reconhub/linelist/issues) system. All other questions should be posted on the **RECON forum**: <br>
[http://www.repidemicsconsortium.org/forum/](http://www.repidemicsconsortium.org/forum/)

Contributions are welcome via **pull requests**.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Roadmap
=======

The linelist package should have the following features in the future:

 - [ ] A data dictionary that allows you to map standard variable names to columns
 - [ ] Integration with [\#hxl standard](http://hxlstandard.org/standard/1_1final/)
 - [ ] Validation of categorical values
