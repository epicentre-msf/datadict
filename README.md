
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datadict: Data dictionary tools for the OCA data sharing platform

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epicentre-msf/datadict/workflows/R-CMD-check/badge.svg)](https://github.com/epicentre-msf/datadict/actions)
[![Codecov test
coverage](https://codecov.io/gh/epicentre-msf/datadict/branch/main/graph/badge.svg)](https://codecov.io/gh/epicentre-msf/datadict?branch=main)
<!-- badges: end -->

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/datadict")
```

### Example usage

#### Generate data dictionary from ODK template

The `dict_from_odk()` function can be used to generate an OCA-style data
dictionary from an ODK template (both the ‘survey’ and ‘options’ sheets
of the ODK template are required as inputs).

``` r
library(datadict)
library(readxl)

# path to example ODK template (a WHO mortality survey)
path_data <- system.file("extdata", package = "datadict")
path_odk_template <- file.path(path_data, "WHOVA2016_v1_5_3_ODK.xlsx")

# read 'survey' sheet and 'choices' sheet
odk_survey <- readxl::read_xlsx(path_odk_template, sheet = "survey")
odk_choices <- readxl::read_xlsx(path_odk_template, sheet = "choices")

# derive OCA-style data dictionary
dict <- dict_from_odk(odk_survey, odk_choices)

# examine first few rows/cols
dict[1:5,1:4]
#> # A tibble: 5 × 4
#>   variable_name short_label                                  type       choices                                         
#>   <chr>         <chr>                                        <chr>      <chr>                                           
#> 1 Id10002       Is this a region of high HIV/AIDS mortality? Coded list high, High | low, Low | veryl, Very low         
#> 2 Id10003       Is this a region of high malaria mortality?  Coded list high, High | low, Low | veryl, Very low         
#> 3 Id10004       During which season did (s)he die?           Coded list wet, Wet | dry, Dry | DK, Doesn't know          
#> 4 Id10007       What is the name of VA respondent?           Free text  <NA>                                            
#> 5 Id10007a      What is the sex of VA respondent?            Coded list female, Female | male, Male | undetermined, Amb…
```

#### Generate data dictionary from REDCap template

The `dict_from_redcap()` function can be used to generate an OCA-style
data dictionary from a REDCap data dictionary. The input dictionary can
be exported directly from a REDCap project website or fetched via the
API using e.g. the R package
[redcap](https://github.com/epicentre-msf/redcap).

``` r
# path to example REDCap template
path_data <- system.file("extdata", package = "datadict")
path_redcap_dict <- file.path(path_data, "REDCapDataDictionaryDemo.csv")

# read dictionary
redcap_dict <- read.csv(path_redcap_dict)

# derive OCA-style data dictionary
dict <- dict_from_redcap(redcap_dict)

# examine first few rows/cols
dict[1:5,1:5]
#> # A tibble: 5 × 5
#>   variable_name form_or_group short_label                 type      choices
#>   <chr>         <chr>         <chr>                       <chr>     <chr>  
#> 1 study_id      demographics  Study ID                    Free text <NA>   
#> 2 date_enrolled demographics  Date subject signed consent Date      <NA>   
#> 3 first_name    demographics  First Name                  Free text <NA>   
#> 4 last_name     demographics  Last Name                   Free text <NA>   
#> 5 address       demographics  Street, City, State, ZIP    Free text <NA>
```

#### Generate data dictionary template from a dataset

The `dict_from_data()` function can be used to generate a template
OCA-style data dictionary (which may require further processing) from a
dataset. Data types are based on the class of each column within in the
input dataset, e.g.:

| Column class in R | Dictionary data type                                      |
|-------------------|-----------------------------------------------------------|
| Date              | Date                                                      |
| POSIX             | Datetime                                                  |
| integer           | Numeric                                                   |
| numeric           | Numeric                                                   |
| factor            | Coded list                                                |
| character         | Coded list or Free text (see argument `factor_threshold`) |

``` r
# path to example dataset
path_data <- system.file("extdata", package = "datadict")
path_linelist <- file.path(path_data, "linelist_cleaned.xlsx")

# read data
dat <- readxl::read_xlsx(path_linelist)

# derive OCA-style data dictionary template
dict <- dict_from_data(dat)

# examine first few rows/cols
dict[1:7,1:5]
#> # A tibble: 7 × 5
#>   variable_name        short_label type       choices               origin  
#>   <chr>                <chr>       <chr>      <chr>                 <chr>   
#> 1 case_id              <NA>        Free text  <NA>                  original
#> 2 generation           <NA>        Numeric    <NA>                  original
#> 3 date_infection       <NA>        Datetime   <NA>                  original
#> 4 date_onset           <NA>        Datetime   <NA>                  original
#> 5 date_hospitalisation <NA>        Datetime   <NA>                  original
#> 6 date_outcome         <NA>        Datetime   <NA>                  original
#> 7 outcome              <NA>        Coded list 0, Death | 1, Recover original
```
