
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datadict: Data dictionary tools for the OCA data sharing platform

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/datadict")
```

### Example usage

#### Generate data dictionary from ODK template

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
dict[1:5,1:5]
#> # A tibble: 5 × 5
#>   variable_name short_label                                              type       choices                       origin
#>   <chr>         <chr>                                                    <chr>      <chr>                         <chr> 
#> 1 Id10002       (Id10002) [Is this a region of high HIV/AIDS mortality?] Coded list high, High | low, Low | very… Origi…
#> 2 Id10003       (Id10003) [Is this a region of high malaria mortality?]  Coded list high, High | low, Low | very… Origi…
#> 3 Id10004       (Id10004) [During which season did (s)he die?]           Coded list wet, Wet | dry, Dry | DK, Do… Origi…
#> 4 Id10007       (Id10007) [What is the name of VA respondent?]           Free text  <NA>                          Origi…
#> 5 Id10007a      (Id10007a) [What is the sex of VA respondent?]           Coded list female, Female | male, Male … Origi…
```

#### Generate data dictionary from REDCap template

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
