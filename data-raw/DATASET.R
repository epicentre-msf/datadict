
library(tidyverse)
library(redcap)


# fetch REDCap dictionary from test project and write to csv
conn <- redcap::rconn(
  url = "https://www.research.epicentre.msf.org/api/",
  token = Sys.getenv("REDCAP_PKG")
)

redcap_dict <- redcap::meta_dictionary(conn)

readr::write_csv(redcap_dict, "raw-data/dict_redcap_pkg.csv")


# allowed values for certain columns in OCA-style dictionaries
dict_valid_categories <- readxl::read_xlsx("data-raw/dict_allowed_values.xlsx")

usethis::use_data(dict_valid_categories, internal = TRUE, overwrite = TRUE)
