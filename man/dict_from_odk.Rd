% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dict_from_odk.R
\name{dict_from_odk}
\alias{dict_from_odk}
\title{Convert an ODK template to the OCA data sharing dictionary standard}
\usage{
dict_from_odk(survey, choices, col_labels = NULL)
}
\arguments{
\item{survey}{A data.frame reflecting the 'survey' sheet within an ODK
template}

\item{choices}{A data.frame reflecting the 'choices' sheet within an ODK
template}

\item{col_labels}{(Optional) Name of the column found in both sheets 'survey'
and 'choices' giving variable labels, e.g. "label::English". Defaults to
\code{NULL} in which case the first column starting with prefix "label" is used
for the label column.}
}
\value{
A \code{\link[tibble:tbl_df-class]{tibble}}-style data frame representing a data dictionary
formatted to the OCA data sharing standard
}
\description{
Convert an ODK template to the OCA data sharing dictionary standard
}
\examples{
path_data <- system.file("extdata", package = "datadict")
path_odk_template <- file.path(path_data, "WHOVA2016_v1_5_3_ODK.xlsx")

odk_survey <- readxl::read_xlsx(path_odk_template, sheet = "survey")
odk_choices <- readxl::read_xlsx(path_odk_template, sheet = "choices")

dict_from_odk(odk_survey, odk_choices)

}
