% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valid_dict.R
\name{valid_dict}
\alias{valid_dict}
\title{Check that a data dictionary complies with the OCA data sharing standard}
\usage{
valid_dict(dict, verbose = TRUE)
}
\arguments{
\item{dict}{A data frame reflecting a data dictionary to validate}

\item{verbose}{Logical indicating whether to give warning describing the
checks that have failed. Defaults to TRUE.}
}
\value{
\code{TRUE} if all checks pass, \code{FALSE} if any checks fail
}
\description{
Includes the following checks:
\itemize{
\item contains required columns
\item required columns complete (no missing values)
\item no duplicated values in column \code{variable_name}
\item no non-valid values in columns \code{type}, \code{origin}, \code{status}, \code{indirect_identifier}
\item no missing choices (for coded-list type variables)
\item no incorrectly formatted choices (for coded-list type variables)
}
}
\examples{
# read example dataset
path_data <- system.file("extdata", package = "datadict")
dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))

# generate data dictionary template from dataset
dict <- dict_from_data(dat, factor_values = "string")

# dictionary column 'indirect_identifier' must be manually specified (yes/no)
dict$indirect_identifier <- "no"

# check for validity
valid_dict(dict)

}
