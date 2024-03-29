% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k_anonymity.R
\name{k_anonymity}
\alias{k_anonymity}
\title{Check the k-anonymity of one or more variables in a dataset}
\usage{
k_anonymity(x, vars)
}
\arguments{
\item{x}{A data frame}

\item{vars}{A character vector containing the name(s) of the variable(s) in
\code{x} to be included in the k-anonymity calculation}
}
\value{
The minimum observed value of k in the dataset, corresponding to the
unique combination of identifying variables with the fewest observations
}
\description{
Given a dataset and set of one or more variables of interest (e.g. variables
that are potential indirect identifiers), the function returns the minimum
observed value of k in the dataset, corresponding to the unique combination
of identifying variables with the fewest observations.
}
\examples{
# read example dataset
path_data <- system.file("extdata", package = "datadict")
dat <- readxl::read_xlsx(file.path(path_data, "linelist_cleaned.xlsx"))

# find minimum observed k for potential indirect identifiers gender and age_cat
k_anonymity(dat, vars = c("gender", "age_cat"))

}
