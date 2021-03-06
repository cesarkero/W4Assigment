% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{fars_summarize_years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{vector with years to find data}
}
\value{
Single tibble with the sum of registers by month and year
}
\description{
Function summarize data from different source files into one tibble containing
months in rows and the number of register by month and year. If a year from the vector
is not listed It will kept out of the tibble
}
\details{
Data will be readed from the execution place of the file
The data in csv.bz2 format need to be in the working directory
}
\examples{
\dontrun{
x <- fars_summarize_years(c(2000,2013,2015, 2021))
}
}
