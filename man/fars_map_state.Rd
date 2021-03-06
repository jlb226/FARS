% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot a map of accidents}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer or a string corresponding to a specific state within the data set}

\item{year}{An integer or a string corresponding to a specific year}
}
\value{
The fars_map_state function returns a map of the observations for the specified state and year
}
\description{
The fars_map_state function accepts as input a state number and a year as either integers or strings.
It then uses the values to plot a map of the observations for the specified state and year.
}
\details{
The fars_map_state function returns an error message for invalid years, invalid state numbers,
and if the data is not present
}
\examples{
\dontrun{
if (requireNamespace("maps", quietly = TRUE)){
    fars_map_state(3, 2015)
  }
}

}
