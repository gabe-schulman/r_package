% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Maps yearly accident data for a state.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The unique ID that identifies the state the user is interested in analyzing.}

\item{year}{The year of accident data the user wants to display.}
}
\value{
Returns a map that will be displayed.
}
\description{
This function takes the state and year as arguments and produces a map of accidents in the state
for the year specified. This function will throw an error if the state number is not valid, or if there are
no accidents in state for the year specified. This function requires the "maps" package to be installed.
}
