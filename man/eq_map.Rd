% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Earthquake.R
\name{eq_map}
\alias{eq_map}
\title{eq_map}
\usage{
eq_map(df, annot_col, color = "blue", radius = 5, weight = 1)
}
\arguments{
\item{df}{The data frame you would like to use. Must have Latitude and Longitude columns.}

\item{annot_col}{The column from which the annotations are taken from. Must use df$column notation.}

\item{color}{The color of the location dots.}

\item{radius}{The size of the location dots.}

\item{weight}{The size of the outlines of the dots.}
}
\value{
A map widget with location points of each earthquake
}
\description{
This function creates a map widget of Earthquake Epicenters.
}
\examples{
\dontrun{
EarthquakeData \%>\%
  filter(Country == "Mexico" & year(Date) >= 2000) \%>\%
  eq_map(annot_col = .$Date)}

}
