% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Earthquake.R
\name{geom_timeline}
\alias{geom_timeline}
\title{geom_timeline}
\usage{
geom_timeline(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{mapping aesthetics to be used for your time line. Includes x, y, color, size, alpha, and shape parameters.}

\item{data}{the data to be used to add your time line to the graph. Defaults to the ggplot data if not specified.}

\item{stat}{A stat to be used to transform the given data.}

\item{position}{The positioning of the time line on the graph.}

\item{na.rm}{Removes NA's if TRUE.}

\item{show.legend}{Can show a legend for the time line.}

\item{inherit.aes}{Asks if you would like to inherit parameters.}

\item{...}{any additional arguments.}
}
\value{
A pointsGrob object in the shape of a time line on your graph.
}
\description{
This function adds a time line to a ggplot object.
}
\examples{
\dontrun{
ggplot(data = EarthquakeData[6175:6200,]) +
geom_timeline(aes(x = Date, y = factor(Mag, levels = unique(Mag)),
                  color = `Houses Destroyed`, size = `Total Deaths`, alpha = `Total Injuries`))
}
\dontrun{ggplot(EarthquakeData) + geom_timeline(aes(x = Date, y = 5))}

}
