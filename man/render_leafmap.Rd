% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{render_leafmap}
\alias{render_leafmap}
\title{render_leafmap}
\usage{
render_leafmap(
  data,
  color,
  popup,
  labels,
  radius,
  opacity,
  map = "CartoDB.DarkMatter"
)
}
\arguments{
\item{data}{The data to be ploted on the leaflet map.}

\item{color}{The vector that is to be used to choose colour gradient.}

\item{popup}{The vector of characters that will be used in the popups. Can put raw html in the text such as "<br>".}

\item{labels}{The vector of characters that will be used in the labels that appear during hover.}

\item{radius}{The numerical vector to be used to determine the radius of the circles on the map.}

\item{opacity}{The numerical vector to be used to determine the opacity of the circles on the map.}

\item{map}{Can take boolean value such as outputted by a checkbox to indicate if the light theme map should be used.
Can also take a character string indicating the map distripution to be used for ploting.}
}
\value{
A leaflet map.
}
\description{
Render leaflet map enable the selection of which elements (colour, size, etc) governs what graphical element.
Remember to select only those that have infraction counts over 0 and not NULL.
}
