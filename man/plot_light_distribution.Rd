% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lighting_distribution.R
\name{plot_light_distribution}
\alias{plot_light_distribution}
\title{Plots a polar chart of a light distribution}
\usage{
plot_light_distribution(
  lum_int_extended_tbl,
  line_color = "#BCCF03",
  line_width = 1,
  title = "",
  x_lab = expression(gamma ~ "[°]"),
  y_lab = expression("I [cd/1000 lm]")
)
}
\arguments{
\item{lum_int_extended_tbl}{A tibble of extended luminous intensity data}

\item{line_color}{A text in hex colour format}

\item{line_width}{A numeric, which controls the line size of \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}}

\item{title}{The text for the plot title}

\item{x_lab}{The text for the x-axis label}

\item{y_lab}{The text for the y-axis label}
}
\value{
A ggplot2 object with polar chart visualisation
}
\description{
\code{plot_light_distribution()} returns a ggplot polar chart of a
light distribution using the extended luminous intensity data, which are
stored in a \code{ld_list}, like \link{ld_data}.
}
\examples{
plot_light_distribution(ld_data$lum_int_extended_tbl)
plot_light_distribution(
  ld_data$lum_int_extended_tbl,
  title = "Test title",
  x_lab = "x-axis",
  y_lab = "y-axis"
)

}
