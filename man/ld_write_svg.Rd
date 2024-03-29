% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lighting_distribution.R
\name{ld_write_svg}
\alias{ld_write_svg}
\title{Write a \code{ld_list} polar chart to a SVG file}
\usage{
ld_write_svg(ld_list, file)
}
\arguments{
\item{ld_list}{A specific light distribution list}

\item{file}{The path to file without extension.}
}
\value{
\code{ld_write_svg()} returns the ggplot2 object invisibly.
}
\description{
\code{ld_write_svg()} exports the ggplot2 object of the specific
light distribution list \code{ld_list} as a SVG graphic file (*.svg).
}
\examples{
ld_data$plot <- ld_data \%>\% ld_add_light_distribution_plot()

# ld_write_svg() will write the file to the current working directory, if
# it is unspecified without directory path.
\dontrun{
ld_write_svg(ld_data, file = "test")
}
}
