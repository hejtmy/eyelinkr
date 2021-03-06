% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyelinkr-preprocessing.R
\name{flip_y_axis}
\alias{flip_y_axis}
\title{Flips the Y axis, which is logged with the 0 in bottom left corner}
\usage{
flip_y_axis(df, disp_resolution)
}
\arguments{
\item{df}{dataframe with x, y columns, either gaze or fixations}

\item{disp_resolution}{list with height and width fields}
}
\value{
data.frame with y axis flipped
}
\description{
Flips the Y axis, which is logged with the 0 in bottom left corner
}
