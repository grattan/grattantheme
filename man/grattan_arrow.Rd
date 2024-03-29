% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grattan_arrow.R
\name{grattan_arrow}
\alias{grattan_arrow}
\title{Create arrows in the Grattan style}
\usage{
grattan_arrow(..., type = "closed", angle = 20, length = unit(0.5, "lines"))
}
\arguments{
\item{...}{Arguments passed on to either `grid::arrow()`.
See \code{?arrow}.}

\item{type}{Arrow head type. Default is "closed".}

\item{angle}{Angle of arrow head. Default is 20.}

\item{length}{A unit specifying the length of the arrow head (from tip to base). Default 0.5.}
}
\description{
`grattan_arrow()` adds arrows to lines using
`grid::arrow()`. The arrows will have a closed arrow head, in keeping with Grattan
Institute's visual style.
}
\examples{

library(ggplot2)

# Create a dataset -- mtcars with rownames as column
mt <- mtcars
mt$car <- rownames(mt)

# Make a basic chart with the mtcars data
p <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
 geom_point() +
 theme_grattan()

# Add a simple callout arrow to the chart, using geom_segment, with arrow = grattan_arrow()
p +
geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
 arrow = grattan_arrow(),
 colour = grattan_red)

# Alternatively we could add a curved arrow, using geom_cuve()
p +
geom_curve(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
 arrow = grattan_arrow(),
 curvature = 0.3,
 colour = grattan_red)


}
