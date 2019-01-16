## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ggplot2)
library(grattantheme)

## ---- eval = FALSE, include = FALSE--------------------------------------
#  # Run this to build the vignette:
#  devtools::build_vignettes()

## ------------------------------------------------------------------------
plot <- ggplot(Orange,
               aes(x = age,
                   y = circumference,
                   colour = Tree)) +
        geom_line() +
        labs(x = "Age of tree",
             y = "")

## ---- echo = FALSE-------------------------------------------------------
plot

## ------------------------------------------------------------------------
plot + 
  theme_grattan()

## ------------------------------------------------------------------------
plot + 
  theme_grattan(legend = "top")

## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  grattan_colour_manual(n = 5)

## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  grattan_colour_manual(n = 5, reverse = TRUE)

