## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", fig.align='center')
library(ggplot2)
library(grattantheme)

## ---- eval = FALSE, include = FALSE--------------------------------------
#  # Run to build this vignette before release:
#  devtools::build_vignettes()
#  devtools::check()

## ------------------------------------------------------------------------
plot <- ggplot(Orange,
               aes(x = age,
                   y = circumference,
                   fill = Tree)) +
        geom_bar(stat = "identity") +
        labs(x = "Age of tree",
             y = "",
             colour = "Tree")

## ---- fig.align='center', echo = FALSE-----------------------------------
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
  scale_y_continuous_grattan()

## ------------------------------------------------------------------------
plot + 
  theme_grattan(background = "orange") +
  scale_y_continuous_grattan()

## ------------------------------------------------------------------------
plot + 
  coord_flip() +
  theme_grattan(flipped = TRUE) +
  scale_y_continuous_grattan()

## ------------------------------------------------------------------------

plot + 
  theme_grattan(base_size = 8, base_family = "serif") +
  scale_y_continuous_grattan()


## ---- out.width = "250px", echo = FALSE----------------------------------
knitr::include_graphics("grattan_pal.pdf")

## ------------------------------------------------------------------------
plot + 
  geom_vline(xintercept = 850, colour = grattan_grey3) +
  theme_grattan() +
  scale_y_continuous_grattan()


## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  scale_y_continuous_grattan() +
  grattan_fill_manual(n = 5)


## ------------------------------------------------------------------------
plot + 
  theme_grattan() +
  scale_y_continuous_grattan() +
  grattan_fill_manual(n = 5, reverse = TRUE)


## ---- error = TRUE-------------------------------------------------------
plot + 
  theme_grattan() +
  scale_y_continuous_grattan() +
  grattan_fill_manual(n = 3)


