# grattantheme
Create ggplot2 charts in the Grattan Institute style

Use theme_grattan() to format your ggplot2 charts in a style consistent with the Grattan style guide, including elements such as gridline colours and width, font size, etc.

Use grattan_pal() to format the coloured elements of your ggplot2 chart using appropriately-spaced Grattan colours, ordered from either light to dark or the reverse.

Use grattan_save() to save your ggplot2 charts (eg. as .png or .pdf files) for use elsewhere, such as in Powerpoint or LaTeX, with the size and resolution set to style guide-consistent values.

A range of colours from the style guide (such as grattan_lightorange, grattan_red, and so on) are defined for your convenience.

An example, using the three functions in the package:

library(tidyverse)
library(grattantheme)

mtcars %>%
ggplot(aes(x = wt, y = mpg, col = factor(cyl))) +
geom_point(size = 2) +
theme_grattan() +
scale_y_continuous(limits = c(0, 35), expand = c(0,0)) +
scale_colour_manual(values = grattan_pal(n = 3))

grattan_save("test.png")

