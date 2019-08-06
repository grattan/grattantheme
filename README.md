[![Travis-CI Build Status](https://travis-ci.org/MattCowgill/grattantheme.svg?branch=master)](https://travis-ci.org/MattCowgill/grattantheme)


# grattantheme
Create ggplot2 charts in the Grattan Institute style.

See the [grattantheme vignette](https://github.com/MattCowgill/grattantheme/blob/master/vignettes/using_grattantheme.pdf) to learn how to make your ggplot2 charts Grattan-y.


## Make your charts look nice
Use `theme_grattan()` to format your ggplot2 charts in a style consistent with the Grattan style guide, including elements such as gridline colours and line width, font size, etc.

Use `grattan_colour_manual(n)` or `grattan_fill_manual(n)` to format the `n` coloured elements of your `ggplot2` plot. These functions will choose appropriately-spaced Grattan colours, ordered from either light to dark or the reverse. 

![](man/grattan_pal_colours.png)

Use `grattan_y_continuous()` to set default values for your vertical axis that will work well with most Grattan charts.

A range of colours from the style guide (such as `grattan_lightorange`, `grattan_red`, and so on) are defined for your convenience.

## Save your nice looking charts

Use `grattan_save()` to save your ggplot2 charts (eg. as `.png` or `.pdf` files) for use elsewhere, such as in Powerpoint, LaTeX, or the Grattan Blog, with the size and resolution set to style guide-consistent values. Design features including the Grattan logo are included in the image where appropriate.

Use `make_slide()` and `make_presentation()` to create Powerpoint presentations from your chart(s), including editable titles and subtitles.

## Notes

Some graph defaults (such as colour) will be modified during your R session; restart R to restore all defaults.

Please note that some manual modification to your chart will most likely be required in order to make it fully consistent with the style guide, just as it would in other visualisation software or Excel/Powerpoint. 
