# grattantheme
Create ggplot2 charts in the Grattan Institute style.

Use `theme_grattan()` to format your ggplot2 charts in a style consistent with the Grattan style guide, including elements such as gridline colours and line width, font size, etc.

Use `grattan_pal()` to format the coloured elements of your ggplot2 chart using appropriately-spaced Grattan colours, ordered from either light to dark or the reverse.

![grattan_pal](atlas/grattan_pal_colours.png)

Use `scale_y_continuous_grattan()` to set default values for your vertical axis that will work well with most Grattan charts.

Use `grattan_save()` to save your ggplot2 charts (eg. as `.png` or `.pdf` files) for use elsewhere, such as in Powerpoint or LaTeX, with the size and resolution set to style guide-consistent values.

A range of colours from the style guide (such as `grattan_lightorange`, `grattan_red`, and so on) are defined for your convenience.

Please note that some manual modification to your chart will most likely be required in order to make it fully consistent with the style guide, just as it would in other graphing software. Some graph defaults (such as colour) will be modified during your R session; restart R to restore all defaults.

