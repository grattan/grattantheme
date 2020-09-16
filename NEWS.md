# grattantheme 0.8.3
* `grattan_save_all()` now included as a wrapper around `grattan_save(type = "all")`

# grattantheme 0.8.2.1
* `chart_types` data frame is now exported, for contexts where it is useful to have
information about the chart types in the grattantheme package.

# grattantheme 0.8.2
* "blog_half" chart type now available. Main use is Powerpoint slides where you want the chart to take up half the slide.

# grattantheme 0.8.1
* Title and subtitle in patchwork plots now recognised by `create_fullslide()` and related functions.

# grattantheme 0.8.0.3
* Error in 'blog' PPTX template fixed.

# grattantheme 0.8.0.2
* A range of additional chart types are now able to be saved as Powerpoint files with `grattan_save_pptx()`, including 'blog', 'fullpage', 'normal', 'wholecolumn'.

# grattantheme 0.8.0.1
* Error fixed with new `grattan_save_pptx()` function - it would previously fail when title and/or subtitle was blank.
* Default lineheight for `grattan_label()` reduced to 0.8

# grattantheme 0.8.0
* New function `grattan_save_pptx()` saves a ggplot2 plot, or a list of plots, as a Powerpoint document with vector graphics and editable labels.
* `grattan_save()` gains an argument: `save_pptx`. When `TRUE`, it will use `grattan_save_pptx()` to also save your image as a Powerpoint document.
* Functions `make_slide()` and `make_presentation()` have been deprecated.
* In `grattan_save()`, `save_data` no longer automatically `TRUE` if `type = "all"`

# grattantheme 0.7.0
* `create_fullslide()` completely rewritten; now much more flexible + faster
* `warn_labs` argument to `grattan_save()` has been removed

# grattantheme 0.6.2
* fullslide_169 animations now possible

# grattantheme 0.6.1
* Title and caption now aligned with left of plot
* ggplot 3.3.0 now required
* Default panel spacing expanded

# grattantheme 0.6.0
* theme_grattan() has been rewritten and is now a complete theme
* theme_grattan() has gained a new chart_type argument; can be set to "scatter" to format scatterplots nicely.
* Minor revisions to reflect changes in ggplot2
* Bug fixes and improvements to save_chartdata() function

# grattantheme 0.5.1
* Charts without a subtitle that are saved as "fullslide" or similar will now vertically expand the plot to fill the space of the absent subtitle 
* save_chartdata() function added
* New 'faded' versions of the Grattan palette available

# grattantheme 0.5.0
* grattan_anim_save() allows users to save gganimate animations formatted in the Grattan style
* theme_grattan() gains a panel_borders argument
* Minor visual fixes to fullslide charts
* More testing of functions to ensure style guide-compatibility
* stitch_pdfs() function is now hard-deprecated

# grattantheme 0.4.0
* New "blog" type available with grattan_save()
* Users can now manually specify height of figures, overriding defaults
* New palettes options added
* Left and white plot margins reduced
* stitch_pdfs() function is now deprecated
* grattan_save() now accepts additional arguments ... to pass to ggsave()
* Warnings about labels are no longer displayed when using grattan_save(type = "all")
* fullslide charts are now quicker to save, and don't print chart elements
* Vertical alignment of subtitles in fullslide charts fixed, to ensure appropriate whitespace between subtitle and plot
* make_presentation() now includes the chart title, subtitle, and caption in .pptx notes
* Grattan-ish Word template now included for use with redoc/Rmarkdown
* fixes bug in create_fullslide() introduced with ggplot 3.2.0

# grattantheme 0.3.1
* New make_presentation() function takes a list of graphs and creates a .pptx presentation with multiple slides
* grattan_save() accepts type="all", which will save your graph in all available types
* Bugs fixed

# grattantheme 0.3.0
* New make_slide() function creates a .pptx slide with an embedded Grattan-y graph
* Labels now wrap nicely for various graph types
* New stitch_pdfs() function allows you to combine multiple pdfs into one; useful for creating a chart pack from multiple charts saved with grattan_save()
* grattan_pal now copes with `n` up to 9
* More geoms are now updated by default to make them Grattan-y
* grattan_save() now has more save types, including fullslide_169 and fullslide_44
