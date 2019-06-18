# grattantheme 0.3.1.900
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
