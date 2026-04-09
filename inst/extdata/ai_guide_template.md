## Grattan Chart Style Guide

This file provides guidance for creating charts that conform to the
Grattan Institute style guide using the `grattantheme` R package.

## Key Principles

### Titles and captions

-   **Title**: Should explain the key takeaway/insight, not just
    describe the data (e.g. "Car ownership rises with apartment size"
    not "Car ownership by apartment size")
-   **Subtitle**: Generally serves as the y-axis or x-axis label - you
    do not need to include a separate axis label in this case. If there's units
    that need further explanation (e.g. Growth in rents, December 2019
    to March 2025), include that here, but otherwise units are usually
    included directly on the axis labels (e.g. '\$20k' or '100%').
-   **Caption**: If required, should include a "Notes:" section (clarifying key
    points about the data). Must include a "Source:" section (e.g. "Grattan
    analysis of ABS Census microdata (2021)"). Do NOT separate these with a
    newline as `grattantheme` will insert linebreaks automatically.
-   **Axis title**: The y-axis title is generally blank if not a
    scatterplot. The x-axis title can be blank if the units are self-evident
    (e.g. 2020, 2025) or otherwise explained in the subtitle.

### Use direct labelling instead of legends

Prefer direct labelling with coloured text over traditional legends. Use
`annotate()` or `grattan_label()` to place text labels in the chart
area, coloured to match the data series:

``` r

# EXAMPLE: Using ggdirectlabel for a on-chart 'legend'
geom_richlegend(aes(label = city),
                legend.position = 'bottomright')

# EXAMPLE: Using annotate for manual positioning
# You must adjust x/y coordinates to match your chart's data range
annotate("text", x = 6, y = 15000,
         colour = grattan_orange, label = "Sydney") +
annotate("text", x = 6, y = 13000,
         colour = grattan_red, label = "Melbourne")

# EXAMPLE: Using grattan_label for data-driven positioning
grattan_label(
  data = label_data,
  aes(y = category, x = label_pos, label = series_name, colour = series),
  hjust = 0.5,
  size = 16  # Note: grattan_label auto-converts to .pt
)
```

Don't use bold or italicised labels unless specifically requested.

## Core Functions

### theme_grattan()

The main theme function. Apply to all charts.

``` r
theme_grattan(
  base_size = 18,        # Default font size (use 16 for smaller charts)
  base_family = "sans",
  chart_type = "normal", # or "scatter" for scatter plots
  flipped = NULL,        # Auto-detects; set TRUE for coord_flip(), FALSE to opt out
  background = "white",  # or "orange"/"box" for report boxes
  legend = "none",       # "bottom", "left", "right", "top", or c(0.9, 0.1)
  panel_borders = FALSE  # TRUE for maps
)
```

Note: as of grattantheme 1.5, `flipped` defaults to `NULL` and
auto-detects horizontal/flipped charts (e.g. `coord_flip()` or bar
charts with a discrete y-axis). A message is shown when auto-detection
fires. Set `flipped = TRUE` explicitly to silence the message, or
`flipped = FALSE` to opt out.

### grattan_save() and grattan_save_all()

Save charts in Grattan-approved sizes.

``` r
grattan_save(
  filename = "path/to/chart.pdf",
  object = my_chart,
  type = "normal",        # See chart types below
  save_pptx = TRUE,       # Also save PowerPoint version
  save_data = TRUE,       # Export chart data as xlsx
  no_new_folder = TRUE,   # Don't create subdirectory
  latex = TRUE,           # Export LaTeX code for Overleaf
  force_labs = FALSE       # Keep title/subtitle/caption
)

# Shortcut for all formats with pptx and data to a named subdirectory:
grattan_save_all("chart.pdf", my_chart)
```

Often chart-making scripts should end with `grattan_save_all()` as this
ensures all chart formats and chart data are available at a later stage
for use, without having to re-run the code.

**Chart types:**

- `normal` - Standard report chart (22.2cm x 14.5cm)
- `wholecolumn` - Full column height (22.2cm x 22.2cm)
- `fullpage` - Full page width (44.3cm x 22.2cm)
- `fullslide` - 16:9 PowerPoint slide with logo
- `fullslide_narrow` - Narrower chart for annotations
- `fullslide_half` - Half-width for side-by-side
- `all` - Save in all formats

### grattan_save_overleaf()

Save directly to Overleaf project atlas folder. You must first define
the atlas path using `set_overleaf_project()`. Usually this can be done
in a separate 'setup.R' file, which is then loaded before chart-making
scripts.

``` r
set_overleaf_project("project-name")  # Set once per session
grattan_save_overleaf("chart.pdf", my_chart, type = "normal")
```

## Axis Scales - Avoiding Gaps

Use these functions to ensure no gap between axis and data:

### grattan_y_continuous() / scale_y_continuous_grattan()

``` r
# Basic usage - removes gap at top/bottom
grattan_y_continuous()

# With limits
grattan_y_continuous(limits = c(0, NA))

# For percentages (data in decimal format 0-1)
grattan_y_continuous(
  labels = scales::label_percent(),
  limits = c(0, 1),
  breaks = c(0, 0.25, 0.5, 0.75, 1)
)

# For dollars
grattan_y_continuous(labels = scales::label_currency())

# Custom expand parameters
grattan_y_continuous(
  expand_bottom = 0,      # Default 0 - no gap at bottom
  expand_top = 0.015      # Default 0.015 - small gap at top
)
```

### grattan_x_continuous() / scale_x_continuous_grattan()

``` r
scale_x_continuous_grattan(
  expand_left = 0,        # Default 0 - no gap at left
  expand_right = 0.015    # Default 0.015 - small gap at right
)
```

### For Integer Percentages (data already 0-100)

Generally, our preference is to divide the data by 100 for the avoidance
of doubt.

But another approach is as follows:

``` r
scale_y_continuous_grattan(
  labels = function(x) paste0(x, "%"),
  breaks = c(0, 25, 50, 75, 100)
)
```

## Labelling Functions

### grattan_label()

Creates labels with no border and minimal padding (Grattan style):

``` r
grattan_label(
  data = label_data,
  aes(x = x_pos, y = y_pos, label = text, colour = group),
  size = 18,           # Font size (auto-converted to .pt)
  padding = 0.1,       # White padding around label
  lineheight = 0.8,    # Line spacing
  fill = "white"       # Background colour
)
```

### grattan_label_repel()

Same as above but uses `ggrepel` to avoid overlapping labels:

``` r
grattan_label_repel(
  aes(label = text),
  size = 18
)
```

### grattan_point_filled()

Create points with white centres (useful for line charts):

``` r
grattan_point_filled(
  mapping = NULL,    # Inherits plot aesthetics by default
  data = NULL,       # Inherits plot data by default
  size = 3,
  stroke = 1.5,
  fill = "white",
  shape = 21
)

# Common pattern - add filled points at end of lines
geom_line() +
grattan_point_filled(data = . %>% filter(date == max(date)))
```

## Colour Palette

### Colour Scales

Unless requested, leave `grattantheme` to do its own colour scales. Do not make
your own manual colour palette unless asked. 

### Core Colours

Primary palette (use in this order for discrete scales):

- `grattan_orange` - Primary colour
- `grattan_red` - Secondary colour
- `grattan_yellow` - Third colour
- `grattan_darkorange` - Fourth colour
- `grattan_darkred` - Fifth colour

Additional colours:

- `grattan_blue`, `grattan_darkblue`
- `grattan_grey1` through `grattan_grey5`
- `grattan_black`
- `grattan_box` / `grattan_orange_alpha` - For box backgrounds

Each colour has faded variants (1-8), e.g. `grattan_orange3`,
`grattan_red5`.

If you need to create a manual colour palette, you can do as follows:

``` r
# For continuous scales
scale_colour_grattan(discrete = FALSE, palette = "sequential")
scale_fill_grattan(discrete = FALSE, palette = "diverging")

# Manual assignment 
scale_fill_manual(values = c(
  "Category A" = grattan_yellow,
  "Category B" = grattan_orange,
  "Category C" = grattan_darkorange,
  "Category D" = grattan_red
))
```

## Helper Functions

### fct_case_when()

Like `case_when()` but returns a factor with levels in the order
specified:

``` r
mutate(category = fct_case_when(
  value < 5 ~ "0-5km",
  value < 10 ~ "5-10km",
  TRUE ~ "10km+"
))
```

### str_wrap_factor()

Wrap strings without dropping factor levels:

``` r
mutate(label = str_wrap_factor(long_label, width = 20))
```

### paulify_classifications()

Clean up ANZSIC/ANZSCO classifications (sentence case, oxford commas):

``` r
mutate(industry = paulify_classifications(anzsic_division))
```

### check_chart()

Preview chart in correct aspect ratio before saving (generally don't
include in code, but useful to see output):

``` r
check_chart(type = "fullslide", my_chart)
```

## Common Chart Patterns

### Stacked Bar Charts with Labels

``` r
ggplot(data, aes(y = category, x = pct, fill = group)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = my_colors, guide = "none") +
  grattan_label(
    data = label_data,
    aes(y = category, x = label_pos, label = group, colour = group),
    hjust = 0.5,
    nudge_y = 0.6,
    size = 14
  ) +
  scale_colour_manual(values = my_colors, guide = "none") +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  theme_grattan(base_size = 14) +
  theme(legend.position = "none")
```

### Bar charts with labels that vary in placement according to size of bar

``` r
data %>%
  ggplot(aes(x = number,
             y = category)) +
  geom_col() +
  geom_text(aes(label = round(number, 0),
                hjust = if_else(number < 0.05, -0.2, 1.1),
                colour = if_else(number < 0.05, 'black', 'white')),
            size = 16 / .pt) +
  scale_color_manual(values = c('black', 'white')) +
  theme_grattan(flipped = TRUE)
```

### A 'dumbbell' chart that shows start and end-points of data

``` r

# A chart showing the increase in price-to-income ratios by region

data %>%
  filter(date == max(date) | date == min(date)) %>%
  mutate(date = factor(year(date))) %>%
  ggplot(aes(x = price_to_income, y = fct_rev(region), colour = date)) +
  grattan_x_continuous(limits = c(2, 10),
                     breaks = c(2, 4, 6, 8, 10)) +
  geom_segment(data = . %>%
                 pivot_wider(names_from = date,
                             values_from = price_to_income),
               aes(x = `2001`+ 0.05, xend = `2024` - 0.12,
                   y = fct_rev(region)),
               inherit.aes = FALSE,
               colour = grattan_grey4, linewidth = 0.7,
               arrow = arrow(length = unit(0.035, "npc"),
                             type = 'closed')) +
  grattan_point_filled() +
  grattan_label(data = tibble(x = 3.7, y = 7,
                           label = '2001', urban = 'Capital cities'),
             aes(x = x, y = y, label = label),
             colour = grattan_orange) +
  grattan_label(data = tibble(x = 9.3, y = 7,
                           label = '2024', urban = 'Capital cities'),
             aes(x = x, y = y, label = label), colour = grattan_red) +
  facet_wrap(~urban, scales = 'free_y', ncol = 1) +
  theme_grattan(flipped = TRUE)

```

### Line charts with endpoints

``` r
ggplot(data, aes(x = date, y = value, colour = series)) +
  geom_line() +
  grattan_point_filled(data = . %>% filter(date == max(date))) +
  grattan_y_continuous(labels = scales::label_percent()) +
  grattan_label(
    data = . %>% filter(date == max(date)),
    aes(label = series),
    hjust = 0
  )
```

### Maps

``` r
geom_sf(aes(fill = category), colour = "white", size = 0.3) +
theme_grattan(panel_borders = TRUE) +
theme(
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_blank()
)
```

## Text sizing

For `annotate()` and `geom_text()`, use `.pt` conversion:

``` r
size = 16 / .pt    # Small labels
size = 18 / .pt    # Standard labels

# grattan_label() auto-converts, so just use:
grattan_label(size = 16)  # No need for /.pt
```

## Code styling

### Comments

Don't replace existing comments unless requested. Be parsimonious with
new comments - they should not explain what is clear from the code.
Don't number steps.

### Headings

Often there are several steps to an analysis script. Use section
headings (commented with `# ----` or `## ----`) to separate major
steps.

### Printing to console

Avoid adding `cat()`, `print()`, `message()`, or `cli_alert()`
statements unless explicitly necessary. Do not add progress messages,
status updates, or confirmations - let the code run silently.

### Tidyverse usage

Use `tidyverse` tools and the standard pipe (`%>%`) operator where
applicable for clean, readable analysis code.

## Project setup

### File structure

Usually an analysis folder should be set up as follows:

- `folder_name.Rproj` - An R project for the analysis folder.
- `data/` - Raw data files (e.g. CSVs).
- `R/` - R scripts.
- `atlas/` - All chart outputs saved here.

Any additional README files can be saved in the folder as well.

### R script creation and labelling

Usually we create one chart, or analyse one data source per script.
Names for scripts should be easy to read and consistent, for instance:
`gdp-growth.R`.

Scripts should not assume the presence of any objects in R memory unless
other scripts are specifically sourced, i.e. the script should run
end-to-end without the user having to find other scripts or run code in
the console.

### Data import

Any data that is loaded locally should be saved in the `data/` folder,
and read in using relative file-paths, rather than hard-coded paths.

Where data is available online, we prefer to fetch it directly as part
of the script. For example, we use the `readabs` function to download
ABS data:

``` r
vars <- c(
  'A83778643C', # Total building, original
  'A83775979C' # Private residential, original
  )

data <- read_abs(series_id = vars)
```

Any sensitive data (e.g. ABS or ATO microdata) should be stored in the
Grattan data warehouse. Use the `read_microdata` function from the
`grattandata` package to read that data in.

### Exporting key stats to LaTeX

For reports using Overleaf, export key statistics to `.tex` files. You
must first define the atlas path - either manually or via
`set_overleaf_project()`:

``` r
# Option 1: Define atlas path manually (required at start of script)
atlas <- "C:/Users/username/Grattan Institute Dropbox/.../Apps/Overleaf/project-name/atlas/"

# Option 2: Or use set_overleaf_project() and get the path
set_overleaf_project("project-name")
atlas <- get_overleaf_project()

# Write single values
writeLines(as.character(my_value), paste0(atlas, "stat_name.tex"))

# Format with commas for large numbers
formatted <- format(round(value, -3), big.mark = ",", scientific = FALSE)
writeLines(formatted, file.path(atlas, "stat_name.tex"))

# Format percentages
writeLines(as.character(round(pct_value, 0)), paste0(atlas, "pct_stat.tex"))
```
