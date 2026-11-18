test_that("colour_text works", {
  text <- colour_text(grattan_red, "fine")

  text_not_bolder <- colour_text(grattan_red, "fine", bold_labs = FALSE, is_note = TRUE)

  expect_type(text, "character")

  expect_type(text_not_bolder, "character")
})

test_that("colour_text is not bold by default", {
  expect_equal(as.character(colour_text("#FF0000", "fine")),
               "<span style='color:#FF0000'>fine</span>")
  expect_equal(as.character(colour_text("#FF0000", "fine", bold_labs = TRUE)),
               "<b style='color:#FF0000'>fine</b>")
})

test_that("colour_text notes don't need a colour", {
  note <- paste0("<span style='color:", grattan_grey3, "'>a note</span>")

  expect_equal(as.character(colour_text("a note", is_note = TRUE)), note)
  expect_equal(as.character(colour_text(text = "a note", is_note = TRUE)), note)
  # A colour, if given, is ignored, as is bold_labs
  expect_equal(as.character(colour_text(grattan_red, "a note", is_note = TRUE,
                                        bold_labs = TRUE)), note)
})

test_that("colour_text checks its inputs", {
  expect_error(colour_text("#FF0000"), "text")
  expect_error(colour_text(1, "fine"), "colour")
  expect_error(colour_text("#FF0000", 1), "text")
  expect_error(colour_text(is_note = TRUE), "text")
})

# Count the number of gridtext richtext grobs in a built ggplot - one per panel
# on which grattan_richlegend() actually draws.
count_richlegend_grobs <- function(p) {
  n <- 0
  recurse <- function(x) {
    if (inherits(x, "richtext_grob")) n <<- n + 1
    if (!is.null(x$children)) for (ch in x$children) recurse(ch)
    if (inherits(x, "gtable")) for (gg in x$grobs) recurse(gg)
  }
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  g <- ggplot2::ggplotGrob(p)
  grid::grid.draw(g)
  grDevices::dev.off()
  unlink(tmp)
  recurse(g)
  n
}

df_legend <- data.frame(
  year = rep(2000:2005, 3),
  value = c(1:6, 2:7, 3:8),
  series = rep(c("A", "B", "C"), each = 6)
)

base_legend_plot <- function(facet = NULL, add_facet = TRUE) {
  args <- list(ggplot2::aes(label = series))
  if (!is.null(facet)) args$facet <- facet
  layer <- do.call(grattan_richlegend, args)
  p <- ggplot(df_legend, aes(year, value, colour = series)) +
    geom_line() +
    layer +
    theme_grattan()
  if (add_facet) p <- p + facet_wrap(~series)
  p
}

test_that("grattan_richlegend draws on the top-left facet by default", {
  expect_equal(count_richlegend_grobs(base_legend_plot()), 1)
})

test_that("grattan_richlegend can draw on every facet", {
  expect_equal(count_richlegend_grobs(base_legend_plot(facet = "all")), 3)
})

test_that("grattan_richlegend can draw on chosen facets by number", {
  expect_equal(count_richlegend_grobs(base_legend_plot(facet = c(2, 3))), 2)
  expect_equal(count_richlegend_grobs(base_legend_plot(facet = 2)), 1)
})

test_that("grattan_richlegend still works on an un-faceted chart", {
  expect_equal(count_richlegend_grobs(base_legend_plot(add_facet = FALSE)), 1)
})

test_that("grattan_richlegend rejects an invalid facet argument", {
  p <- base_legend_plot(facet = TRUE)
  expect_error(count_richlegend_grobs(p),
               "`facet` must be NULL")
})
