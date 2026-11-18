box_orange <- grattan_orange_alpha
header_grey <- "#F2F2F2"

base_plot <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_grattan()
}

# Colour of a pixel near the bottom-right corner of a saved PNG, which is
# background in every chart type
corner_colour <- function(file) {
  img <- png::readPNG(file)
  px <- img[nrow(img) - 5, ncol(img) - 5, 1:3]
  toupper(grDevices::rgb(px[1], px[2], px[3]))
}

# Checking the argument --------------------------------------------------------

test_that("check_background() accepts and tidies the allowed values", {
  expect_equal(check_background("white"), character(0))
  expect_equal(check_background("box"), "box")
  expect_equal(check_background("orange"), "box")
  expect_equal(check_background("grey_blog"), "grey_blog")
  expect_setequal(check_background(c("box", "grey_blog")),
                  c("box", "grey_blog"))
})

test_that("check_background() rejects other values with a useful error", {
  expect_error(check_background("grey"), "grey_blog")
  expect_error(check_background("blue"), "blue")
  expect_error(check_background(c("white", "box")), "combined")
  expect_error(check_background(1), "background")
})

# Which colour each format gets ------------------------------------------------

test_that("each format gets the right background colour", {
  both <- c("box", "grey_blog")

  expect_equal(background_colour("normal", both), box_orange)
  expect_equal(background_colour("fullpage", both), box_orange)
  expect_equal(background_colour("normal", both, web = TRUE), "white")
  expect_equal(background_colour("fullslide", both), "white")
  expect_equal(background_colour("blog", both), header_grey)
  expect_equal(background_colour("blog", "box"), "white")
  expect_equal(background_colour("normal", "grey_blog"), "white")

  # Nothing asked for: the chart is left exactly as it is
  expect_null(background_colour("normal", character(0)))
})

test_that("theme_grattan(background = 'box') counts as background = 'box'", {
  expect_equal(resolve_background(base_plot(), "white"), character(0))
  expect_equal(resolve_background(base_plot() +
                                    theme_grattan(background = "box"),
                                  "white"),
               "box")
})

# Labels -----------------------------------------------------------------------

test_that("white label boxes take the background colour", {
  p <- base_plot() +
    grattan_label(data = data.frame(wt = 3, mpg = 30),
                  ggplot2::aes(label = "a")) +
    grattan_richlegend(ggplot2::aes(label = factor(cyl),
                                    colour = factor(cyl))) +
    grattan_label(data = data.frame(wt = 4, mpg = 20),
                  ggplot2::aes(label = "b"), fill = grattan_yellow)

  recoloured <- apply_background(p, header_grey)

  expect_equal(recoloured$layers[[2]]$aes_params$fill, header_grey)
  expect_equal(recoloured$layers[[3]]$geom_params$fill, header_grey)
  # A label with its own fill keeps it
  expect_equal(recoloured$layers[[4]]$aes_params$fill, grattan_yellow)
  # The caller's plot is unchanged
  expect_equal(p$layers[[2]]$aes_params$fill, "white")
})

# Saving -----------------------------------------------------------------------

test_that("box and grey_blog backgrounds are used only where they belong", {
  skip_on_cran()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  both <- c("box", "grey_blog")

  save_type <- function(type, background = both, p = base_plot()) {
    grattan_save(file.path(dir, "chart.png"), p, type = type,
                 background = background, no_new_folder = TRUE)
    corner_colour(file.path(dir, paste0("chart_", type, ".png")))
  }

  expect_equal(save_type("normal"), toupper(box_orange))
  expect_equal(save_type("wholecolumn"), toupper(box_orange))
  expect_equal(save_type("fullslide"), "#FFFFFF")
  expect_equal(save_type("blog"), header_grey)
  expect_equal(save_type("blog", "white"), "#FFFFFF")

  # theme_grattan(background = "box") is orange only in report charts
  box_theme <- base_plot() + theme_grattan(background = "box")
  expect_equal(save_type("normal", "white", box_theme), toupper(box_orange))
  expect_equal(save_type("fullslide", "white", box_theme), "#FFFFFF")
  expect_equal(save_type("blog", "white", box_theme), "#FFFFFF")
})

test_that("web-ready charts are never box-coloured", {
  skip_on_cran()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()

  expect_message(
    grattan_save_web(file.path(dir, "web.png"), base_plot(), type = "normal",
                     background = "box", no_new_folder = TRUE),
    "only used for report charts"
  )
  expect_equal(corner_colour(file.path(dir, "web_normal.png")), "#FFFFFF")

  grattan_save_web(file.path(dir, "web.png"),
                   base_plot() + theme_grattan(background = "box"),
                   type = "blog", background = "grey_blog",
                   no_new_folder = TRUE)
  expect_equal(corner_colour(file.path(dir, "web_blog.png")), header_grey)
})

test_that("saying when a background doesn't apply", {
  skip_on_cran()
  dir <- withr::local_tempdir()

  expect_message(
    grattan_save(file.path(dir, "c.png"), base_plot(), type = "blog",
                 background = "box", no_new_folder = TRUE),
    "only used for report charts"
  )
  expect_message(
    grattan_save(file.path(dir, "c.png"), base_plot(), type = "normal",
                 background = "grey_blog", no_new_folder = TRUE),
    "only used for blog charts"
  )
  expect_no_message(
    grattan_save(file.path(dir, "c.png"), base_plot(), type = "normal",
                 background = "box", no_new_folder = TRUE)
  )
})

test_that("Powerpoint slides get the box and grey backgrounds", {
  skip_on_cran()
  dir <- withr::local_tempdir()

  slide_xml <- function(pptx) {
    files <- utils::unzip(pptx, exdir = withr::local_tempdir())
    slides <- sort(grep("ppt/slides/slide[0-9]+\\.xml$", files, value = TRUE))
    paste(readLines(utils::tail(slides, 1), warn = FALSE), collapse = "")
  }
  # The name of the layout the last slide uses
  last_layout <- function(pptx) {
    dir <- withr::local_tempdir()
    utils::unzip(pptx, exdir = dir)
    x <- officer::read_pptx(pptx)
    layout_file <- x$slide$get_slide(length(x))$layout_name()
    xml <- paste(readLines(file.path(dir, "ppt", "slideLayouts", layout_file),
                           warn = FALSE), collapse = "")
    sub('.*<p:cSld name="([^"]+)".*', "\\1", xml)
  }

  suppressMessages(
    grattan_save_pptx(p = base_plot(), filename = file.path(dir, "s.pptx"),
                      type = c("normal", "fullslide", "blog"),
                      background = c("box", "grey_blog"))
  )

  normal <- slide_xml(file.path(dir, "s_normal.pptx"))
  fullslide <- slide_xml(file.path(dir, "s_fullslide.pptx"))

  expect_match(normal, toupper(sub("#", "", box_orange)), fixed = TRUE)
  expect_false(grepl(toupper(sub("#", "", box_orange)), fullslide,
                     fixed = TRUE))

  # A grey blog slide uses the template's grey layout
  expect_equal(last_layout(file.path(dir, "s_blog.pptx")), "1_Two Content")
  expect_equal(last_layout(file.path(dir, "s_normal.pptx")), "Two Content")

  suppressMessages(
    grattan_save_pptx(p = base_plot(), filename = file.path(dir, "w.pptx"),
                      type = "blog")
  )
  expect_equal(last_layout(file.path(dir, "w_blog.pptx")), "Two Content")
})

test_that("check_chart() shows the background the chart will be saved with", {
  skip_on_cran()
  skip_if_not_installed("png")
  shown <- NULL
  local_mocked_bindings(file_show = function(path, ...) shown <<- path,
                        .package = "fs")

  check_chart("normal", base_plot(), background = "box")
  expect_equal(corner_colour(shown), toupper(box_orange))

  check_chart("blog", base_plot(), background = "grey_blog")
  expect_equal(corner_colour(shown), header_grey)
})
