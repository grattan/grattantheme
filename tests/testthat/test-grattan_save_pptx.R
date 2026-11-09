library(ggplot2)

# Run every test in this file from a temporary directory, so the Powerpoint
# files they write don't end up in the package source
withr::local_dir(withr::local_tempdir())

base_p1 <- mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()

p1 <-  base_p1 +
  labs(title = "My title",
       subtitle = "My subtitle",
       caption = "Notes: notes. Source: source")

p2 <- mtcars %>%
  ggplot(aes(x = mpg, y = wt)) +
  geom_point() +
  labs(title = "My title",
       subtitle = "My subtitle") +
  theme_grattan() +
  labs(title = "title",
       subtitle = "subtitle",
       caption = "Notes: notes. Source: source")

is_valid_pptx <- function(filename) {
  exists <- file.exists(filename)

  if (isFALSE(exists)) stop(filename, " does not exist")

  x <- officer::read_pptx(filename)

  if (isFALSE(length(x) > 0)) stop(filename, " does not contain any slides")
  if (isFALSE(inherits(x, "rpptx"))) stop(filename, " is not a valid pptx doc")

  TRUE
}

no_slides <- function(filename) {
  x <- officer::read_pptx(filename)
  length(x)
}

# The fonts used by the chart itself are recorded in the slide's DrawingML
pptx_typefaces <- function(filename) {
  slide_dir <- file.path(tempdir(), "pptx_typefaces")
  unlink(slide_dir, recursive = TRUE)
  utils::unzip(filename, exdir = slide_dir)
  xml <- readLines(file.path(slide_dir, "ppt", "slides", "slide1.xml"),
                   warn = FALSE)
  matches <- regmatches(xml, gregexpr('typeface="[^"]*"', xml))
  unique(gsub('typeface="|"', "", unlist(matches)))
}

test_that("add_graph_to_pptx adds ggplot2 object(s) to pptx shell", {

  skip_on_cran()

  add_graph_to_pptx(list(p1), "temp.pptx", "fullslide", num_slides = 1, font = "normal")
  expect_true(is_valid_pptx("temp.pptx"))

  x <- officer::read_pptx("temp.pptx")
  summ_x <- officer::slide_summary(x)

  expect_identical(summ_x$text[3], "My title")
  expect_identical(summ_x$text[1], "My subtitle")
  expect_identical(summ_x$text[4], "Notes: notes.\nSource: source")
  expect_identical(summ_x$text[2], "1015202530352345wtmpg")
})

test_that("grattan_save_pptx works in various configurations", {

  skip_on_cran()

  # fullslide_43 is deprecated, and warns, but must still export
  expect_warning(
    grattan_save_pptx(p = p1, filename = "temp.pptx", type = "fullslide_43"),
    "deprecated"
  )
  expect_true(is_valid_pptx("temp_fullslide_43.pptx"))
  expect_equal(no_slides("temp_fullslide_43.pptx"), 1)
  unlink("temp", recursive = T, force = T)

  grattan_save_pptx(p = list(p1, p2), filename = "temp.pptx", type = "fullslide")
  expect_true(is_valid_pptx("temp_fullslide.pptx"))
  expect_equal(no_slides("temp_fullslide.pptx"), 2)
  unlink("temp", recursive = T, force = T)


  expect_warning(
    grattan_save_pptx(p = list(p1, p2), filename = "multi_slide.pptx",
                      type = c("fullslide_43", "fullslide")),
    "deprecated"
  )
  expect_false(file.exists("multi_slide.pptx"))
  expect_true(is_valid_pptx("multi_slide_fullslide.pptx"))
  expect_true(is_valid_pptx("multi_slide_fullslide_43.pptx"))
  unlink("multi_slide", recursive = T)
})

test_that("grattan_save_pptx works when labels are present / absent", {

  skip_on_cran()
  # No labels
  grattan_save_pptx(p = base_p1, filename = "temp.pptx")
  expect_true(is_valid_pptx("temp_fullslide.pptx"))
  expect_equal(no_slides("temp_fullslide.pptx"), 1)
  unlink("temp", recursive = T, force = T)

  # Just title
  grattan_save_pptx(p = base_p1 + labs(title = "Some text"),
                    filename = "temp.pptx")
  expect_true(is_valid_pptx("temp_fullslide.pptx"))
  expect_equal(no_slides("temp_fullslide.pptx"), 1)
  unlink("temp", recursive = T, force = T)

  # Just subtitle
  grattan_save_pptx(p = base_p1 + labs(subtitle = "Some text"),
                    filename = "temp.pptx")
  expect_true(is_valid_pptx("temp_fullslide.pptx"))
  expect_equal(no_slides("temp_fullslide.pptx"), 1)
  unlink("temp", recursive = T, force = T)

})

test_that("grattan_save_pptx creates pptx with rich_subtitle = TRUE", {

  skip_on_cran()

  grattan_save_pptx(p = p1,
                    filename = "temp_rich.pptx",
                    type = "fullslide",
                    rich_subtitle = TRUE)

  expect_true(is_valid_pptx("temp_rich_fullslide.pptx"))
  expect_equal(no_slides("temp_rich_fullslide.pptx"), 1)

  unlink("temp_rich", recursive = TRUE, force = TRUE)
})

test_that("deprecated chart types are only accepted by grattan_save_pptx", {

  skip_on_cran()

  # Accepted here, with a warning, so old decks can be regenerated
  expect_warning(
    grattan_save_pptx(p = p1, filename = "legacy.pptx", type = "fullslide_old"),
    "deprecated"
  )
  expect_true(is_valid_pptx("legacy_fullslide_old.pptx"))
  unlink("legacy_fullslide_old.pptx", force = TRUE)

  # Rejected everywhere else, with a message pointing at grattan_save_pptx()
  expect_error(grattan_save(filename = file.path(tempdir(), "x.pdf"),
                            object = p1,
                            type = "fullslide_old"),
               "grattan_save_pptx")

  expect_error(save_chartdata(filename = file.path(tempdir(), "x.xlsx"),
                              object = p1,
                              type = "fullslide_old"),
               "grattan_save_pptx")

  expect_error(check_chart(type = "fullslide_old", object = p1),
               "grattan_save_pptx")

  expect_error(create_fullslide(plot = p1, type = "fullslide_old"),
               "not a valid fullslide chart type")

  # Defunct types are not accepted anywhere, including here
  expect_error(grattan_save_pptx(p = p1, filename = "x.pptx", type = "tiny"),
               "not one of the allowed types")

  # ... and the error says why, rather than treating them as a typo
  expect_error(grattan_save(filename = file.path(tempdir(), "x.pdf"),
                            object = p1,
                            type = "tiny"),
               "defunct")
})

test_that("blog pptx charts use the slide body font", {

  skip_on_cran()
  skip_if(get_grattan_font("slide", "body") == "sans",
          "slide font not available")

  pptx <- file.path(tempdir(), "font.pptx")
  grattan_save_pptx(pptx, p2, type = "blog")

  typefaces <- pptx_typefaces(file.path(tempdir(), "font_blog.pptx"))
  expect_true(get_grattan_font("slide", "body") %in% typefaces)
  expect_false("Arial" %in% typefaces)
})

test_that("all plots in a patchwork pptx chart use the same font", {

  skip_on_cran()

  pw_p1 <- ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) +
    geom_text() +
    theme_grattan(base_family = "mono")
  pw_p2 <- ggplot(mtcars, aes(x = hp, y = mpg, label = gear)) +
    geom_text() +
    theme_grattan()
  pw <- (pw_p1 + pw_p2) +
    patchwork::plot_annotation(title = "title", subtitle = "subtitle")

  pptx <- file.path(tempdir(), "patchwork.pptx")
  grattan_save_pptx(pptx, pw, type = "fullslide")

  typefaces <- pptx_typefaces(file.path(tempdir(), "patchwork_fullslide.pptx"))
  expect_length(typefaces, 1)
})

# Read the author and "last modified by" fields from an Office file's
# document properties (docProps/core.xml inside the zip archive)
doc_authors <- function(filename) {
  dir <- tempfile("core_")
  utils::unzip(filename, files = "docProps/core.xml", exdir = dir)
  core <- paste(readLines(file.path(dir, "docProps", "core.xml"),
                          warn = FALSE), collapse = "")
  unlink(dir, recursive = TRUE)
  field <- function(tag) {
    sub(paste0(".*<", tag, ">(.*?)</", tag, ">.*"), "\\1", core, perl = TRUE)
  }
  c(creator = field("dc:creator"), last_modified_by = field("cp:lastModifiedBy"))
}

test_that("grattan_save_pptx records Grattan Institute as the author", {
  filename <- file.path(tempdir(), "author.pptx")

  grattan_save_pptx(p = p1, filename = filename, type = "fullslide")

  # Only the author can be set: officer always records the login name as
  # "last modified by"
  expect_equal(doc_authors(file.path(tempdir(), "author_fullslide.pptx"))[["creator"]],
               "Grattan Institute")
})

test_that("grattan_save_pptx accepts a custom or blank author", {
  filename <- file.path(tempdir(), "author_custom.pptx")

  grattan_save_pptx(p = p1, filename = filename, type = "fullslide",
                    author = "Someone Else")
  expect_equal(doc_authors(file.path(tempdir(), "author_custom_fullslide.pptx"))[["creator"]],
               "Someone Else")

  grattan_save_pptx(p = p1, filename = filename, type = "fullslide",
                    author = "")
  expect_equal(doc_authors(file.path(tempdir(),
                                     "author_custom_fullslide.pptx"))[["creator"]],
               "")

  expect_error(grattan_save_pptx(p = p1, filename = filename,
                                 type = "fullslide", author = c("a", "b")),
               "`author` must be a single string")
})

test_that("trim_synced_path() keeps the shared part of synced paths", {
  paths <- c(
    "C:\\Users\\jo\\Grattan Institute Dropbox\\a.pptx",
    "/Users/jo/Library/CloudStorage/OneDrive-SharedLibraries-Grattan/Team/a.pptx",
    "C:\\Users\\jo\\Grattan Institute\\Team\\a.pptx",
    "//server/share/a.pptx"
  )
  expected <- c(
    "Dropbox/a.pptx",                   # Dropbox
    "Team/a.pptx",                      # SharePoint on a Mac
    "Grattan Institute/Team/a.pptx",    # SharePoint on Windows
    "//server/share/a.pptx"             # outside the home folder: unchanged
  )
  expect_equal(vapply(paths, trim_synced_path, "", USE.NAMES = FALSE), expected)
})
