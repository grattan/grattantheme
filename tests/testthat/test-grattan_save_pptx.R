library(ggplot2)

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

test_that("create_pptx_shell creates an empty pptx document with the appropriate number of slides", {
  skip("Deprecated")

  # Plot argument to create_pptx_shell() must be a list of ggplot2 object(s)
  expect_error(create_pptx_shell(p1,
                                 "test.pptx",
                                 type = "fullslide_43"))

  create_pptx_shell(list(p1, p2), "temp.pptx", "fullslide_43")
  on.exit(unlink("temp.pptx"))
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 2)

  create_pptx_shell(list(p1, p2), "temp.pptx", "fullslide")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 2)

  create_pptx_shell(list(p1), "temp.pptx", "fullslide")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 1)
})

test_that("add_graph_to_pptx adds ggplot2 object(s) to pptx shell", {

  skip_on_cran()

  add_graph_to_pptx(list(p1), "temp.pptx", "fullslide", num_slides = 1)
  expect_true(is_valid_pptx("temp.pptx"))

  x <- officer::read_pptx("temp.pptx")
  summ_x <- officer::slide_summary(x)

  expect_identical(summ_x$text[3], "My title")
  expect_identical(summ_x$text[1], "My subtitle")
  expect_identical(summ_x$text[4], "Notes: notes. \nSource: source")
  expect_identical(summ_x$text[2], "1015202530352345wtmpg")
})

test_that("grattan_save_pptx works in various configurations", {

  skip_on_cran()
  grattan_save_pptx(p = p1, filename = "temp.pptx", type = "fullslide_43")
  expect_true(is_valid_pptx("temp_fullslide_43.pptx"))
  expect_equal(no_slides("temp_fullslide_43.pptx"), 1)
  unlink("temp", recursive = T, force = T)

  grattan_save_pptx(p = list(p1, p2), filename = "temp.pptx", type = "fullslide")
  expect_true(is_valid_pptx("temp_fullslide.pptx"))
  expect_equal(no_slides("temp_fullslide.pptx"), 2)
  unlink("temp", recursive = T, force = T)


  grattan_save_pptx(p = list(p1, p2), filename = "multi_slide.pptx",
                    type = c("fullslide_43", "fullslide"))
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
