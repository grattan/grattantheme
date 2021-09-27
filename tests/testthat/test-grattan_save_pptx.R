library(ggplot2)

base_p1 <- mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()

p1 <-  base_p1 +
  labs(title = "My title",
       subtitle = "My subtitle")

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
  skip_on_cran()

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

  create_pptx_shell(list(p1), "temp.pptx", "fullslide_43")
  on.exit(unlink("temp.pptx"))

  add_graph_to_pptx(list(p1), "temp.pptx", "fullslide_43")
  expect_true(is_valid_pptx("temp.pptx"))

  x <- officer::read_pptx("temp.pptx")
  summ_x <- officer::slide_summary(x)

  expect_identical(summ_x$text[1], "My title")
  expect_identical(summ_x$text[2], "My subtitle")
  expect_identical(summ_x$text[3], "1015202530352345wtmpg")
})

test_that("grattan_save_pptx works in various configurations", {

  skip_on_cran()
  grattan_save_pptx(p1, "temp.pptx", type = "fullslide_43")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 1)
  unlink("temp.pptx", recursive = T, force = T)

  grattan_save_pptx(list(p1, p2), "temp.pptx", type = "fullslide_43")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 2)
  unlink("temp.pptx", recursive = T, force = T)


  grattan_save_pptx(list(p1, p2), "multi_slide.pptx",
                    type = c("fullslide_43", "fullslide"))
  expect_false(file.exists("multi_slide.pptx"))
  expect_true(is_valid_pptx("multi_slide/multi_slide_fullslide.pptx"))
  expect_true(is_valid_pptx("multi_slide/multi_slide_fullslide.pptx"))
  unlink("multi_slide", recursive = T)
})

test_that("grattan_save_pptx works when labels are present / absent", {

  skip_on_cran()
  # No labels
  grattan_save_pptx(base_p1, "temp.pptx")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 1)
  unlink("temp.pptx", recursive = T, force = T)

  # Just title
  grattan_save_pptx(base_p1 + labs(title = "Some text"),
                    "temp.pptx")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 1)
  unlink("temp.pptx", recursive = T, force = T)

  # Just subtitle
  grattan_save_pptx(base_p1 + labs(subtitle = "Some text"),
                    "temp.pptx")
  expect_true(is_valid_pptx("temp.pptx"))
  expect_equal(no_slides("temp.pptx"), 1)
  unlink("temp.pptx", recursive = T, force = T)

})
