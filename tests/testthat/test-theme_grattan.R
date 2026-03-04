context("Check that theme_grattan() and associated functions work")

# Create base object to use for test
base_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

p <- base_plot + theme_grattan()

p_scatter <- base_plot + theme_grattan(chart_type = "scatter")

test_that("theme_grattan() is a theme object", {
  expect_is(theme_grattan(), "theme")
})

test_that("plot with theme_grattan() is a ggplot2 object",{
  expect_is(p, "gg")
})

test_that("theme conforms with style guide in key ways",{
  expect_equal(p$theme$text$size, 18)

  expect_equal(p$theme$line$colour, "#C3C7CB")

  expect_equal(p$theme$rect$fill, "white")

})

test_that("theme_grattan() arguments work",{
  p_flipped <- base_plot + theme_grattan(flipped = TRUE)

  expect_null(p_flipped$theme$panel.grid.major.x)
  expect_is(p_flipped$theme$panel.grid.major.y, "ggplot2::element_blank")

  p_orange <- base_plot + theme_grattan(background = "orange")

  expect_equal(p_orange$theme$rect$fill, "#FEF0DE")

  expect_equal(p$theme$legend.position, "none")

  p_legend <- base_plot + theme_grattan(legend = "top")

  expect_equal(p_legend$theme$legend.position, "top")

  # Normal plot:
  expect_equal(class(p$theme$axis.line.y)[1], "ggplot2::element_blank")

  # Scatter plot:
  expect_null(p_scatter$theme$axis.line.y)

})


test_that("theme_grattan() sends the right messages",{
  expect_message(base_plot +
                  theme_grattan(chart_type = "scatter", flipped = TRUE))

  expect_warning(base_plot +
                   theme_grattan(chart_type = "nah mate"))

})


# --- Auto-flip detection tests ---

mt_flip <- mtcars
mt_flip$car <- rownames(mtcars)

test_that("theme_grattan() wraps return with grattan_theme class", {
  t <- theme_grattan()
  expect_true(inherits(t, "grattan_theme"))
  expect_true(inherits(t, "theme"))
  expect_false(is.null(attr(t, "grattan_args")))
})

test_that("auto-flip detects coord_flip()", {
  # Clear throttle so message fires

  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  expect_message(
    {
      p <- ggplot(mt_flip, aes(x = reorder(car, mpg), y = mpg)) +
        geom_col() +
        coord_flip() +
        theme_grattan()
    },
    "Auto-detected"
  )

  # Should have flipped theme elements
  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
  expect_null(p$theme$panel.grid.major.x)
})

test_that("auto-flip detects discrete y + continuous x", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  expect_message(
    {
      p <- ggplot(mt_flip, aes(y = reorder(car, mpg), x = mpg)) +
        geom_col() +
        theme_grattan()
    },
    "Auto-detected"
  )

  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
  expect_null(p$theme$panel.grid.major.x)
})

test_that("auto-flip detects layer-level mappings when plot mapping is empty", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  expect_message(
    {
      p <- ggplot() +
        geom_col(data = mt_flip, aes(y = reorder(car, mpg), x = mpg)) +
        theme_grattan()
    },
    "Auto-detected"
  )

  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
})

test_that("auto-flip does NOT trigger for vertical bar charts", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  # x is discrete, y is numeric → standard vertical bar chart
  p <- ggplot(mt_flip, aes(x = reorder(car, mpg), y = mpg)) +
    geom_col() +
    theme_grattan()

  # Non-flipped: x grid blank, y axis line blank
  expect_s3_class(p$theme$panel.grid.major.x, "element_blank")
  expect_s3_class(p$theme$axis.line.y, "element_blank")
})

test_that("auto-flip does NOT trigger for scatter chart_type", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  # Even with discrete y, scatter chart_type skips detection
  p <- ggplot(mt_flip, aes(x = mpg, y = factor(cyl))) +
    geom_point() +
    theme_grattan(chart_type = "scatter")

  expect_null(p$theme$panel.grid.major.y)
})

test_that("auto-flip does NOT trigger when flipped = TRUE is explicit", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  # No message when flipped is already set
  expect_silent({
    p <- ggplot(mt_flip, aes(y = reorder(car, mpg), x = mpg)) +
      geom_col() +
      theme_grattan(flipped = TRUE)
  })

  # Still has flipped elements
  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
})

test_that("auto-flip does NOT trigger when flipped = FALSE is explicit", {
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")

  # No message and no auto-flip when explicitly set to FALSE
  expect_silent({
    p <- ggplot(mt_flip, aes(y = reorder(car, mpg), x = mpg)) +
      geom_col() +
      theme_grattan(flipped = FALSE)
  })

  # Non-flipped theme elements
  expect_s3_class(p$theme$panel.grid.major.x, "element_blank")
  expect_s3_class(p$theme$axis.line.y, "element_blank")
})

test_that("auto-flip message is throttled", {
  # Set last message to now
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = as.character(Sys.time()))

  expect_silent(
    ggplot(mt_flip, aes(y = reorder(car, mpg), x = mpg)) +
      geom_col() +
      theme_grattan()
  )

  # Clean up
  Sys.setenv(GRATTANTHEME_FLIP_LAST_MESSAGE = "")
})

test_that("auto-flip handles after_stat() gracefully", {
  expect_no_error({
    p <- ggplot(mtcars, aes(x = factor(cyl), y = after_stat(count))) +
      geom_bar(stat = "count") +
      theme_grattan()
  })
})

test_that("auto-flip handles empty ggplot() gracefully", {
  expect_no_error({
    p <- ggplot() + theme_grattan()
  })
})
