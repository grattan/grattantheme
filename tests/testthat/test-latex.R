p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
     geom_point() +
     theme_grattan() +
     labs(title = "Title",
          subtitle = "Subtitle",
          caption = "Notes: say something. Sources: this citation.")

test_that("export_latex_code exports the right latex code", {

  skip_on_travis()

  export_latex_code(p)

  co <- clipr::read_clip(allow_non_interactive = TRUE)

  expect_equal(co[1], "\\begin{figure}")
  expect_equal(co[5], "\t\\noteswithsource{Say something.}{This citation.}")
  expect_equal(co[6], "\\end{figure}")

})
