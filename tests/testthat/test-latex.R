p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
     geom_point() +
     theme_grattan() +
     labs(title = "Title",
          subtitle = "Subtitle",
          caption = "Notes: say something. Sources: this citation.")

test_that("export_latex_code exports the right latex code to clipboard", {

  skip_on_ci()
  skip_on_travis()

  if (!clipr::clipr_available()) {
    skip("clipr not available")
  }

  export_latex_code(p)

  co <- clipr::read_clip(allow_non_interactive = TRUE)

  expect_equal(co[1], "\\begin{figure}")
  expect_equal(co[5], "\t\\noteswithsource{Say something.}{This citation.}")
})


test_that("export_latex_code returns the right latex code", {

  skip_on_cran()
  x <- export_latex_code(p)
  text <- "\\begin{figure}\n\t\\caption{Title\\label{fig:chart}}\n\t\\units{Subtitle}\n\t\\includegraphics[page= 1, width=1\\columnwidth]{atlas/chart/chart_wholecolumn.pdf}\n\t\\noteswithsource{Say something.}{This citation.}\n\\end{figure}"

  expect_identical(x, text)

})
