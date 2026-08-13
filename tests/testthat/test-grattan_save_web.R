library(ggplot2)

web_test_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_grattan() +
  scale_colour_grattan(discrete = TRUE) +
  grattan_y_continuous(limits = c(0, 40)) +
  labs(title = "Smaller cars get better mileage",
       subtitle = "Miles-per-gallon by weight",
       caption = "Source: mtcars dataset")

test_that("grattan_save_web() produces a PNG and does not create PPTX/xlsx", {

  test_dir <- file.path(tempdir(), "grattan_save_web_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  out <- file.path(test_dir, "web_chart.png")
  grattan_save_web(out, object = web_test_plot,
                   type = "normal", no_new_folder = TRUE)

  # File exists (the internal filename suffix appends `_normal`)
  expect_true(file.exists(file.path(test_dir, "web_chart_normal.png")))

  # No pptx or xlsx files produced
  expect_length(list.files(test_dir, pattern = "\\.pptx$"), 0)
  expect_length(list.files(test_dir, pattern = "\\.xlsx$"), 0)
})

test_that("grattan_save_web() works with multiple chart types", {

  test_dir <- file.path(tempdir(), "grattan_save_web_types")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  for (tp in c("normal", "wholecolumn", "blog", "fullslide")) {
    out <- file.path(test_dir, paste0("chart_", tp, ".png"))
    grattan_save_web(out, object = web_test_plot, type = tp,
                     no_new_folder = TRUE)
    expected <- file.path(test_dir, paste0("chart_", tp, "_", tp, ".png"))
    expect_true(file.exists(expected), info = paste("type:", tp))
  }
})

test_that("grattan_save_web() errors on non-PNG filename", {
  test_dir <- file.path(tempdir(), "grattan_save_web_err")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_error(
    grattan_save_web(file.path(test_dir, "bad.pdf"), object = web_test_plot,
                     no_new_folder = TRUE),
    "PNG"
  )

  expect_error(
    grattan_save_web(file.path(test_dir, "bad.jpg"), object = web_test_plot,
                     no_new_folder = TRUE),
    "PNG"
  )
})

test_that("grattan_save_web() appends .png when extension missing", {
  test_dir <- file.path(tempdir(), "grattan_save_web_noext")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  grattan_save_web(file.path(test_dir, "chart"),
                   object = web_test_plot,
                   type = "normal",
                   no_new_folder = TRUE)
  expect_true(file.exists(file.path(test_dir, "chart_normal.png")))
})

test_that("grattan_save(save_web = TRUE) writes a web-ready PNG", {

  test_dir <- file.path(tempdir(), "grattan_save_web_wired")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  grattan_save(file.path(test_dir, "chart.pdf"),
               object = web_test_plot,
               type = "blog",
               save_web = TRUE)

  out_dir <- file.path(test_dir, "chart")

  expect_true(file.exists(file.path(out_dir, "chart_blog.png")))
  # The web PNG is additional to, not a replacement for, the main image
  expect_true(file.exists(file.path(out_dir, "chart_blog.pdf")))
})

test_that("grattan_save_all() writes web PNGs for the normal and blog types", {

  skip_on_cran()

  test_dir <- file.path(tempdir(), "grattan_save_all_web")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  grattan_save_all(file.path(test_dir, "chart.pdf"), object = web_test_plot)

  out_dir <- file.path(test_dir, "chart")

  expect_equal(sort(list.files(out_dir, pattern = "\\.png$")),
               sort(paste0("chart_", grattantheme:::web_chart_types, ".png")))

  # The other formats are unaffected
  expect_length(list.files(out_dir, pattern = "\\.pdf$"),
                length(grattantheme:::all_chart_types))
  expect_true(file.exists(file.path(out_dir, "chart.xlsx")))
})

test_that("grattan_save() errors on a non-logical save_web", {
  expect_error(grattan_save(file.path(tempdir(), "x.pdf"),
                            object = web_test_plot,
                            save_web = "yes"),
               "save_web must be either TRUE or FALSE")
})

test_that("grattan_save() warns when the web png would replace a standard png", {

  test_dir <- file.path(tempdir(), "grattan_save_web_clash")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  expect_warning(
    grattan_save(file.path(test_dir, "chart.png"),
                 object = web_test_plot,
                 type = "normal",
                 save_web = TRUE),
    "same filenames"
  )

  # No .pdf is produced, and the single png is the web version
  expect_equal(list.files(file.path(test_dir, "chart")), "chart_normal.png")
})
