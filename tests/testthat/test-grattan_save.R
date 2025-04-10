library(ggplot2)
library(openxlsx)
library(magick)

test_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_grattan() +
  scale_colour_grattan(discrete = TRUE) +
  grattan_y_continuous(limits = c(0, 40)) +
  labs(x = "Weight",
       title = "Smaller cars get better mileage",
       subtitle = "Miles-per-gallon by weight",
       caption = "Source: mtcars dataset")

test_plot_nolabs <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_grattan() +
  scale_colour_grattan(discrete = TRUE) +
  grattan_y_continuous(limits = c(0, 40))

test_plot_longlabs <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_grattan() +
  scale_colour_grattan(discrete = TRUE) +
  grattan_y_continuous(limits = c(0, 40)) +
  labs(x = "Weight",
       title = "Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage",
       subtitle = "Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage Smaller cars get better mileage",
       caption = "Source: mtcars dataset")


test_that("grattan_save() saves charts (no powerpoint)", {
  
  test_dir <- file.path(tempdir(), "grattan_save_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  grattan_save(filename = file.path(test_dir, "test_plot.png"),
               object = test_plot,
               save_pptx = FALSE,
               save_data = FALSE,
               type = "all")

  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_fullslide.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_fullpage.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_normal_169.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_normal.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_tiny.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_wholecolumn.png")))
  expect_true(file.exists(file.path(test_dir, "test_plot", "test_plot_a4.png")))
  
  expect_false(file.exists(file.path(test_dir, "test_plot", "test_plot_fullslide_43.png")))
  expect_false(file.exists(file.path(test_dir, "test_plot.png", "test_plot_blog_half.png")))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("grattan_save() saves charts (with powerpoint)", {

  skip_on_cran()
  
  test_dir <- file.path(tempdir(), "grattan_save_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  grattan_save(filename = file.path(test_dir, "test_plot.png"),
               object = test_plot,
               save_pptx = TRUE,
               save_data = TRUE,
               type = "all")
  
  output_dir <- file.path(test_dir, "test_plot")
  
  expect_true(file.exists(file.path(output_dir, "test_plot_fullslide.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullpage.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal_169.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_tiny.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_wholecolumn.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullslide.pptx")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullpage.pptx")))
  
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_43.png")))
  expect_false(file.exists(file.path(output_dir, "test_plot_blog_half.png")))
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_44.png")))

  unlink(test_dir, recursive = TRUE)
  
})

test_that("grattan_save() saves last_plot() and works with repeated calls", {
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  skip_on_cran()
  
  test_dir <- file.path(tempdir(), "grattan_save_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  grattan_save(file.path(test_dir, "test.png"), type = "all", save_pptx = TRUE)
  # Test that a repeated save causes no problems
  grattan_save(file.path(test_dir, "test.png"), type = "all", save_pptx = TRUE)

  expect_true(file.exists(file.path(test_dir, "test", "test_fullslide.png")))

  unlink(test_dir, recursive = TRUE)
  
})


test_that("grattan_save() doesn't save chart data / PPTX when not requested", {

  skip_on_cran()

  test_dir <- file.path(tempdir(), "grattan_save_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  
  grattan_save(filename = file.path(test_dir, "test_plot.png"),
               object = test_plot,
               type = "all")
  
  output_dir <- file.path(test_dir, "test_plot")
  
  expect_true(file.exists(file.path(output_dir, "test_plot_fullslide.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullpage.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal_169.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_tiny.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_wholecolumn.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_a4.png")))
  
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_43.png")))
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide.pptx")))
  expect_false(file.exists(file.path(output_dir, "test_plot.xlsx")))
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_44.png")))

  unlink(test_dir, recursive = TRUE)
  
})

test_that("grattan_save() saves chart data when requested",{
  grattan_save(filename = "test.pdf",
               object = test_plot,
               force_labs = FALSE,
               type = "normal",
               save_data = TRUE,
               select_data = FALSE)

  expect_true(file.exists("test/test_normal.pdf"))
  expect_true(file.exists("test/test.xlsx"))

  saved_data <- openxlsx::read.xlsx("test/test.xlsx",
                                    rows = c(3:35),
                                    cols = c(2:12))

  names(saved_data) <- tolower(names(saved_data))

  mtcars_no_rownames <- mtcars
  rownames(mtcars_no_rownames) <- NULL

  expect_is(saved_data, "data.frame")
  expect_identical(saved_data, mtcars_no_rownames)

  unlink("test.pdf")
  unlink("test.xlsx")

})

test_that("grattan_save() height behaviour works as expected with normal charts", {

  grattan_save(filename = "test_plot_normal_default_height.png",
               object = test_plot,
               type = "normal",
               device = 'png')

  grattan_save(filename = "test_plot_normal_manual_height.png",
               object = test_plot,
               type = "normal",
               height = 20, 
               device = 'png')

  expect_true(file.exists("test_plot_normal_default_height/test_plot_normal_default_height_normal.png"))
  expect_true(file.exists("test_plot_normal_manual_height/test_plot_normal_manual_height_normal.png"))

  default_img <- magick::image_read("test_plot_normal_default_height/test_plot_normal_default_height_normal.png")
  manual_img <- magick::image_read("test_plot_normal_manual_height/test_plot_normal_manual_height_normal.png")
  
  default_info <- magick::image_info(default_img)
  manual_info <- magick::image_info(manual_img)
  
  expect_gt(manual_info$height, default_info$height)
  
  unlink("test_plot_normal_default_height", recursive = TRUE)
  unlink("test_plot_normal_manual_height", recursive = TRUE)

})

test_that("grattan_save() height behaviour works as expected with fullslide charts", {

  grattan_save(filename = "default_height.png",
               object = test_plot,
               type = "fullslide",
               device = 'png')

  grattan_save(filename = "manual_height.png",
               object = test_plot,
               type = "fullslide",
               height = 40,
               device = 'png')

  expect_true(file.exists("default_height/default_height_fullslide.png"))
  expect_true(file.exists("manual_height/manual_height_fullslide.png"))

  expect_gt(file.size("manual_height/manual_height_fullslide.png"),
            file.size("default_height/default_height_fullslide.png"))

  unlink("default_height", recursive = TRUE)
  unlink("manual_height", recursive = TRUE)

})

test_that("grattan_save() sends the right messages",{
  expect_error(
  grattan_save(filename = "test_plot_normal_default_height.png",
               object = test_plot,
               type = "nahhhh mate")
  )

})

test_that("grattan_save() saves a plot with a watermark", {

  grattan_save(filename = "test_plot_watermark.png",
               object = test_plot,
               watermark = "DRAFT")

  expect_true(file.exists("test_plot_watermark/test_plot_watermark_normal.png"))


  unlink("test_plot_watermark", recursive = TRUE)
})

# I'm going to update the grattan_save_all works test_that function so that it works with a temp_dir as per the tests above.

test_that("grattan_save_all() works", {
  
  skip_on_cran()
  
  test_dir <- file.path(tempdir(), "grattan_save_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  
  grattan_save_all(filename = file.path(test_dir, "test_plot.png"),
                   object = test_plot)
  
  output_dir <- file.path(test_dir, "test_plot")
  
  expect_true(file.exists(file.path(output_dir, "test_plot_fullslide.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullpage.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal_169.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_normal.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_tiny.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot_wholecolumn.png")))
  expect_true(file.exists(file.path(output_dir, "test_plot.xlsx")))
  expect_true(file.exists(file.path(output_dir, "test_plot_fullslide.pptx")))
  
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_43.png")))
  expect_false(file.exists(file.path(output_dir, "test_plot_blog_half.png")))
  expect_false(file.exists(file.path(output_dir, "test_plot_fullslide_44.png")))
  
  unlink(test_dir, recursive = TRUE)
  
})



test_that("grattan_save(ignore_long_titles = TRUE) successfully ignores long titles", {

  expect_error(
    grattan_save(filename = "default_height.png",
               object = test_plot_longlabs,
               type = "fullslide")
  )

  grattan_save(filename = "default_height.png",
               object = test_plot_longlabs,
               type = "fullslide",
               ignore_long_title = TRUE)
})
