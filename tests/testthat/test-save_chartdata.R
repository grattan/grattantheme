
library(patchwork)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
     geom_point() +
     theme_grattan() +
     labs(title = "Title",
          subtitle = "Subtitle",
          caption = "Caption")

test_that("save_chartdata creates an xlsx workbook", {

  expect_invisible(save_chartdata("save_chartdata_test.xlsx", p))

  expect_true(file.exists("save_chartdata_test.xlsx"))

  unlink("../testthat/save_chartdata_test.xlsx")

  if(file.exists("../testthat/Rplots.pdf")) {
    unlink("../testthat/Rplots.pdf")
  }

})

test_that("save_chartdata works with patchwork plots", {

  # Create two simple plots with the same data
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan()

  p2 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point() +
    theme_grattan()

  # Combine with patchwork
  p_combined <- p1 + p2 +
    plot_annotation(title = "Combined plots",
                    subtitle = "Two charts side by side",
                    caption = "Source: mtcars")

  save_chartdata("patchwork_test.xlsx", p_combined, select_data = FALSE)

  expect_true(file.exists("patchwork_test.xlsx"))

  # Get the data used in each component plot
  p1_data <- p1$data
  p2_data <- p2$data

  # Calculate positions for reading the two data boxes
  # First box starts at row 3, column 2
  first_box_rows <- nrow(p1_data)
  # Second box starts 2 rows after first box ends
  second_box_start <- 3 + first_box_rows + 2

  # Read first data box
  saved_data_1 <- openxlsx::read.xlsx("patchwork_test.xlsx",
                                      rows = 3:(3 + first_box_rows),
                                      cols = 2:(ncol(p1_data)+1))

  # Read second data box
  saved_data_2 <- openxlsx::read.xlsx("patchwork_test.xlsx",
                                      rows = second_box_start:(second_box_start + nrow(p2_data)),
                                      cols = 2:(ncol(p2_data)+1))

  # Make column names lowercase to match
  names(saved_data_1) <- tolower(names(saved_data_1))
  names(saved_data_2) <- tolower(names(saved_data_2))

  # Remove named rows
  rownames(p1_data) <- NULL
  rownames(p2_data) <- NULL

  # Compare data
  expect_equal(saved_data_1, p1_data)
  expect_equal(saved_data_2, p2_data)

  unlink("patchwork_test.xlsx")

  if(file.exists("Rplots.pdf")) {
    unlink("Rplots.pdf")
  }
})

test_that("save_chartdata recovers data supplied directly to a geom", {

  df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 6))

  # No data in the ggplot() call - it all lives in the geom
  p_geom <- ggplot() +
    geom_point(data = df, aes(x, y)) +
    theme_grattan()

  expect_warning(save_chartdata("geom_only.xlsx", p_geom),
                 "extracted from the data supplied directly")

  expect_true(file.exists("geom_only.xlsx"))

  saved <- openxlsx::read.xlsx("geom_only.xlsx", rows = 3:8, cols = 2:3)
  names(saved) <- tolower(names(saved))
  expect_equal(saved, df)

  unlink("geom_only.xlsx")
  if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
})

test_that("save_chartdata writes a block per distinct geom data table", {

  df1 <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 6))
  df2 <- data.frame(a = 1:3, b = c(9, 8, 7))

  p_multi <- ggplot() +
    geom_point(data = df1, aes(x, y)) +
    geom_line(data = df2, aes(a, b)) +
    theme_grattan()

  expect_warning(save_chartdata("geom_multi.xlsx", p_multi, select_data = FALSE),
                 "2 different data tables")

  expect_true(file.exists("geom_multi.xlsx"))

  # First block: df1 at rows 3:8, cols 2:3
  block1 <- openxlsx::read.xlsx("geom_multi.xlsx", rows = 3:8, cols = 2:3)
  names(block1) <- tolower(names(block1))
  expect_equal(block1, df1)

  # Second block starts 2 rows after the first block ends
  second_start <- 3 + nrow(df1) + 2
  block2 <- openxlsx::read.xlsx("geom_multi.xlsx",
                                rows = second_start:(second_start + nrow(df2)),
                                cols = 2:3)
  names(block2) <- tolower(names(block2))
  expect_equal(block2, df2)

  unlink("geom_multi.xlsx")
  if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
})

test_that("save_chartdata still errors when there is no data anywhere", {

  p_empty <- ggplot() + theme_grattan()

  expect_error(save_chartdata("no_data.xlsx", p_empty),
               "No valid data found")

  if (file.exists("no_data.xlsx")) unlink("no_data.xlsx")
  if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
})

test_that("save_chartdata handles patchwork without proper annotations", {

  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_grattan()

  p2 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point() +
    theme_grattan()

  # Combine without annotations
  p_no_annotations <- p1 + p2

  expect_warning(save_chartdata("patchwork_no_annotations.xlsx", p_no_annotations),
                 "No subtitle found")
  expect_warning(save_chartdata("patchwork_no_annotations.xlsx", p_no_annotations),
                 "No caption found")

  expect_true(file.exists("patchwork_no_annotations.xlsx"))

  unlink("patchwork_no_annotations.xlsx")

  if(file.exists("Rplots.pdf")) {
    unlink("Rplots.pdf")
  }
})
