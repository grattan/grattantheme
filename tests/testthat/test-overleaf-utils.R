context("Check that Overleaf atlas identifiers work correctly")


test_that("find_overleaf_projects returns project names", {
  # Create a mock Overleaf directory structure
  temp_overleaf <- file.path(tempdir(), "mock_overleaf")
  dir.create(temp_overleaf, showWarnings = FALSE)
  dir.create(file.path(temp_overleaf, "Project-A"), showWarnings = FALSE)
  dir.create(file.path(temp_overleaf, "Project-B"), showWarnings = FALSE)

  projects <- find_overleaf_projects(temp_overleaf)

  expect_equal(sort(projects), c("Project-A", "Project-B"))

  unlink(temp_overleaf, recursive = TRUE)
})

test_that("set_overleaf_project finds exact matches", {
  skip_on_cran()

  # Create mock structure
  temp_base <- file.path(tempdir(), "mock_overleaf")
  dir.create(temp_base, showWarnings = FALSE)
  dir.create(file.path(temp_base, "Democratic-Resilience"), showWarnings = FALSE)
  dir.create(file.path(temp_base, "Transport-Report"), showWarnings = FALSE)

  # Mock get_overleaf_base_path to return our temp directory
  with_mocked_bindings(
    get_overleaf_base_path = function() temp_base,
    {
      result <- set_overleaf_project("Democratic-Resilience")
      expect_true(grepl("Democratic-Resilience", result))
      expect_equal(Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT"), "Democratic-Resilience")
    }
  )

  unlink(temp_base, recursive = TRUE)
})

test_that("set_overleaf_project finds partial matches", {
  skip_on_cran()

  temp_base <- file.path(tempdir(), "mock_overleaf")
  dir.create(temp_base, showWarnings = FALSE)
  dir.create(file.path(temp_base, "EPaD-2026-Democratic-Resilience"), showWarnings = FALSE)
  dir.create(file.path(temp_base, "Transport-Report"), showWarnings = FALSE)

  with_mocked_bindings(
    get_overleaf_base_path = function() temp_base,
    {
      result <- set_overleaf_project("democratic")
      expect_true(grepl("Democratic-Resilience", result))
    }
  )

  unlink(temp_base, recursive = TRUE)
})

test_that("set_overleaf_project errors on multiple matches", {
  skip_on_cran()

  temp_base <- file.path(tempdir(), "mock_overleaf")
  dir.create(temp_base, showWarnings = FALSE)
  dir.create(file.path(temp_base, "Democratic-Resilience"), showWarnings = FALSE)
  dir.create(file.path(temp_base, "Democratic-Institutions"), showWarnings = FALSE)

  with_mocked_bindings(
    get_overleaf_base_path = function() temp_base,
    {
      expect_error(
        set_overleaf_project("democratic"),
        "Multiple Overleaf projects found"
      )
    }
  )

  unlink(temp_base, recursive = TRUE)
})

test_that("set_overleaf_project errors on no matches", {
  skip_on_cran()

  temp_base <- file.path(tempdir(), "mock_overleaf")
  dir.create(temp_base, showWarnings = FALSE)
  dir.create(file.path(temp_base, "Transport-Report"), showWarnings = FALSE)

  with_mocked_bindings(
    get_overleaf_base_path = function() temp_base,
    {
      expect_error(
        set_overleaf_project("nonexistent"),
        "No Overleaf projects found matching"
      )
    }
  )

  unlink(temp_base, recursive = TRUE)
})

test_that("set_overleaf_project accepts full paths", {
  skip_on_cran()

  temp_project <- file.path(tempdir(), "Apps", "Overleaf", "My-Project")
  dir.create(temp_project, recursive = TRUE, showWarnings = FALSE)

  result <- set_overleaf_project(temp_project)

  expect_equal(result, temp_project)
  expect_equal(Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT"), "My-Project")
  expect_equal(Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT_PATH"), temp_project)

  unlink(file.path(tempdir(), "Apps"), recursive = TRUE)
})

test_that("get_overleaf_project creates atlas directory", {
  skip_on_cran()

  temp_project <- file.path(tempdir(), "Test-Project")
  dir.create(temp_project, showWarnings = FALSE)

  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT = "Test-Project")
  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT_PATH = temp_project)

  atlas_path <- get_overleaf_project()

  expect_true(dir.exists(atlas_path))
  expect_true(grepl("atlas$", atlas_path))

  unlink(temp_project, recursive = TRUE)
})

test_that("grattan_save_overleaf saves to correct location", {
  skip_on_cran()

  temp_project <- file.path(tempdir(), "Test-Project")
  temp_atlas <- file.path(temp_project, "atlas")
  dir.create(temp_atlas, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT = "Test-Project")
  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT_PATH = temp_project)

  # Create a simple test plot
  library(ggplot2)
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  grattan_save_overleaf("test_chart.pdf", object = test_plot)

  expect_true(file.exists(file.path(temp_atlas, "test_chart_normal.pdf")))

  unlink(temp_project, recursive = TRUE)
})

test_that("grattan_save_overleaf uses last_plot by default", {
  skip_on_cran()

  temp_project <- file.path(tempdir(), "Test-Project")
  temp_atlas <- file.path(temp_project, "atlas")
  dir.create(temp_atlas, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT = "Test-Project")
  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT_PATH = temp_project)

  library(ggplot2)
  ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  # Should use last_plot without specifying object
  grattan_save_overleaf("last_plot_test.pdf")

  expect_true(file.exists(file.path(temp_atlas, "last_plot_test_normal.pdf")))

  unlink(temp_project, recursive = TRUE)
})
