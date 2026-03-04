test_that("grattan_write_ai_guide creates file with default name", {
  tmp <- tempdir()
  on.exit(unlink(file.path(tmp, "CLAUDE.md")))

  result <- grattan_write_ai_guide(path = tmp)

  expect_true(file.exists(file.path(tmp, "CLAUDE.md")))
  expect_equal(result, normalizePath(file.path(tmp, "CLAUDE.md")))
})

test_that("grattan_write_ai_guide creates file with custom name", {
  tmp <- tempdir()
  on.exit(unlink(file.path(tmp, "AGENTS.md")))

  result <- grattan_write_ai_guide("AGENTS.md", path = tmp)

  expect_true(file.exists(file.path(tmp, "AGENTS.md")))
})

test_that("grattan_write_ai_guide does not overwrite by default", {
  tmp <- tempdir()
  filepath <- file.path(tmp, "CLAUDE.md")
  on.exit(unlink(filepath))

  writeLines("existing content", filepath)

  expect_message(
    grattan_write_ai_guide(path = tmp),
    "already exists"
  )

  expect_equal(readLines(filepath), "existing content")
})

test_that("grattan_write_ai_guide overwrites when requested", {
  tmp <- tempdir()
  filepath <- file.path(tmp, "CLAUDE.md")
  on.exit(unlink(filepath))

  writeLines("existing content", filepath)

  grattan_write_ai_guide(path = tmp, overwrite = TRUE)

  content <- readLines(filepath)
  expect_true(any(grepl("Grattan", content)))
  expect_false(identical(content, "existing content"))
})

test_that("grattan_write_ai_guide content includes key sections", {
  tmp <- tempdir()
  filepath <- file.path(tmp, "CLAUDE.md")
  on.exit(unlink(filepath))

  grattan_write_ai_guide(path = tmp, overwrite = TRUE)

  content <- paste(readLines(filepath), collapse = "\n")

  expect_true(grepl("theme_grattan", content))
  expect_true(grepl("grattan_save", content))
  expect_true(grepl("grattan_label", content))
  expect_true(grepl("Colour Palette", content))
  expect_true(grepl("check_chart", content))
})

test_that("grattan_write_ai_guide creates directory if needed", {
  tmp <- file.path(tempdir(), "new_subdir_test")
  on.exit(unlink(tmp, recursive = TRUE))

  grattan_write_ai_guide(path = tmp)

  expect_true(file.exists(file.path(tmp, "CLAUDE.md")))
})
