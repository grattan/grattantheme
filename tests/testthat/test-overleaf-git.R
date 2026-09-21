context("Check that Overleaf git syncing works correctly")

# Helpers ---------------------------------------------------------------------

# A bare repo standing in for the Overleaf git bridge, seeded with an atlas
# folder so it looks like a real project.
make_fake_overleaf <- function(env = parent.frame()) {

  remote <- file.path(tempfile("overleaf-remote-"))
  gert::git_init(remote, bare = TRUE)

  seed <- file.path(tempfile("overleaf-seed-"))
  gert::git_clone(remote, seed, verbose = FALSE)
  gert::git_config_set("user.name", "Seed", repo = seed)
  gert::git_config_set("user.email", "seed@example.com", repo = seed)

  dir.create(file.path(seed, "atlas"))
  writeLines("\\documentclass{article}", file.path(seed, "main.tex"))
  writeLines("placeholder", file.path(seed, "atlas", "existing.txt"))

  gert::git_add(c("main.tex", "atlas"), repo = seed)
  gert::git_commit("Initial commit", repo = seed)
  gert::git_push(repo = seed, verbose = FALSE)

  withr::defer(unlink(c(remote, seed), recursive = TRUE), envir = env)

  list(remote = remote, seed = seed)
}

# Point the package at a fake project, as set_overleaf_project() would
use_fake_project <- function(remote, env = parent.frame()) {

  cache <- file.path(tempfile("overleaf-cache-"))
  clone <- file.path(cache, "fakeproject")

  withr::local_envvar(
    GRATTANTHEME_OVERLEAF_CACHE = cache,
    GRATTANTHEME_OVERLEAF_BACKEND = "git",
    GRATTANTHEME_OVERLEAF_PROJECT = "fakeproject",
    GRATTANTHEME_OVERLEAF_PROJECT_PATH = clone,
    OVERLEAF_TOKEN = "not-a-real-token",
    .local_envir = env
  )

  overleaf_ensure_clone(path = clone, remote = remote)

  withr::defer(unlink(cache, recursive = TRUE), envir = env)

  clone
}

# What does the remote actually contain?
remote_atlas_files <- function(remote) {
  check <- file.path(tempfile("overleaf-check-"))
  on.exit(unlink(check, recursive = TRUE))
  gert::git_clone(remote, check, verbose = FALSE)
  list.files(file.path(check, "atlas"))
}


# Recognising project URLs ----------------------------------------------------

test_that("is_overleaf_project recognises URLs and IDs", {

  expect_true(is_overleaf_project(
    "https://www.overleaf.com/project/0123456789abcdef01234567"))
  expect_true(is_overleaf_project(
    "http://overleaf.com/project/0123456789abcdef01234567"))
  expect_true(is_overleaf_project(
    "https://www.overleaf.com/project/0123456789abcdef01234567/edit"))
  expect_true(is_overleaf_project(
    "https://git.overleaf.com/0123456789abcdef01234567"))
  expect_true(is_overleaf_project("0123456789abcdef01234567"))
})

test_that("is_overleaf_project ignores Dropbox project names and paths", {

  expect_false(is_overleaf_project("Orange"))
  expect_false(is_overleaf_project("Orange-Book-2025"))
  expect_false(is_overleaf_project("/Users/me/Dropbox/Apps/Overleaf/My Report"))
  expect_false(is_overleaf_project(NULL))
  expect_false(is_overleaf_project(NA))
  expect_false(is_overleaf_project(c("a", "b")))
})

test_that("overleaf_project_id extracts the ID", {

  expect_equal(
    overleaf_project_id("https://www.overleaf.com/project/0123456789abcdef01234567"),
    "0123456789abcdef01234567")

  expect_equal(
    overleaf_project_id("https://www.overleaf.com/project/0123456789abcdef01234567/edit?x=1"),
    "0123456789abcdef01234567")

  expect_equal(overleaf_project_id("0123456789abcdef01234567"),
               "0123456789abcdef01234567")

  expect_error(overleaf_project_id("Orange-Book"),
               "Could not find an Overleaf project ID")
})

test_that("overleaf_remote_url builds the git bridge URL", {
  expect_equal(overleaf_remote_url("0123456789abcdef01234567"),
               "https://git@git.overleaf.com/0123456789abcdef01234567")
})


# Commit messages -------------------------------------------------------------

test_that("overleaf_commit_message names charts, then counts them", {

  expect_equal(overleaf_commit_message("atlas/figure1_normal.pdf"),
               "Update figure1_normal.pdf (grattantheme)")

  expect_equal(overleaf_commit_message(c("atlas/a.pdf", "atlas/b.pdf")),
               "Update a.pdf, b.pdf (grattantheme)")

  expect_equal(overleaf_commit_message(paste0("atlas/chart", 1:5, ".pdf")),
               "Update 5 charts (grattantheme)")
})


# Cloning and syncing ---------------------------------------------------------

test_that("overleaf_ensure_clone clones once and reuses the clone", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  expect_true(dir.exists(file.path(clone, ".git")))
  expect_true(file.exists(file.path(clone, "atlas", "existing.txt")))

  # A second call is a no-op rather than a re-clone
  marker <- file.path(clone, "marker.txt")
  writeLines("still here", marker)
  overleaf_ensure_clone(path = clone, remote = fake$remote)
  expect_true(file.exists(marker))
})

test_that("overleaf_sync pushes a new chart to the remote", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  writeLines("chart", file.path(clone, "atlas", "figure1_normal.pdf"))

  expect_true(overleaf_sync())
  expect_true("figure1_normal.pdf" %in% remote_atlas_files(fake$remote))
})

test_that("overleaf_sync does nothing when the chart hasn't changed", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  writeLines("chart", file.path(clone, "atlas", "figure1_normal.pdf"))
  overleaf_sync()

  commits_before <- nrow(gert::git_log(repo = clone))

  # Saving an identical chart shouldn't produce a commit
  writeLines("chart", file.path(clone, "atlas", "figure1_normal.pdf"))
  expect_false(overleaf_sync())
  expect_equal(nrow(gert::git_log(repo = clone)), commits_before)
})

test_that("overleaf_sync(push = FALSE) commits but doesn't push", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  writeLines("chart", file.path(clone, "atlas", "figure1_normal.pdf"))

  expect_false(suppressMessages(overleaf_sync(push = FALSE)))
  expect_false("figure1_normal.pdf" %in% remote_atlas_files(fake$remote))

  # ... and overleaf_push() sends it
  expect_true(overleaf_push())
  expect_true("figure1_normal.pdf" %in% remote_atlas_files(fake$remote))
})

test_that("overleaf_push batches several charts into one push", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  for (i in 1:3) {
    writeLines("chart", file.path(clone, "atlas", paste0("figure", i, ".pdf")))
    suppressMessages(overleaf_sync(push = FALSE))
  }

  expect_true(overleaf_push())
  expect_true(all(paste0("figure", 1:3, ".pdf") %in%
                    remote_atlas_files(fake$remote)))
})


# Concurrent edits ------------------------------------------------------------

test_that("a chart still reaches Overleaf when the project has moved on", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  # Someone edits the project in Overleaf after we cloned it
  writeLines("edited in Overleaf", file.path(fake$seed, "main.tex"))
  writeLines("someone else's chart",
             file.path(fake$seed, "atlas", "their_chart.pdf"))
  gert::git_add(c("main.tex", "atlas"), repo = fake$seed)
  gert::git_commit("Edit in Overleaf", repo = fake$seed)
  gert::git_push(repo = fake$seed, verbose = FALSE)

  # Our push is rejected, so we reset onto their work and try again
  writeLines("chart", file.path(clone, "atlas", "figure1_normal.pdf"))
  expect_true(overleaf_sync())

  on_remote <- remote_atlas_files(fake$remote)

  # Both our chart and their work survive
  expect_true("figure1_normal.pdf" %in% on_remote)
  expect_true("their_chart.pdf" %in% on_remote)
})

test_that("charts win over a stale copy of the same chart in Overleaf", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  # The same chart is changed on both sides
  writeLines("their version", file.path(fake$seed, "atlas", "figure1.pdf"))
  gert::git_add("atlas", repo = fake$seed)
  gert::git_commit("Their chart", repo = fake$seed)
  gert::git_push(repo = fake$seed, verbose = FALSE)

  writeLines("our version", file.path(clone, "atlas", "figure1.pdf"))
  expect_true(overleaf_sync())

  check <- file.path(tempfile("overleaf-check-"))
  on.exit(unlink(check, recursive = TRUE))
  gert::git_clone(fake$remote, check, verbose = FALSE)

  expect_equal(readLines(file.path(check, "atlas", "figure1.pdf")),
               "our version")
})


# Saving charts end to end ----------------------------------------------------

test_that("grattan_save_overleaf pushes a chart over git", {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  fake <- make_fake_overleaf()
  use_fake_project(fake$remote)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  grattan_save_overleaf("test_chart.pdf", object = p)

  expect_true("test_chart_normal.pdf" %in% remote_atlas_files(fake$remote))
})


# The Dropbox backend is unaffected -------------------------------------------

test_that("overleaf_sync is a no-op for Dropbox projects", {

  withr::local_envvar(GRATTANTHEME_OVERLEAF_BACKEND = "dropbox")

  expect_false(overleaf_sync())
  expect_message(expect_false(overleaf_push()), "Dropbox")
})
