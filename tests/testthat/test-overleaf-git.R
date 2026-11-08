context("Check that Overleaf git syncing works correctly")

# Helpers ---------------------------------------------------------------------

# A bare repo standing in for the Overleaf git bridge, seeded with an atlas
# folder so it looks like a real project. `seed` is a second clone that plays
# a colleague editing the project in Overleaf.
make_fake_overleaf <- function(env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("gert")
  skip_if_not_installed("withr")

  remote <- tempfile("overleaf-remote-")
  seed <- tempfile("overleaf-seed-")
  withr::defer(unlink(c(remote, seed), recursive = TRUE), envir = env)

  gert::git_init(remote, bare = TRUE)
  gert::git_clone(remote, seed, verbose = FALSE)
  gert::git_config_set("user.name", "Seed", repo = seed)
  gert::git_config_set("user.email", "seed@example.com", repo = seed)

  dir.create(file.path(seed, "atlas"))
  writeLines("\\documentclass{article}", file.path(seed, "main.tex"))
  writeLines("original", file.path(seed, "atlas", "existing.pdf"))
  edit_in_overleaf(seed, "Initial commit")

  list(remote = remote, seed = seed)
}

# Commit and push whatever has changed in the seed clone
edit_in_overleaf <- function(seed, message = "Edit in Overleaf") {
  gert::git_add(".", repo = seed)
  gert::git_commit(message, repo = seed)
  gert::git_push(repo = seed, verbose = FALSE)
}

# Point the package at a fake project, as set_overleaf_project() would
use_fake_project <- function(remote, env = parent.frame()) {

  cache <- tempfile("overleaf-cache-")
  clone <- file.path(cache, "fakeproject")
  withr::defer(unlink(cache, recursive = TRUE), envir = env)

  withr::local_envvar(
    GRATTANTHEME_OVERLEAF_BACKEND = "git",
    GRATTANTHEME_OVERLEAF_PROJECT = "fakeproject",
    GRATTANTHEME_OVERLEAF_PROJECT_PATH = clone,
    OVERLEAF_TOKEN = "not-a-real-token",
    .local_envir = env
  )

  overleaf_ensure_clone(path = clone, remote = remote)

  clone
}

# What does the remote's atlas folder hold? Returns each file's first line,
# named by file
remote_atlas <- function(remote) {
  check <- tempfile("overleaf-check-")
  on.exit(unlink(check, recursive = TRUE))
  gert::git_clone(remote, check, verbose = FALSE)
  files <- list.files(file.path(check, "atlas"), full.names = TRUE)
  first_line <- function(f) suppressWarnings(readLines(f, n = 1))
  stats::setNames(vapply(files, first_line, character(1)), basename(files))
}


# Project URLs and credentials ------------------------------------------------

test_that("overleaf_project_id finds IDs in project URLs, and only there", {

  id <- "0123456789abcdef01234567"

  expect_equal(overleaf_project_id(
    paste0("https://www.overleaf.com/project/", id, "/edit?x=1")), id)
  expect_equal(overleaf_project_id(id), id)

  # Dropbox project names and paths aren't Overleaf URLs
  expect_null(overleaf_project_id("Orange-Book-2025"))
  expect_null(overleaf_project_id("/Users/me/Dropbox/Apps/Overleaf/My Report"))
  expect_null(overleaf_project_id(NULL))
})

test_that("overleaf_auth_url adds and strips credentials", {

  url <- "https://git.overleaf.com/0123456789abcdef01234567"
  with_token <- "https://git:olp_abc@git.overleaf.com/0123456789abcdef01234567"

  expect_equal(overleaf_auth_url(url, "olp_abc"), with_token)
  # The bare 'git@' stored by earlier versions is replaced, not doubled up
  expect_equal(overleaf_auth_url(sub("//", "//git@", url), "olp_abc"),
               with_token)
  expect_equal(overleaf_auth_url(with_token), url)
  # Local paths, as used in these tests, are left alone
  expect_equal(overleaf_auth_url("/tmp/remote", "olp_abc"), "/tmp/remote")
})

test_that("with_overleaf_auth leaves no token in the clone's config", {
  skip_if_not_installed("gert")

  repo <- tempfile("overleaf-auth-")
  on.exit(unlink(repo, recursive = TRUE))
  gert::git_init(repo)
  url <- "https://git.overleaf.com/0123456789abcdef01234567"
  gert::git_remote_add(url, name = "origin", repo = repo)

  origin_url <- function() gert::git_remote_info("origin", repo = repo)$url

  # The token is there while git runs, and gone afterwards even if git fails
  expect_match(with_overleaf_auth(repo, "olp_abc", origin_url()), "olp_abc")
  expect_error(with_overleaf_auth(repo, "olp_abc", stop("push failed")),
               "push failed")
  expect_equal(origin_url(), url)
})

test_that("overleaf_commit_message names charts, then counts them", {
  expect_equal(overleaf_commit_message(c("atlas/a.pdf", "atlas/b.pdf")),
               "Update a.pdf, b.pdf (grattantheme)")
  expect_equal(overleaf_commit_message(paste0("atlas/chart", 1:5, ".pdf")),
               "Update 5 charts (grattantheme)")
})


# Syncing ---------------------------------------------------------------------

test_that("grattan_save_overleaf pushes a chart, and re-saving it is a no-op", {

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  grattan_save_overleaf("test_chart.pdf", object = p)
  expect_true("test_chart_normal.pdf" %in% names(remote_atlas(fake$remote)))

  commits <- nrow(gert::git_log(repo = clone))
  expect_false(overleaf_sync())
  expect_equal(nrow(gert::git_log(repo = clone)), commits)
})

test_that("push = FALSE holds charts back until overleaf_push()", {

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  for (chart in c("figure1.pdf", "figure2.pdf")) {
    writeLines("chart", file.path(clone, "atlas", chart))
    expect_message(overleaf_sync(push = FALSE), "overleaf_push")
  }
  expect_false("figure1.pdf" %in% names(remote_atlas(fake$remote)))

  expect_true(overleaf_push())
  expect_true(all(c("figure1.pdf", "figure2.pdf") %in%
                    names(remote_atlas(fake$remote))))
})

test_that("pushing keeps a colleague's newer work in Overleaf", {

  fake <- make_fake_overleaf()
  clone <- use_fake_project(fake$remote)

  # After we cloned, a colleague edits the text, updates a chart we haven't
  # touched, and uploads a version of the chart we're about to save
  writeLines("edited", file.path(fake$seed, "main.tex"))
  writeLines("their update", file.path(fake$seed, "atlas", "existing.pdf"))
  writeLines("their version", file.path(fake$seed, "atlas", "figure1.pdf"))
  edit_in_overleaf(fake$seed)

  # We save a chart. A stray edit outside atlas/ must never be sent.
  writeLines("our version", file.path(clone, "atlas", "figure1.pdf"))
  writeLines("stray local edit", file.path(clone, "main.tex"))
  expect_true(overleaf_sync())

  # Our chart wins; nothing else of theirs is overwritten
  atlas <- remote_atlas(fake$remote)
  expect_equal(atlas[["figure1.pdf"]], "our version")
  expect_equal(atlas[["existing.pdf"]], "their update")

  # The commit we pushed changes our chart and nothing else
  check <- tempfile("overleaf-check-")
  on.exit(unlink(check, recursive = TRUE))
  gert::git_clone(fake$remote, check, verbose = FALSE)
  expect_equal(gert::git_diff("HEAD", repo = check)$new, "atlas/figure1.pdf")
  expect_equal(readLines(file.path(check, "main.tex")), "edited")
})

test_that("overleaf_sync is a no-op for Dropbox projects", {
  skip_if_not_installed("withr")

  withr::local_envvar(GRATTANTHEME_OVERLEAF_BACKEND = "dropbox")

  expect_false(overleaf_sync())
  expect_message(expect_false(overleaf_push()), "Dropbox")
})
