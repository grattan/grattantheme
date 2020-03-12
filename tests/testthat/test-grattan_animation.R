library(ggplot2)
library(gganimate)

test_that("animation works", {

  #skip_on_travis()

  static_plot <- ggplot(mtcars,
                        aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "If you use a long title, grattan_anim_save() will break it over two lines (but not three)",
         subtitle = "Your subtitle will also break over two lines if it needs to, and let's face it, it probably will need to. How nice! How cool!",
         caption = "And the title, subtitle, and caption are all aligned with the left of the image, not the left of the plotting area.") +
    theme_grattan()  +
    theme(axis.title = element_blank())

  anim_plot <- static_plot +
    transition_states(cyl)

  types <- c("blog", "normal", "fullslide_169")

  blog_file <- paste(tempfile(), "blog", sep = "_")
  normal_file <- paste(tempfile(), "normal", sep = "_")
  fullslide_169_file <- paste(tempfile(), "fullslide_169", sep = "_")

  on.exit(unlink(c(blog_file, normal_file, fullslide_169_file)))

  grattan_anim_save(blog_file, anim_plot, type = "blog", nframes = 6, fps = 2)
  grattan_anim_save(normal_file, anim_plot, type = "normal", nframes = 6, fps = 2)
  grattan_anim_save(fullslide_169_file, anim_plot, type = "fullslide_169", nframes = 6, fps = 2)

  expect_true(file.exists(blog_file))
  expect_true(file.exists(normal_file))
  expect_true(file.exists(fullslide_169_file))

  frames_in_file_blog <- length(magick::image_read(blog_file))
  frames_in_file_normal <- length(magick::image_read(normal_file))
  frames_in_file_fullslide_169 <- length(magick::image_read(fullslide_169_file))

  expect_equal(frames_in_file_blog, 6)
  expect_equal(frames_in_file_normal, 6)
  expect_equal(frames_in_file_fullslide_169, 6)

})

