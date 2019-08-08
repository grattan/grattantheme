library(ggplot2)
library(gganimate)

test_that("animation works with blog type chart", {

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

  tempfile <- tempfile()
  on.exit(unlink(tempfile))

  grattan_anim_save(tempfile, anim_plot, type = "blog", nframes = 6, fps = 2)

  expect_true(file.exists("test.gif"))

  expect_gt(file.size("test.gif"), 300000)

  expect_lt(file.size("test.gif"), 450000)

  unlink("test.gif")

})

