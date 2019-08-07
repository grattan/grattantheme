library(tidyverse)
library(gganimate)
library(magrittr)
devtools::load_all(".")
library(gridExtra)
library(grid)

# Static plot -----
static_plot <- iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width,
             color = Species)) +
  geom_point() +
  labs(title = "This is a proof-of-concept animated chart with the fullslide Grattan stylings",
       subtitle = "Because it's cool",
       caption = "Caption goes here") +
  theme_grattan()

static_plot

# anim_plot <- static_plot +
#   transition_states(Species,
#                     transition_length = 1,
#                     state_length = 1)


# Scene 2 -----
Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                        vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)

    plot <- create_fullslide(plot, "fullslide",
                             height = NULL,
                             warn_labs = FALSE,
                             print_object = FALSE)

    if (newpage) grid::grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid::grid.draw(plot)
    } else {
      if (is.character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid::grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  })

# create_scene2 -----
create_scene2 <- function(transition, view, shadow, ease, transmuters, nframes) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene2, transition = transition,
          view = view, shadow = shadow, ease = ease,
          transmuters = transmuters, nframes = nframes)
}

# ggplot_build2 ----
ggplot_build2 <- gganimate:::ggplot_build.gganim
body(ggplot_build2) <- body(ggplot_build2) %>%
  as.list() %>%
  inset2(4,
         quote(scene <- create_scene2(plot$transition, plot$view, plot$shadow,
                                      plot$ease, plot$transmuters, plot$nframes))) %>%
  as.call()

# prerender2 -----
prerender2 <- gganimate:::prerender
body(prerender2) <- body(prerender2) %>%
  as.list() %>%
  inset2(3,
         quote(ggplot_build2(plot))) %>%
  as.call()

# animate2 -----
animate2 <- gganimate:::animate.gganim
body(animate2) <- body(animate2) %>%
  as.list() %>%
  inset2(7,
         quote(plot <- prerender2(plot, nframes_total))) %>%
  as.call()

# usage ----

anim_plot <- static_plot +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)
#
# animate2(anim_plot,
#          nframes = 10,
#          fps = 3,
#          renderer = gifski_renderer())
#
#
# animate(anim_plot,
#         nframes = 10,
#         fps = 3,
#         renderer = gifski_renderer(width = 5000,
#                                    height = 3000))
#
# anim_save("anim_plot.gif", anim_plot,
#           nframes = 10, fps = 3,
#           width = 3200, height = 2394)

anim_plot2 <- animate2(anim_plot,
                       nframes = 10, fps = 2,
                       width = 25.4, height = 19.0, res = 320, units = "cm")

gganimate::save_animation(anim_plot2, "test.gif")


