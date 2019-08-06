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


# This code borrows heavily from code by Z. Lin at StackOverflow:
# https://stackoverflow.com/questions/55362961/using-a-gtable-object-with-gganimate


# SceneGrattan -----
SceneGrattan <- ggproto(
  "SceneGrattan",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                          vp = NULL, widths = NULL, heights = NULL, type = "fullslide", ...) {
    plot <- self$get_frame(plot, i)

    plot <- create_fullslide(
      object = plot,
      type = type,
      height = NULL,
      warn_labs = FALSE,
      print_object = FALSE
    )

    if (newpage) grid::grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid::grid.draw(plot)
    } else {
      if (is.character(vp)) {
        seekViewport(vp)
      } else {
        pushViewport(vp)
      }
      grid::grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  }
)

# create_scene2 -----
create_scene_grattan <- function(transition,
                                 view, shadow, ease, transmuters, nframes, type) {
  if (is.null(nframes)) nframes <- 100

  ggproto(NULL, SceneGrattan,
    transition = transition,
    view = view,
    shadow = shadow,
    ease = ease,
    transmuters = transmuters,
    nframes = nframes,
    type = type
  )
}

# ggplot_build_grattan ----

ggplot_build_grattan <- function(plot, type) {}

body(ggplot_build_grattan) <- body(gganimate:::ggplot_build.gganim)
environment(ggplot_build_grattan) <- environment(gganimate:::ggplot_build.gganim)

body(ggplot_build_grattan) <- body(ggplot_build_grattan) %>%
  as.list() %>%
  inset2(
    4,
    quote(scene <- create_scene_grattan(
      transition = plot$transition,
      view = plot$view,
      shadow = plot$shadow,
      ease = plot$ease,
      transmuters = plot$transmuters,
      nframes = plot$nframes,
      type = type
    ))
  ) %>%
  as.call()


# prerender_grattan -----
prerender_grattan <- function(plot, nframes, type){}

body(prerender_grattan) <- body(gganimate:::prerender)
environment(prerender_grattan) <- environment(gganimate:::prerender)

body(prerender_grattan) <- body(prerender_grattan) %>%
  as.list() %>%
  inset2(
    3,
    quote(ggplot_build_grattan(plot, type))
  ) %>%
  as.call()

# animate_grattan -----
animate_grattan <- function(plot, nframes, fps, duration, detail, renderer,
                            device, ref_frame, start_pause, end_pause,
                            rewind, type, ...) {}

body(animate_grattan) <- body(gganimate:::animate.gganim)
environment(animate_grattan) <- environment(gganimate:::animate.gganim)

body(animate_grattan) <- body(animate_grattan) %>%
  as.list() %>%
  inset2(
    7,
    quote(plot <- prerender_grattan(plot, nframes_total, type))
  ) %>%
  as.call()

# usage ----

anim_plot <- static_plot +
  transition_states(Species,
    transition_length = 2,
    state_length = 1
  )


type <- "blog"
anim_plot2 <- animate_grattan(plot = anim_plot,
                              type = "blog",
                              nframes = 10, fps = 2,
                              width = grattantheme:::chart_types$width[grattantheme:::chart_types$type == type],
                              height = grattantheme:::chart_types$height[grattantheme:::chart_types$type == type],
                              res = 320, units = "cm")

gganimate::save_animation(anim_plot2, "test.gif")
