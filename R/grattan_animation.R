#' Save gganimate animation with Grattan visual styling
#'
#' `grattan_anim_save()` takes a gganimate animation and formats it in line with
#' the Grattan Institute style guide, in a manner similar to `grattan_save()` for
#' static plots, then saves the animation to disk.
#'
#' @name grattan_anim_save
#' @param filename File to create on disk; required.
#' @param animation The animation objkect to save. Defaults to last rendered animation
#' using `gganimate::last_animation()`
#' @param type Either "blog" (to save using the Grattan Blog template) or
#' "normal" (to save as a Grattan chart as in a report).
#' @param path Path to save plot to (combined with filename)
#' @param ... arguments passed on to animate
#'
#' @import gganimate
#' @importFrom tools file_ext
#' @importFrom utils getFromNamespace
#'
#' @examples
#'
#' library(ggplot2)
#' library(gganimate)
#'
#' # First, create a static plot:
#'
#' static_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point() +
#'  labs(title = "If you use a long title, grattan_anim_save() will break it over two lines
#'  (but not three)",
#'       subtitle = "Your subtitle will also break over two lines if it needs to, and let's
#'       face it, it probably will need to. How nice! How cool!",
#'       caption = "And the title, subtitle, and caption are all aligned with the left of
#'       the image, not the left of the plotting area.") +
#'  theme_grattan()  +
#'  theme(axis.title = element_blank())
#'
#' # Then animate it (this can be in one step with the code above, if you'd prefer):
#'
#' anim_plot <- static_plot +
#'  transition_states(cyl)
#'
#' # Then save it. Note that you must specify the chart type. You can optionally specify
#' # nframes, fps, and other arguments to gganimate::animate()
#'
#' \dontrun{grattan_anim_save("test.gif", anim_plot, type = "blog", fps = 5, nframes = 20)}
#'
#'
#' @export

# Contains functions to create gganimate animations with Grattan stylings
# Note: the method is hacky and fragile

grattan_anim_save <- function(filename,
                              animation = gganimate::last_animation(),
                              type,
                              path = NULL,
                              ...) {

  # Because it's hacky, grattan_anim_save() creates several functions in the user's global environment
  # and then removed those objects when its done (yes, it's that hacky). If objects with those names already
  # exist  in the global environment when grattan_anim_save() is run,
  # it stops with an error, as I don't want to overwrite and remove those objects.

  object_exists_error <- " in your search path.\ngrattan_anim_save() cannot run if an object with this name exists in your R session."

  if(exists("ggplot_build_grattan")) {
    stop(paste0("You have an object called 'ggplot_build_grattan", object_exists_error))
  }

  if(exists("prerender_grattan")) {
    stop(paste0("You have an object called 'prerender_grattan'", object_exists_error))
  }

  if(exists("animate_grattan")) {
    stop(paste0("You have an object called 'animate_grattan'", object_exists_error))
  }

  # To add more types in the future, add more options here and create a new 'Scene'
  anim_chart_types <- c("blog", "normal")

  if(!type %in% anim_chart_types) {
    stop(paste0(type, " is not currently a supported chart type for a Grattan animated plot.\nFor supported types, see ?grattan_anim_save."))
  }

  anim_width <- chart_types$width[chart_types$type == type]
  anim_height <- chart_types$height[chart_types$type == type]

  # create internal functions

  ggplot_build_grattan <<- create_ggplot_build_grattan()

  prerender_grattan <<- create_prerender_grattan()

  animate_grattan <<- create_animate_grattan()

  # create animation
  anim_plot <- animate_grattan(animation,
                               width = anim_width,
                               height = anim_height,
                               res = 320,
                               units = "cm",
                               type = type,
                               ...)

  # save animation
  anim_save(filename = filename,
            animation = anim_plot,
            path = path)

  rm(ggplot_build_grattan, prerender_grattan, animate_grattan,
     envir = .GlobalEnv)

}

# The following are a series of hacky, non-exported functions that modify
# gganimate non-exported functions. This is a fragile method subject to breakage.
# This is an ill-advised solution to the problem, pending some new gganimate API.

# Create individual ggproto 'scene' objections - one for each chart type

# Scene Grattan -----

SceneBlog <- ggproto(
  "SceneBlog",
  getFromNamespace("Scene", "gganimate"),
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                        vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)

    plot <- create_fullslide(plot,
                             type = "blog",
                             height = NULL,
                             warn_labs = FALSE,
                             print_object = FALSE)

    plot$layout$l[plot$layout$name %in% c("title", "subtitle", "caption")] <- 1

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

SceneNormal <- ggproto(
  "SceneNormal",
  getFromNamespace("Scene", "gganimate"),
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                        vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)

    plot$plot <- wrap_labs(plot$plot, type = "normal")

    plot <- ggplot_gtable(plot)

    plot$layout$l[plot$layout$name %in% c("title", "subtitle", "caption")] <- 1

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


# create_scene_grattan -----
# Function that calls the correct 'scene' ggproto based on selected chart type

create_scene_grattan <- function(type,
                                 transition, view, shadow, ease, transmuters, nframes) {
  if (is.null(nframes)) nframes <- 100

  if(type == "blog") {
    scene <- ggproto(NULL, SceneBlog, transition = transition,
                     view = view, shadow = shadow, ease = ease,
                     transmuters = transmuters, nframes = nframes)
  }

  if(type == "normal") {
    scene <- ggproto(NULL, SceneNormal, transition = transition,
                     view = view, shadow = shadow, ease = ease,
                     transmuters = transmuters, nframes = nframes)

  }

  return(scene)
}

# ggplot_build_grattan ----
# this function *creates a function* that builds the animations
# it first needs to be used to create a function, and then that function can be called
# same for the functions that follow

create_ggplot_build_grattan <- function() {

  ggplot_build_grattan <- getFromNamespace("ggplot_build.gganim", "gganimate")

  formals(ggplot_build_grattan) <- alist(plot = , type = )

  ggplot_build_line_to_edit <- grep("create_scene\\(", body(ggplot_build_grattan))

  body(ggplot_build_grattan)[[ggplot_build_line_to_edit]] <-
    quote(scene <- create_scene_grattan(type = type,
                                        transition = plot$transition,
                                        view = plot$view,
                                        shadow = plot$shadow,
                                        ease = plot$ease,
                                        transmuters = plot$transmuters,
                                        nframes = plot$nframes))

  return(ggplot_build_grattan)
}



# prerender_grattan -----
create_prerender_grattan <- function() {

  prerender_grattan <- getFromNamespace("prerender", "gganimate")

  formals(prerender_grattan) <- alist(plot = , nframes = , type = )

  prerender_line_to_edit <- grep("ggplot_build\\(", body(prerender_grattan))

  body(prerender_grattan)[[prerender_line_to_edit]] <-
    quote(ggplot_build_grattan(plot, type))

  return(prerender_grattan)
}



# animate_grattan -----

create_animate_grattan <- function() {

  animate_grattan <- getFromNamespace("animate.gganim", "gganimate")

  formals(animate_grattan) <- alist(plot = ,
                                    nframes = ,
                                    fps = ,
                                    duration = ,
                                    detail = ,
                                    renderer = ,
                                    device = ,
                                    ref_frame = ,
                                    start_pause = ,
                                    end_pause = ,
                                    rewind = ,
                                    type = ,
                                    ... = )

  animate_line_to_edit <- grep("prerender\\(", body(animate_grattan))

  body(animate_grattan)[[animate_line_to_edit]] <-
    quote(plot <- prerender_grattan(plot, nframes_total, type))

  return(animate_grattan)

}





