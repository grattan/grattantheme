#' Save gganimate animation with Grattan visual styling
#'
#' `grattan_anim_save()` takes a gganimate animation and formats it in line with
#' the Grattan Institute style guide, in a manner similar to `grattan_save()`
#' for static plots, then saves the animation to disk.
#'
#' @name grattan_anim_save
#' @param filename File to create on disk; required.
#' @param animation The animation objkect to save. Defaults to last rendered
#'   animation using `gganimate::last_animation()`
#' @param type Either "blog" (to save using the Grattan Blog template) or
#' "normal" (to save as a Grattan chart as in a report).
#' @param path Path to save plot to (combined with filename)
#' @param ... arguments passed on to animate
#'
#' @import gganimate
#' @importFrom tools file_ext
#' @importFrom utils getFromNamespace packageVersion
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
#'  labs(title = "Title goes here",
#'       subtitle = "Subtitle goes here",
#'       caption = "Notes: some notes. Source: data source") +
#'  theme_grattan()  +
#'  theme(axis.title = element_blank())
#'
#' # Then animate it (this can be in one step with the code above,
#' # if you'd prefer):
#'
#' anim_plot <- static_plot +
#'  transition_states(cyl)
#'
#' # Then save it. Note that you must specify the chart type. You can optionally
#' # specify # nframes, fps, and other arguments to gganimate::animate()
#'
#' \dontrun{grattan_anim_save("test.gif", anim_plot, type = "blog",
#' fps = 5, nframes = 20)}
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

  if (type == "fullslide") {
    message("'fullslide' charts are not yet supported by grattan_anim_save;",
            "creating a 'blog' type instead")
  }

  # To add more types in the future, add more options here and create a new
  # 'Scene'
  anim_chart_types <- c("blog", "normal", "fullslide_169")

  if (!type %in% anim_chart_types) {
    stop(paste0(type,
                " is not currently a supported chart type for a Grattan",
                "animated plot.\nFor supported types, see ?grattan_anim_save."))
  }

  anim_width <- chart_types$width[chart_types$type == type]
  anim_height <- chart_types$height[chart_types$type == type]

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

}

# The following are a series of hacky, non-exported functions that modify
# gganimate non-exported functions.
# This is a fragile method subject to breakage.
# This is an ill-advised solution to the problem, which will in time be
# broken by changes to gganimate.

# Create individual ggproto 'scene' objections - one for each chart type

# Scene Grattan -----

SceneBlog <- ggproto(
  "SceneBlog",
  getFromNamespace("Scene", "gganimate"),
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                        vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)

    plot <- gridextra_fullslide(plot,
                             type = "blog")

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

SceneFullslide169 <- ggproto(
  "SceneFullslide169",
  getFromNamespace("Scene", "gganimate"),
  plot_frame = function(self, plot, i, newpage = is.null(vp),
                        vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)

    plot <- gridextra_fullslide(plot,
                             type = "fullslide_169")

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
                                 transition,
                                 view,
                                 shadow,
                                 ease,
                                 transmuters,
                                 nframes) {
  if (is.null(nframes)) nframes <- 100

  if (type == "blog") {
    scene <- ggproto(NULL, SceneBlog, transition = transition,
                     view = view, shadow = shadow, ease = ease,
                     transmuters = transmuters, nframes = nframes)
  }

  if (type == "normal") {
    scene <- ggproto(NULL, SceneNormal, transition = transition,
                     view = view, shadow = shadow, ease = ease,
                     transmuters = transmuters, nframes = nframes)

  }

  if (type == "fullslide_169") {
    scene <- ggproto(NULL, SceneFullslide169, transition = transition,
                     view = view, shadow = shadow, ease = ease,
                     transmuters = transmuters, nframes = nframes)

  }

  return(scene)
}

# ggplot_build_grattan ----

# This function is based on gganimate:::ggplot_build.gganim by
# Thomas Lin Pedersen
# The only change is that create_scene() is now create_scene_grattan() and the
# function takes a `type` argument, which is passed to create_scene_grattan()

ggplot_build_grattan <- function(plot, type) {

  plot_clone <- getFromNamespace("plot_clone", "gganimate")
  create_layout <- getFromNamespace("create_layout", "gganimate")
  scales_transform_df <- getFromNamespace("scales_transform_df", "gganimate")
  scales_add_missing <- getFromNamespace("scales_add_missing", "gganimate")
  is.waive <- getFromNamespace("is.waive", "gganimate")
  scales_train_df <- getFromNamespace("scales_train_df", "gganimate")
  scales_map_df <- getFromNamespace("scales_map_df", "gganimate")

  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  scene <- create_scene_grattan(type = type,
                                plot$transition,
                                plot$view,
                                plot$shadow,
                                plot$ease,
                                plot$transmuters,
                                plot$nframes)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

  scales <- plot$scales

  # Extract scale names and merge it with label list
  scale_labels <- lapply(scales$scales, `[[`, "name")
  names(scale_labels) <- vapply(scales$scales, function(sc) sc$aesthetics[1], character(1))
  lapply(scales$scales, function(sc) sc$name <- waiver())
  scale_labels <- scale_labels[!vapply(scale_labels, is.waive, logical(1))]
  plot$labels[names(scale_labels)] <- scale_labels

  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }

  # Allow all layers to make any final adjustments based
  # on raw input data and plot info
  data <- layer_data
  if (packageVersion("ggplot2") > "3.1.1") {
    # ggplot2 versions 3.1.0 or earlier do not support `setup_layer()`
    data <- by_layer(function(l, d) l$setup_layer(d, plot))
  }

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data

  layout <- create_layout(plot$facet, plot$coordinates)
  data <- layout$setup(data, plot$data, plot$plot_env)
  scene$setup(data)
  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

  scene$identify_layers(data, layers)
  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  data <- scene$before_stat(data)

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))

  data <- scene$after_stat(data)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  data <- scene$before_position(data)

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout))

  data <- scene$after_position(data)

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what is
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d))

  data <- scene$after_defaults(data)

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d))

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  data <- scene$finish_data(data)

  structure(
    list(data = data, layout = layout, plot = plot, scene = scene),
    class = "gganim_built"
  )
}



# prerender_grattan -----

# Note that this is copied from gganimate::prerender by Thomas Lin Pedersen
# The original calls ggplot_build() while this calls ggplot_build_grattan()

prerender_grattan <- function(plot, nframes, type) {
  set_nframes <- getFromNamespace("set_nframes", "gganimate")

  plot <- set_nframes(plot, nframes)
  ggplot_build_grattan(plot, type)

}



# animate_grattan -----

# Note that this function is copied from gganimate::animate by
# Thomas Lin Pedersen
# The original calls gganimate::prerender()
# while this calls grattantheme::prerender_grattan()

animate_grattan <- function(plot,
                            nframes,
                            fps,
                            duration,
                            detail,
                            renderer,
                            device,
                            ref_frame,
                            start_pause,
                            end_pause,
                            rewind,
                            type,
                            ...) {

  prepare_args <- getFromNamespace("prepare_args", "gganimate")
  get_nframes <- getFromNamespace("get_nframes", "gganimate")
  draw_frames <- getFromNamespace("draw_frames", "gganimate")
  set_last_animation <- getFromNamespace("set_last_animation", "gganimate")

  args <- prepare_args(
    nframes = nframes,
    fps = fps,
    duration = duration,
    detail = detail,
    renderer = renderer,
    device = device,
    ref_frame = ref_frame,
    start_pause = start_pause,
    end_pause = end_pause,
    rewind = rewind,
    ...
  )
  orig_nframes <- args$nframes
  args$nframes <- args$nframes - args$start_pause - args$end_pause
  if (args$rewind) {
    args$nframes <- ceiling((args$nframes - args$start_pause) / 2)
    args$end_pause <- ceiling(args$end_pause / 2)
  }
  nframes_total <- (args$nframes - 1) * args$detail + 1
  plot <- prerender_grattan(plot, nframes_total, type)
  nframes_final <- get_nframes(plot)

  frame_ind <- unique(round(seq(1, nframes_final, length.out = args$nframes)))

  if (args$device == "current") {
    frame_ind <- c(rep(frame_ind[1],
                       args$start_pause),
                   frame_ind,
                   rep(frame_ind[length(frame_ind)],
                                  args$end_pause))
    if (args$rewind) frame_ind <- c(frame_ind,
                                    rev(frame_ind))
    if (args$ref_frame < 0) {
      args$ref_frame <- args$ref_frame - args$end_pause
    } else {
      args$ref_frame <- args$ref_frame + args$start_pause
    }
  }

  if (args$nframes != length(frame_ind)) {
    message("nframes and fps adjusted to match transition")
    args$fps <- args$fps * length(frame_ind) / args$nframes
  }

  if (args$ref_frame < 0) args$ref_frame <- nframes_final + 1 + args$ref_frame

  frames_vars <- do.call(
    draw_frames,
    c(list(plot = plot,
           frames = frame_ind,
           device = args$device,
           ref_frame = args$ref_frame),
      args$dev_args)
  )
  if (args$device == "current") return(invisible(frames_vars))

  if (args$start_pause != 0) frames_vars <- rbind(frames_vars[rep(1, args$start_pause), , drop = FALSE], frames_vars)
  if (args$end_pause != 0) frames_vars <- rbind(frames_vars, frames_vars[rep(nrow(frames_vars), args$end_pause), , drop = FALSE])
  if (args$rewind) frames_vars <- rbind(frames_vars, frames_vars[rev(seq_len(orig_nframes - nrow(frames_vars))), , drop = FALSE])

  animation <- args$renderer(frames_vars$frame_source, args$fps)
  attr(animation, "frame_vars") <- frames_vars
  set_last_animation(animation)
  animation
}
