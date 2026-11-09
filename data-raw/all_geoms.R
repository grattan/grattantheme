## Helper for data-raw/vanilla_geom_aesthetics.R; not shipped with the package

# This script loads a bunch of supporting packages and extracts their geoms.
# These geom names are then used to set the default aesthetics.

geom_packs <- c(
  "ggplot2",
  "ggbeeswarm",
  "ggridges",
  "geomtextpath",
  "ggtext",
  "ggspatial",
  "ggrepel",
  "ggExtra",
  "ggforce",
  "ggh4x"
)

# Not sure yet if adding packages to "suggests" is useful, don't do it for now
# purrr::walk(geom_packs[-1L], use_package, type = "Suggests") # nolint


get_geoms <- function(pack) {
  ls(getNamespace(pack), pattern = "^geom_")
}

all_geoms <- purrr::map(geom_packs, get_geoms) |>
  purrr::flatten_chr() |>
  stringr::str_remove("^geom_") |>
  sort()

