## code to prepare `vanilla_geom_aesthetics` dataset goes here

# Go to zzz.R and comment out set_aesthetics(type = "qnd") to make sure you
# don't overwrite the vanilla aesthetics

devtools::load_all()
check_subclass <- utils::getFromNamespace("check_subclass", "ggplot2")

# Library any packages that add geoms so you can take their defaults
# Check the geom names are in all_geoms in data_raw/all_geoms.R

invisible(lapply(geom_packs, library, character.only = TRUE))

vanilla_geom_aesthetics <- all_geoms %>%
  purrr::map(
    purrr::safely(check_subclass),
    "Geom",
    env = parent.frame()
  ) %>%
  purrr::map(
    purrr::pluck,
    "result"
  ) %>%
  purrr::map(
    purrr::pluck,
    "default_aes"
  ) %>%
  purrr::set_names(all_geoms)


usethis::use_data(vanilla_geom_aesthetics, internal = TRUE, overwrite = TRUE)
# Uncomment that line in zzz.R
# Restart your R session
