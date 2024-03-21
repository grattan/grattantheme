.onLoad <- function(libname, pkgname) {

  if (interactive()) {
    unwanted_animals <- c(
      "anxiouscat", "fish", "grumpycat", "longcat", "longtailcat", "mushroom",
      "shortcat", "signbunny", "stretchycat"
    )
    wanted_animals <-
      names(cowsay::animals)[!names(cowsay::animals) %in% unwanted_animals]
    cowsay::say(praise::praise(), sample(wanted_animals, 1))
  }
  register_palette()
  set_aesthetics("grattan")

}
