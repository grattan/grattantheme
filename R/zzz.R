.onLoad <- function(libname, pkgname) {
  register_palette()

  set_aesthetics("grattan")

  cowsay::say(praise::praise(), sample(names(cowsay::animals), 1) )

}
