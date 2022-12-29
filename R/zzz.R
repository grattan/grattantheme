.onLoad <- function(libname, pkgname) {

  cowsay::say(praise::praise(), sample(names(cowsay::animals), 1) )
  register_palette()
  set_aesthetics("grattan")

}
