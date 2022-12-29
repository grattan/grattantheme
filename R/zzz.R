.onLoad <- function(libname, pkgname) {

  cowsay::say(praise::praise(), sample(names(cowsay::animals)[!names(cowsay::animals) %in%  c("anxiouscat",
                                                                                             "fish",
                                                                                             "grumpycat",
                                                                                             "longcat",
                                                                                             "longtailcat",
                                                                                             "mushroom",
                                                                                             "shortcat",
                                                                                             "signbunny",
                                                                                             "stretchycat")], 1) )
  register_palette()
  set_aesthetics("grattan")

}
