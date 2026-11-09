.onLoad <- function(libname, pkgname) {

  set_aesthetics("grattan")
  setup_grattan_fonts()

}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(get_font_status_message())
}
