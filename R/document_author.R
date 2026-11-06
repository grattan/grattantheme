### Internal helpers for the author recorded in Powerpoint and Excel files.
### Office files store an author ("creator") and a "last modified by" name in
### their document properties. By default these would be the template's
### original author and the user's login name, so grattantheme sets both to a
### single name supplied through the `author` argument of the save functions.

### check_author() stops with an error unless `author` is a single string.
### An empty string is allowed, and leaves both fields blank.
check_author <- function(author) {
  if (!is.character(author) || length(author) != 1 || is.na(author)) {
    stop("`author` must be a single string, such as \"Grattan Institute\".",
         call. = FALSE)
  }
  invisible(author)
}

### save_pptx_with_author() saves an officer Powerpoint object to `target`,
### with `author` as both its author and its "last modified by" name.
save_pptx_with_author <- function(x, target, author) {
  x <- officer::set_doc_properties(x,
                                   creator = author,
                                   last_modified_by = author)

  # officer overwrites "last modified by" with the USER environment variable
  # when it saves, so set USER to the author while saving and restore it after
  old_user <- Sys.getenv("USER", unset = NA)
  on.exit(if (is.na(old_user)) {
    Sys.unsetenv("USER")
  } else {
    Sys.setenv(USER = old_user)
  })
  Sys.setenv(USER = author)

  print(x, target = target)
}
