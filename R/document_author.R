### Internal helpers for the author recorded in Powerpoint and Excel files.
### Office files store an author ("creator") and a "last modified by" name in
### their document properties. By default the author would be the template's
### original author, so grattantheme sets it to the name supplied through the
### `author` argument of the save functions. Excel files also get that name as
### "last modified by"; Powerpoint files keep the computer's login name there.

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
### with `author` as its author. officer (>= 0.7.8) always records the
### computer's login name as "last modified by" when it saves, so that field
### can't be set for Powerpoint files.
save_pptx_with_author <- function(x, target, author) {
  x <- officer::set_doc_properties(x, creator = author)
  print(x, target = target)
}
