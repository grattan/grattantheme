#' Get the Overleaf base path
#'
#' @description Finds the Apps/Overleaf folder in the user's Grattan Institute
#'   Dropbox by searching one level down from the Dropbox root, excluding
#'   'data' and 'Grattan Team' folders.
#'
#' @return Character string containing the path to the Apps/Overleaf folder
#'
#' @importFrom jsonlite fromJSON
#' @keywords internal
get_overleaf_base_path <- function() {

  # Locate the .json file containing information about the user's local Dropbox
  dropbox_info_location <-
    if (Sys.getenv("OS") == "Windows_NT") {
      file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
    } else {
      "~/.dropbox/info.json"
    }

  if (!file.exists(dropbox_info_location)) {
    stop("Could not find your Dropbox location. ",
         "Please ensure Dropbox is installed and syncing.")
  }

  # Get the path to business Dropbox
  dropbox_info <- jsonlite::fromJSON(dropbox_info_location)

  if (!"business" %in% names(dropbox_info)) {
    stop("Could not find business Dropbox. ",
         "Please ensure you're syncing the Grattan Institute Dropbox.")
  }

  dropbox_path <- dropbox_info$business$path

  # Get 'Apps folder' if it exists
  if(dir.exists(file.path(dropbox_path, "Apps", "Overleaf"))) {
    return(file.path(dropbox_path, "Apps", "Overleaf"))
  }

  # If we get here, nothing was found
  stop("Could not find an Apps/Overleaf folder in your Dropbox. ",
       "Please ensure Overleaf is synced via Dropbox.")
}


#' Find Overleaf projects
#'
#' @param overleaf_base_path Path to the Apps/Overleaf folder
#'
#' @return Character vector of project folder names
#'
#' @keywords internal
find_overleaf_projects <- function(overleaf_base_path) {
  projects <- list.dirs(overleaf_base_path,
                        full.names = FALSE,
                        recursive = FALSE)
  projects
}


#' Set or reset the Overleaf project location
#'
#' @param project Either a project name fragment (like "Orange Book"), a full
#'   project name (like "Orange-Book-2025"), or a full file path
#'   to an Overleaf project folder. If NULL (the default), you will be prompted
#'   to select from available projects.
#'
#' @details This function lets you choose which Overleaf project to save charts
#'   to when using \code{grattan_save_overleaf()}. The setting is stored as an
#'   environment variable for the current R session only.
#'
#'   If you don't provide a project, you'll see a menu of available Overleaf
#'   projects to choose from. If you provide a project name or fragment, it will
#'   search for matching projects (case-insensitive). If you provide a full file
#'   path, it will use that path directly.
#'
#'   Since this setting only persists for the current R session, we recommend
#'   calling \code{set_overleaf_project()} at the beginning of your script, or
#'   in a project setup script or .Renv file, to improve reproducibility.
#'
#' @return The full path to the selected Overleaf project folder, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prompt to select from available projects
#' set_overleaf_project()
#'
#' # Search for a project by name fragment
#' set_overleaf_project("Orange")  # Finds "Orange-Book-2025"
#'
#' # Use exact project name
#' set_overleaf_project("Orange-Book-2025")
#'
#' # Manually set a specific project path
#' set_overleaf_project("/Users/you/Dropbox/yourname/Apps/Overleaf/My Report")
#'
#' # Call at the start of your script for reproducibility
#' set_overleaf_project("transport-report")
#' # ... rest of your script
#' }
set_overleaf_project <- function(project = NULL) {

  if (is.null(project)) {
    # Prompt user to select from available projects
    overleaf_base <- get_overleaf_base_path()
    projects <- find_overleaf_projects(overleaf_base)

    if (length(projects) == 0) {
      stop("No Overleaf projects found in ", overleaf_base)
    }

    choice <- menu(choices = projects,
                   title = "Which Overleaf project should charts be saved to?")

    if (choice == 0) {
      stop("No Overleaf project selected.")
    }

    current_project <- projects[choice]
    project_path <- file.path(overleaf_base, current_project)

  } else if (dir.exists(project)) {
    # User provided a full path that exists - use it directly
    if (!grepl("Overleaf", project)) {
      warning("The path '", project, "' does not contain 'Overleaf'. ",
              "Are you sure this is an Overleaf project folder?")
    }

    current_project <- basename(project)
    project_path <- project

  } else {
    # User provided a project name or fragment - search for it
    overleaf_base <- get_overleaf_base_path()
    all_projects <- find_overleaf_projects(overleaf_base)

    if (length(all_projects) == 0) {
      stop("No Overleaf projects found in ", overleaf_base)
    }

    # Case-insensitive search for matching projects
    matched_projects <- all_projects[grepl(project, all_projects, ignore.case = TRUE)]

    if (length(matched_projects) == 0) {
      stop("No Overleaf projects found matching '", project, "'. ",
           "Available projects are:\n", paste0(all_projects, collapse = "\n"))
    }

    if (length(matched_projects) > 1) {
      stop("Multiple Overleaf projects found matching '", project, "':\n",
           paste0(matched_projects, collapse = "\n"),
           "\nPlease be more specific.")
    }

    current_project <- matched_projects[1]
    project_path <- file.path(overleaf_base, current_project)
  }

  # Set environment variables for current session
  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT = current_project)
  Sys.setenv(GRATTANTHEME_OVERLEAF_PROJECT_PATH = project_path)
  Sys.setenv(GRATTANTHEME_OVERLEAF_LAST_MESSAGE = as.character(Sys.time()))

  message("Overleaf project set to: ", current_project)

  invisible(project_path)
}


#' Get the Overleaf project atlas directory
#'
#' @description Returns the path to the atlas folder of your chosen Overleaf
#'   project. On first use (or if no project has been set), you'll be prompted
#'   to select from available Overleaf projects in your Dropbox. Your choice
#'   is stored as an environment variable for the current R session only.
#'
#' @details This function is called internally by \code{grattan_save_overleaf()},
#'   but can also be used directly if you need the Overleaf atlas path for other
#'   purposes.
#'
#'   If the session persists longer than 24 hours, a reminder message will
#'   display which project folder is being used.
#'   To change projects or set a custom path, use \code{set_overleaf_project()}.
#'
#'   The function creates the atlas subfolder if it doesn't already exist.
#'
#'   Since the setting only persists for the current R session, we recommend
#'   calling \code{set_overleaf_project()} at the beginning of your script for
#'   clarity and reproducibility.
#'
#' @return Character string containing the full path to the atlas folder of
#'   the selected Overleaf project.
#'
#' @seealso \code{\link{set_overleaf_project}} to change which project is used,
#'   \code{\link{grattan_save_overleaf}} to save charts directly to Overleaf
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the current Overleaf project path
#' atlas_path <- get_overleaf_project()
#'
#' # Use it to save files manually
#' write.csv(my_data, file.path(atlas_path, "my_data.csv"))
#' }
get_overleaf_project <- function() {

  current_project <- Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT", unset = "")

  if (current_project == "") {
    # First use - call set_overleaf_project() to prompt
    set_overleaf_project()
    # After setting, get the values
    current_project <- Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT")
  }

  # Check if we should show reminder (every 72 hours)
  last_message_str <- Sys.getenv("GRATTANTHEME_OVERLEAF_LAST_MESSAGE", unset = "")

  if (last_message_str != "") {
    last_message <- as.POSIXct(last_message_str)

    if (difftime(Sys.time(), last_message, units = "hours") >= 24) {
      message("Saving to Overleaf project: ", current_project)

      # Update timestamp
      Sys.setenv(GRATTANTHEME_OVERLEAF_LAST_MESSAGE = as.character(Sys.time()))
    }
  }

  # Get the full path
  project_full_path <- Sys.getenv("GRATTANTHEME_OVERLEAF_PROJECT_PATH", unset = "")

  if (project_full_path == "") {
    # Reconstruct from base path and project name
    overleaf_base <- get_overleaf_base_path()
    project_full_path <- file.path(overleaf_base, current_project)
  }

  # Return path to atlas folder
  atlas_path <- file.path(project_full_path, "atlas")
  if (!dir.exists(atlas_path)) {
    dir.create(atlas_path, recursive = TRUE, showWarnings = FALSE)
  }

  atlas_path
}
