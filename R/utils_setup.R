#' Setup Reactor Dashboard Directory
#'
#' @description This utility function ensures that the necessary directory structure
#' and files for the Reactor Dashboard are created. If the directory `~/Reactor Dashboard`
#' does not exist, it creates the directory along with a subdirectory `Reactions`.
#' Additionally, it copies a default settings file (`Reaction settings.xlsx`)
#' to the newly created `Reactions` folder as `Standard.xlsx`.
#'
#' @details This function is useful for initializing the environment required
#' for the Reactor Dashboard application. It checks for the existence of the
#' main directory and creates it if necessary. It also ensures that a default
#' configuration file is available in the appropriate location.
#'
#' @return This function does not return a value. It performs its operations
#' as a side effect by creating directories and copying files.
#'
#' @noRd
setup <- function() {
  if (!dir.exists(paths = "~/Reactor Dashboard")) {
    dir.create(path = "~/Reactor Dashboard/Reactions", recursive = TRUE)
    file.copy(
      from = "Reaction settings.xlsx",
      to = "~/Reactor Dashboard/Reactions/Standard.xlsx"
    )
  }
}
