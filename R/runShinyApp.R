#' @export
runExample <- function() {
  appDir <- system.file("shiny-apps", "taskscoring", package = "englelab")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `englelab`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
