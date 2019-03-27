#' @export
launch_multi <- function() {
  appDir <- system.file("multi", package = "Multi")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
