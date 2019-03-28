#' Function is to launch the GUI of the package, based on Shiny.
#'
#' `launch_multi()` If used on Rstudio the function will launch an interface to run
#' the analysis graphically.
#' @author Pierre-Gilles Lemasle <pg.lemasle@gmail.com>
#' @export
#' @export
launch_multi <- function() {
  appDir <- system.file("multi", package = "Multi")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
