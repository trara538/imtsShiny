#' Launch IMTS SDMX Shiny App
#'
#' Launches the IMTS Excel to SDMX Shiny application.
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "imtsShiny")
  if (app_dir == "") {
    stop("App directory not found")
  }

  shiny::runApp(app_dir, launch.browser = TRUE)
}
