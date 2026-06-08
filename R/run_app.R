#' Run the Shiny Application
#'
#' @param launch.browser Logical, passed to [shiny::runApp()].
#' @param data_env An environment to search for PM_model and PM_data objects.
#'   Defaults to [.GlobalEnv].
#' @param ... Additional arguments passed to golem options.
#'
#' @export
#' @importFrom shiny shinyApp runApp
#' @importFrom golem with_golem_options
run_app <- function(launch.browser = TRUE, data_env = .GlobalEnv, ...) {
  app <- golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(data_env = data_env, ...)
  )
  shiny::runApp(app, launch.browser = launch.browser)
}

#' Launch the Pmetrics Explorer app
#'
#' Convenience wrapper around [run_app()] to rapidly simulate from a
#' Pmetrics model and explore parameter ranges interactively.
#'
#' @param launch.browser Logical, passed to [shiny::runApp()].
#' @param data_env An environment to search for PM_model and PM_data objects.
#'   Defaults to [.GlobalEnv].
#' @param ... Additional arguments passed to golem options.
#'
#' @export
pm_explorer <- function(launch.browser = TRUE, data_env = .GlobalEnv, ...) {
  run_app(launch.browser = launch.browser, data_env = data_env, ...)
}
