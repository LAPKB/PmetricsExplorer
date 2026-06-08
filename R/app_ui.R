#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      theme = bslib::bs_theme(bootswatch = "zephyr"),
      title = "Pmetrics Explorer",
      sidebar = bslib::sidebar(
        width = 320,
        shiny::selectInput(
          "model_choice",
          "PM_model object:",
          choices = character(0)
        ),
        shiny::selectInput(
          "data_choice",
          "PM_data object:",
          choices = character(0)
        ),
        shiny::hr(),
        shiny::uiOutput("param_inputs"),
        shiny::hr(),
        bslib::input_switch("log_y", "Log Y axis", value = TRUE),
        shiny::hr(),
        shiny::actionButton(
          "exit_btn",
          "Exit",
          class = "btn-danger w-100"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        card_header = NULL,
        plotly::plotlyOutput("sim_plot", height = "100%")
      ),
      shiny::tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "PmetricsExplorer"
    )
  )
}
