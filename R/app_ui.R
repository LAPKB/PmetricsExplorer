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
        shiny::actionButton(
          "help_me_btn",
          "Help Me!",
          icon = shiny::icon("wand-magic-sparkles"),
          class = "btn-primary w-100"
        ),
        shiny::div(
          class = "d-flex gap-2 mt-2",
          shiny::actionButton(
            "copy_close_btn",
            "Copy and Close",
            icon = shiny::icon("clipboard-check"),
            class = "btn-success flex-fill"
          ),
          shiny::actionButton(
            "close_btn",
            "Close",
            icon = shiny::icon("xmark"),
            class = "btn-danger flex-fill"
          )
        ),
        shiny::uiOutput("help_me_status"),
        shiny::hr(),
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
        shiny::selectInput(
          "outeq",
          "Output equation:",
          choices = 1
        ),
        shiny::selectizeInput(
          "subjects",
          "Subjects to include:",
          choices = character(0),
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        shiny::hr(),
        shiny::uiOutput("param_inputs"),
        shiny::hr(),
        bslib::input_switch("log_y", "Log Y axis", value = TRUE)
      ),
      bslib::card(
        full_screen = TRUE,
        card_header = NULL,
        plotly::plotlyOutput("sim_plot", height = "100%")
      ),
      shiny::tags$style(
        type = "text/css",
        "html { font-size: 80%; }",
        ".js-plotly-plot .plotly text { font-size: 80% !important; }",
        "#help_me_status { min-height: 1.4rem; margin-top: 0.4rem; }",
        ".help-status { color: var(--bs-secondary-color); font-size: 0.9rem; }",
        ".help-status-success { color: var(--bs-success); }",
        ".help-status-error { color: var(--bs-danger); }",
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
    ),
    shiny::tags$script(shiny::HTML("
      $(function() {
        function fallbackCopy(text) {
          var textarea = document.createElement('textarea');
          textarea.value = text;
          textarea.style.position = 'fixed';
          textarea.style.opacity = '0';
          document.body.appendChild(textarea);
          textarea.focus();
          textarea.select();
          var copied = document.execCommand('copy');
          document.body.removeChild(textarea);
          return copied;
        }

        Shiny.addCustomMessageHandler('copy_pri_and_close', function(message) {
          function finish(success) {
            Shiny.setInputValue(
              'copy_close_result',
              { nonce: message.nonce, success: success },
              { priority: 'event' }
            );
          }

          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(message.text)
              .then(function() { finish(true); })
              .catch(function() { finish(fallbackCopy(message.text)); });
          } else {
            finish(fallbackCopy(message.text));
          }
        });
      });
    "))
  )
}
