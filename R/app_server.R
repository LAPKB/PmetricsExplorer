#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # --- helpers ---------------------------------------------------------------

  # Return the environment to search for Pmetrics objects
  data_env <- function() {
    golem::get_golem_options("data_env") %||% .GlobalEnv
  }

  # List all objects of a given class in the data environment
  list_objects_of_class <- function(cls) {
    env <- data_env()
    nms <- ls(envir = env)
    nms[vapply(nms, \(n) inherits(get(n, envir = env), cls), logical(1))]
  }

  # Safely retrieve an object from the data environment by name
  get_obj <- function(name) {
    if (is.null(name) || name == "") return(NULL)
    env <- data_env()
    if (exists(name, envir = env, inherits = FALSE)) {
      get(name, envir = env)
    } else {
      NULL
    }
  }

  # --- populate object choosers on load / refresh ----------------------------

  shiny::observe({
    model_choices <- list_objects_of_class("PM_model")
    data_choices  <- list_objects_of_class("PM_data")
    shiny::updateSelectInput(session, "model_choice",
      choices  = c("" = "", model_choices),
      selected = if (length(model_choices) > 0) model_choices[[1]] else ""
    )
    shiny::updateSelectInput(session, "data_choice",
      choices  = c("" = "", data_choices),
      selected = if (length(data_choices) > 0) data_choices[[1]] else ""
    )
  })

  # --- reactive: selected model ----------------------------------------------

  selected_model <- shiny::reactive({
    shiny::req(input$model_choice)
    get_obj(input$model_choice)
  })

  # --- reactive: parameter names & midpoints from model ----------------------

  param_info <- shiny::reactive({
    model <- selected_model()
    shiny::req(model)
    params <- model$model_list$parameters
    pri    <- model$model_list$pri
    shiny::req(length(params) > 0, length(pri) > 0)

    midpoints <- vapply(seq_along(params), \(i) {
      mn <- pri[[i]]$min
      mx <- pri[[i]]$max
      (mn + mx) / 2
    }, numeric(1))

    list(params = params, midpoints = midpoints, pri = pri)
  })

  # --- render numeric inputs for each parameter ------------------------------

  output$param_inputs <- shiny::renderUI({
    info <- param_info()
    shiny::req(info)
    params    <- info$params
    midpoints <- info$midpoints
    pri       <- info$pri

    inputs <- lapply(seq_along(params), \(i) {
      p   <- params[[i]]
      mid <- midpoints[[i]]
      mn  <- pri[[i]]$min
      mx  <- pri[[i]]$max
      step <- max((mx - mn) / 100, .Machine$double.eps * 100)
      shiny::numericInput(
        inputId = paste0("param_", p),
        label   = p,
        value   = round(mid, 4),
        min     = mn,
        max     = mx,
        step    = round(step, 6)
      )
    })
    shiny::tagList(
      shiny::h6("Parameter values:"),
      inputs
    )
  })

  # --- reactive: build poppar data frame from current input values -----------

  poppar_reactive <- shiny::reactive({
    info <- param_info()
    shiny::req(info)
    params <- info$params

    vals <- vapply(params, \(p) {
      v <- input[[paste0("param_", p)]]
      if (is.null(v) || is.na(v)) info$midpoints[[which(info$params == p)]] else v
    }, numeric(1))

    df <- as.data.frame(t(vals))
    names(df) <- params
    df
  })

  # --- reactive: run simulation ----------------------------------------------

  sim_result <- shiny::reactive({
    model  <- selected_model()
    data   <- get_obj(input$data_choice)
    poppar <- poppar_reactive()

    shiny::req(model, data, poppar)
    shiny::req(nrow(poppar) > 0)

    tryCatch(
      PM_sim$new(
        poppar  = poppar,
        data    = data,
        model   = model,
        predInt = 1
      ),
      error = function(e) {
        structure(list(message = conditionMessage(e)), class = "sim_error")
      }
    )
  })

  # --- render plot -----------------------------------------------------------

  output$sim_plot <- plotly::renderPlotly({
    result <- sim_result()
    shiny::req(result)

    if (inherits(result, "sim_error")) {
      # Return an empty plotly with an annotation
      plotly::plot_ly() |>
        plotly::layout(
          annotations = list(list(
            text      = paste("Simulation error:", result$message),
            x         = 0.5,
            y         = 0.5,
            xref      = "paper",
            yref      = "paper",
            showarrow = FALSE,
            font      = list(size = 16, color = "red")
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    } else {
      data_obj <- get_obj(input$data_choice)
      log_y    <- isTRUE(input$log_y)

      obs_arg <- if (!is.null(data_obj)) data_obj else shiny::req(FALSE)

      p <- tryCatch(
        result$plot(obs = obs_arg, log = log_y, print = FALSE),
        error = function(e) NULL
      )

      if (is.null(p)) {
        plotly::plot_ly() |>
          plotly::layout(
            annotations = list(list(
              text      = "Simulation error",
              x         = 0.5,
              y         = 0.5,
              xref      = "paper",
              yref      = "paper",
              showarrow = FALSE,
              font      = list(size = 16, color = "red")
            )),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      } else {
        p$p
      }
    }
  })

  # --- exit button -----------------------------------------------------------

  shiny::observeEvent(input$exit_btn, {
    shiny::stopApp()
  })
}
