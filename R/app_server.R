#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # --- helpers ---------------------------------------------------------------

  # Retrieve Pmetrics objects by class from the golem data_env option
  list_objects_of_class <- function(cls) {
    env <- golem::get_golem_options("data_env")
    if (is.null(env)) env <- .GlobalEnv
    nms <- ls(envir = env)
    nms[vapply(nms, function(n) inherits(get(n, envir = env), cls), logical(1))]
  }

  get_obj <- function(name) {
    if (is.null(name) || nchar(name) == 0) return(NULL)
    env <- golem::get_golem_options("data_env")
    if (is.null(env)) env <- .GlobalEnv
    if (exists(name, envir = env, inherits = FALSE)) get(name, envir = env) else NULL
  }

  # --- populate choosers on session start ------------------------------------

  shiny::observe({
    model_choices <- list_objects_of_class("PM_model")
    data_choices  <- list_objects_of_class("PM_data")

    shiny::updateSelectInput(
      session, "model_choice",
      choices  = c("Select a model..." = "", model_choices),
      selected = if (length(model_choices) > 0) model_choices[[1]] else ""
    )
    shiny::updateSelectInput(
      session, "data_choice",
      choices  = c("Select a dataset..." = "", data_choices),
      selected = if (length(data_choices) > 0) data_choices[[1]] else ""
    )
  })

  # --- reactive: selected model ----------------------------------------------

  selected_model <- shiny::reactive({
    shiny::req(nchar(input$model_choice) > 0)
    get_obj(input$model_choice)
  })

  # --- reactive: parameter names & midpoints ---------------------------------

  param_info <- shiny::reactive({
    model <- selected_model()
    shiny::req(model)
    params <- model$model_list$parameters
    pri    <- model$model_list$pri
    shiny::req(length(params) > 0, length(pri) > 0)

    midpoints <- vapply(seq_along(params), function(i) {
      (pri[[i]]$min + pri[[i]]$max) / 2
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

    inputs <- lapply(seq_along(params), function(i) {
      p    <- params[[i]]
      mid  <- midpoints[[i]]
      mn   <- pri[[i]]$min
      mx   <- pri[[i]]$max
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

    shiny::tagList(shiny::h6("Parameter values:"), inputs)
  })

  # --- reactive: build poppar from current input values ----------------------

  poppar_reactive <- shiny::reactive({
    info <- param_info()
    shiny::req(info)
    params <- info$params

    vals <- vapply(seq_along(params), function(i) {
      p <- params[[i]]
      v <- input[[paste0("param_", p)]]
      if (is.null(v) || is.na(v)) info$midpoints[[i]] else v
    }, numeric(1))

    df        <- as.data.frame(t(vals))
    names(df) <- params
    df
  })

  # --- reactive: run simulation ----------------------------------------------

  sim_result <- shiny::reactive({
    model  <- selected_model()
    data   <- get_obj(input$data_choice)
    poppar <- poppar_reactive()
    shiny::req(model, data, poppar, nrow(poppar) > 0)

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

  make_error_plot <- function(msg) {
    plotly::plot_ly() |>
      plotly::layout(
        annotations = list(list(
          text      = msg,
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
  }

  output$sim_plot <- plotly::renderPlotly({
    result <- sim_result()
    shiny::req(result)
    log_y <- isTRUE(input$log_y)

    if (inherits(result, "sim_error")) {
      return(make_error_plot(paste("Simulation error:", result$message)))
    }

    # Plot simulation quantiles without obs (PM_data not accepted by plot.PM_sim)
    out <- tryCatch(
      result$plot(log = log_y, print = FALSE),
      error = function(e) NULL
    )

    if (is.null(out)) {
      return(make_error_plot("Simulation error"))
    }

    p <- out$p

    # Overlay observed data from the selected PM_data object
    # (sim template uses -1 as missing sentinel, not NA, so use original data)
    obs_data <- tryCatch({
      sd <- get_obj(input$data_choice)$standard_data
      sd[!is.na(sd$out) & sd$evid == 0, c("time", "out")]
    }, error = function(e) NULL)

    if (!is.null(obs_data) && nrow(obs_data) > 0) {
      p <- p |>
        plotly::add_markers(
          data   = obs_data,
          x      = ~time,
          y      = ~out,
          marker = list(color = "black", symbol = "circle-open", size = 8),
          name   = "Observed",
          inherit = FALSE
        )
    }

    p
  })

  # --- exit ------------------------------------------------------------------

  shiny::observeEvent(input$exit_btn, {
    shiny::stopApp()
  })
}
