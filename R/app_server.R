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

  sim_error <- function(message) {
    structure(list(message = message), class = "sim_error")
  }

  selected_subject_ids <- function(data) {
    if (is.null(data) || is.null(data$standard_data$id)) return(NULL)
    ids <- unique(data$standard_data$id)
    ids <- ids[!is.na(ids)]
    selected <- input$subjects
    if (is.null(selected) || length(selected) == 0) return(ids[FALSE])
    ids[match(selected, as.character(ids), nomatch = 0)]
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

  # --- update outeq choices when data selection changes ----------------------

  shiny::observeEvent(input$data_choice, {
    data <- get_obj(input$data_choice)
    if (is.null(data)) return()
    ids <- unique(data$standard_data$id)
    ids <- ids[!is.na(ids)]
    outeqs <- sort(unique(
      data$standard_data$outeq[data$standard_data$evid == 0]
    ))
    outeqs <- outeqs[!is.na(outeqs)]
    if (length(outeqs) > 0) {
      shiny::updateSelectInput(session, "outeq",
        choices  = outeqs,
        selected = outeqs[[1]]
      )
    }
    shiny::updateSelectizeInput(
      session, "subjects",
      choices = stats::setNames(as.character(ids), as.character(ids)),
      selected = as.character(ids),
      server = TRUE
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

  current_parameter_values <- function(info) {
    vapply(seq_along(info$params), function(i) {
      value <- input[[paste0("param_", info$params[[i]])]]
      if (is.null(value) || !is.finite(value)) info$midpoints[[i]] else value
    }, numeric(1))
  }

  format_pri_number <- function(value) {
    format(signif(value, 10), scientific = FALSE, trim = TRUE)
  }

  make_pri_block <- function(info, values) {
    minimums <- values * 0.5
    maximums <- values * 1.5
    entries <- vapply(seq_along(info$params), function(i) {
      comma <- if (i < length(info$params)) "," else ""
      paste0(
        "  ", info$params[[i]], " = ab(",
        format_pri_number(minimums[[i]]), ", ",
        format_pri_number(maximums[[i]]), ")",
        comma
      )
    }, character(1))
    paste(c("pri = list(", entries, "),"), collapse = "\n")
  }

  # --- simulation helpers ----------------------------------------------------

  run_simulation <- function(poppar, nsim = 1000) {
    model <- selected_model()
    data <- get_obj(input$data_choice)
    include <- selected_subject_ids(data)

    if (is.null(data)) return(sim_error("Choose a PM_data object."))
    if (length(include) == 0) {
      return(sim_error("Select at least one subject to include."))
    }

    tryCatch(
      Pmetrics::PM_sim$new(
        poppar = poppar,
        data = data,
        model = model,
        include = include,
        nsim = nsim,
        predInt = 1,
        quiet = TRUE
      ),
      error = function(e) sim_error(conditionMessage(e))
    )
  }

  simulation_plots <- function(result) {
    outeq <- suppressWarnings(as.integer(input$outeq))
    if (is.na(outeq)) return(sim_error("Choose an output equation."))
    tryCatch(
      result$plot(
        log = isTRUE(input$log_y),
        outeq = outeq,
        print = FALSE,
        quiet = TRUE
      ),
      error = function(e) sim_error(conditionMessage(e))
    )
  }

  # --- reactive: run simulation ----------------------------------------------

  sim_result <- shiny::reactive({
    poppar <- poppar_reactive()
    shiny::req(poppar, nrow(poppar) > 0)
    run_simulation(poppar)
  })

  # --- search for a working parameter set -----------------------------------

  help_status <- shiny::reactiveVal(NULL)

  output$help_me_status <- shiny::renderUI({
    status <- help_status()
    if (is.null(status)) return(NULL)
    shiny::div(
      class = paste("help-status", paste0("help-status-", status$type)),
      status$message
    )
  })

  shiny::observeEvent(
    list(input$model_choice, input$data_choice, input$subjects, input$outeq),
    {
      help_status(NULL)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$help_me_btn, {
    model <- tryCatch(selected_model(), error = function(e) NULL)
    data <- get_obj(input$data_choice)
    info <- tryCatch(param_info(), error = function(e) NULL)

    if (is.null(model) || is.null(data) || is.null(info)) {
      shiny::showNotification(
        "Choose a model and dataset before starting the search.",
        type = "warning"
      )
      return()
    }
    if (length(selected_subject_ids(data)) == 0) {
      shiny::showNotification(
        "Select at least one subject before starting the search.",
        type = "warning"
      )
      return()
    }

    params <- info$params
    lower <- vapply(info$pri, function(x) x$min, numeric(1))
    upper <- vapply(info$pri, function(x) x$max, numeric(1))
    current <- current_parameter_values(info)

    # Start with the current values and midpoint, then cover each prior range
    # with a deterministic low-discrepancy sequence.
    candidates <- list(current, info$midpoints)
    for (trial in seq_len(28)) {
      fractions <- (trial * 0.61803398875 + seq_along(params) * 0.41421356237) %% 1
      candidate <- lower + fractions * (upper - lower)
      candidate[!is.finite(candidate)] <- info$midpoints[!is.finite(candidate)]
      candidates[[length(candidates) + 1]] <- candidate
    }

    help_status(list(type = "info", message = "Searching parameter ranges..."))
    found <- shiny::withProgress(
      message = "Searching for a successful simulation",
      value = 0,
      {
        answer <- NULL
        last_error <- NULL
        for (i in seq_along(candidates)) {
          shiny::setProgress(i / length(candidates),
            detail = paste("Trying candidate", i, "of", length(candidates))
          )
          candidate <- as.data.frame(t(candidates[[i]]))
          names(candidate) <- params
          result <- run_simulation(candidate, nsim = 1)
          if (inherits(result, "sim_error")) {
            last_error <- result$message
            next
          }
          plotted <- simulation_plots(result)
          if (!inherits(plotted, "sim_error")) {
            answer <- candidates[[i]]
            break
          }
          last_error <- plotted$message
        }
        list(values = answer, last_error = last_error)
      }
    )

    if (is.null(found$values)) {
      help_status(list(
        type = "error",
        message = "No working set was found in 30 attempts."
      ))
      shiny::showNotification(
        paste(
          "No successful parameter set was found.",
          if (!is.null(found$last_error)) found$last_error else ""
        ),
        type = "error",
        duration = 8
      )
      return()
    }

    for (i in seq_along(params)) {
      shiny::updateNumericInput(
        session,
        paste0("param_", params[[i]]),
        value = signif(found$values[[i]], 6)
      )
    }
    help_status(list(
      type = "success",
      message = "Working parameter values applied."
    ))
    shiny::showNotification(
      "Found a successful simulation and applied its parameters.",
      type = "message"
    )
  })

  # --- render plot -----------------------------------------------------------

  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub("\"", "&quot;", text, fixed = TRUE)
    text
  }

  wrap_error_lines <- function(text, width) {
    source_lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    source_lines <- trimws(gsub("[[:space:]]+", " ", source_lines))
    source_lines <- source_lines[nzchar(source_lines)]

    unlist(lapply(source_lines, function(line) {
      word_wrapped <- strwrap(line, width = width)
      unlist(lapply(word_wrapped, function(piece) {
        starts <- seq.int(1, nchar(piece), by = width)
        substring(piece, starts, pmin(starts + width - 1, nchar(piece)))
      }), use.names = FALSE)
    }), use.names = FALSE)
  }

  make_error_plot <- function(
    msg,
    plot_width = session$clientData$output_sim_plot_width,
    plot_height = session$clientData$output_sim_plot_height
  ) {
    msg <- gsub("\033\\[[0-9;]*m", "", msg, perl = TRUE)

    if (is.null(plot_width) || !is.finite(plot_width) || plot_width <= 0) {
      plot_width <- 800
    }
    if (is.null(plot_height) || !is.finite(plot_height) || plot_height <= 0) {
      plot_height <- 600
    }

    box_width <- max(100, min(560, plot_width - 48))
    characters_per_line <- max(12, floor(box_width / 7))
    maximum_message_lines <- max(1, floor((plot_height - 140) / 17))
    msg_lines <- wrap_error_lines(msg, characters_per_line)
    if (length(msg_lines) > maximum_message_lines) {
      msg_lines <- c(
        head(msg_lines, maximum_message_lines - 1),
        "[Additional details omitted]"
      )
    }
    msg <- paste(escape_html(msg_lines), collapse = "<br>")

    plotly::plot_ly(
      x = numeric(0),
      y = numeric(0),
      type = "scatter",
      mode = "markers"
    ) |>
      plotly::layout(
        annotations = list(list(
          text = paste0(
            "<b>Simulation couldn't run</b><br><br>",
            "<span style='color:#5f6368'>", msg, "</span><br><br>",
            "<span style='color:#6c757d'>Adjust parameters or use ",
            "<b>Help Me!</b><br>to find a working set.</span>"
          ),
          x         = 0.5,
          y         = 0.5,
          xref      = "paper",
          yref      = "paper",
          xanchor = "center",
          yanchor = "middle",
          showarrow = FALSE,
          align = "left",
          width = box_width,
          font = list(size = 11, color = "#842029"),
          bgcolor = "#fff8f8",
          bordercolor = "#e7b8bd",
          borderwidth = 1,
          borderpad = 12
        )),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#ffffff",
        margin = list(l = 12, r = 12, t = 12, b = 12)
      )
  }

  output$sim_plot <- plotly::renderPlotly({
    result <- sim_result()
    shiny::req(result)
    outeq <- as.integer(input$outeq)
    shiny::req(!is.na(outeq))

    if (inherits(result, "sim_error")) {
      return(make_error_plot(result$message))
    }

    # Plot simulation quantiles for the selected outeq
    out <- simulation_plots(result)

    if (inherits(out, "sim_error")) {
      return(make_error_plot(out$message))
    }

    p <- out$p
    if (is.null(p) && inherits(out, "plotly")) p <- out
    if (is.null(p)) {
      return(make_error_plot("The simulation completed, but no plot was returned."))
    }

    # Overlay observations for the selected outeq from the PM_data object
    obs_data <- tryCatch({
      sd <- get_obj(input$data_choice)$standard_data
      include <- selected_subject_ids(get_obj(input$data_choice))
      sd[
        !is.na(sd$out) & sd$evid == 0 & sd$outeq == outeq &
          sd$id %in% include,
        c("time", "out")
      ]
    }, error = function(e) NULL)

    if (!is.null(obs_data) && nrow(obs_data) > 0) {
      p <- p |>
        plotly::add_markers(
          data    = obs_data,
          x       = ~time,
          y       = ~out,
          marker  = list(color = "black", symbol = "circle-open", size = 8),
          name    = "Observed",
          inherit = FALSE
        )
    }

    p
  })

  # --- copy parameter priors and close ---------------------------------------

  shiny::observeEvent(input$copy_close_btn, {
    info <- tryCatch(param_info(), error = function(e) NULL)
    if (is.null(info)) {
      shiny::showNotification(
        "Choose a model before copying parameter priors.",
        type = "warning"
      )
      return()
    }

    values <- current_parameter_values(info)
    pri_text <- make_pri_block(info, values)
    session$sendCustomMessage(
      "copy_pri_and_close",
      list(
        text = pri_text,
        nonce = paste0(session$token, "-", input$copy_close_btn)
      )
    )
  })

  shiny::observeEvent(input$copy_close_result, {
    if (isTRUE(input$copy_close_result$success)) {
      shiny::stopApp()
    } else {
      shiny::showNotification(
        "The pri block could not be copied. The app was left open.",
        type = "error",
        duration = 8
      )
    }
  })

  shiny::observeEvent(input$close_btn, {
    shiny::stopApp()
  })
}
