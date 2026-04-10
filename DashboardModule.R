# Helper for consistent chart cards to reduce code duplication
create_chart_card <- function(ns, title_id, desc_id = NULL, plot_id, height = "400px", style = "") {
  # Unique ID for the card to handle fullscreen toggling
  card_dom_id <- ns(paste0("card_", plot_id))
  
  div(
    id = card_dom_id,
    class = "content-card",
    style = paste("position: relative; transition: all 0.3s ease;", style),
    
    # Header with Title and Expand Button
    div(
      class = "d-flex justify-content-between align-items-start",
      div(style = "flex-grow: 1;", uiOutput(ns(title_id))),
      tags$button(
        class = "btn btn-sm btn-ghost-light",
        style = "color: rgba(255,255,255,0.5); min-width: 30px; margin-left: 10px; background: transparent; border: none; font-size: 1.2rem; line-height: 1;",
        title = "Toggle Fullscreen",
        onclick = sprintf("
          var card = document.getElementById('%s');
          card.classList.toggle('fullscreen-mode');
          var isFull = card.classList.contains('fullscreen-mode');
          this.innerText = isFull ? '✕' : '⤢';
          // Trigger resize for Plotly after transition
          setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 300);
        ", card_dom_id),
        "⤢"
      )
    ),
    
    if (!is.null(desc_id)) p(class = "info-text", uiOutput(ns(desc_id))),
    
    # Wrapper for plot to handle fullscreen height
    div(
      class = "chart-wrapper",
      plotlyOutput(ns(plot_id), height = height)
    )
  )
}

# Helper for consistent table cards with fullscreen support
create_table_card <- function(ns, title_id, desc_id = NULL, table_id, style = "") {
  card_dom_id <- ns(paste0("card_", table_id))
  
  div(
    id = card_dom_id,
    class = "content-card",
    style = paste("position: relative; transition: all 0.3s ease;", style),
    
    div(
      class = "d-flex justify-content-between align-items-start",
      div(style = "flex-grow: 1;", uiOutput(ns(title_id))),
      tags$button(
        class = "btn btn-sm btn-ghost-light",
        style = "color: rgba(255,255,255,0.5); min-width: 30px; margin-left: 10px; background: transparent; border: none; font-size: 1.2rem; line-height: 1;",
        title = "Toggle Fullscreen",
        onclick = sprintf("
          var card = document.getElementById('%s');
          card.classList.toggle('fullscreen-mode');
          var isFull = card.classList.contains('fullscreen-mode');
          this.innerText = isFull ? '✕' : '⤢';
        ", card_dom_id),
        "⤢"
      )
    ),
    
    if (!is.null(desc_id)) p(class = "info-text", uiOutput(ns(desc_id))),
    
    div(
      class = "table-wrapper",
      style = "width: 100%; overflow-x: auto; min-height: 200px;",
      DT::dataTableOutput(ns(table_id))
    )
  )
}

# Helper for consistent stat cards
create_stat_card <- function(icon, value, label) {
  div(class = "value-box-custom",
      div(class = "value-box-icon", icon),
      div(class = "value-box-value", value),
      div(class = "value-box-label", label))
}

# --- Helpers for Reducing Boilerplate & Speed ---

render_title <- function(key, get_lang_fn, icon = NULL) {
  renderUI({
    lang <- get_lang_fn()
    txt <- t(key, lang)
    if (!is.null(icon)) div(class = "chart-title", span(icon), span(txt))
    else div(class = "chart-title", txt)
  })
}

render_desc <- function(key, get_lang_fn) {
  renderUI({
    t(key, get_lang_fn())
  })
}

# UI Module
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  date_bounds <- tryCatch({
    input_data <- generate_metrics()
    if (is.null(input_data) || nrow(input_data) == 0 || !"datum" %in% names(input_data)) {
      stop("Date bounds unavailable")
    }

    all_dates <- sort(unique(as.Date(input_data$datum)))
    min_date <- min(all_dates, na.rm = TRUE)
    max_date <- max(all_dates, na.rm = TRUE)
    default_start_index <- max(1L, length(all_dates) - 59L)

    list(
      min = min_date,
      max = max_date,
      start = all_dates[default_start_index],
      end = max_date
    )
  }, error = function(e) {
    fallback_end <- Sys.Date()
    list(
      min = fallback_end - 365,
      max = fallback_end,
      start = fallback_end - 210,
      end = fallback_end
    )
  })
  
  metric_choices <- setNames(
    c("balls", "sums", "odds_evens", "table", "difference", "lag"),
    c(
      t("metric_balls", lang),
      t("metric_sums", lang),
      t("metric_odds_evens", lang),
      t("metric_tables", lang),
      t("metric_difference", lang),
      t("metric_lag", lang)
    )
  )
  
  tagList(
    tags$style(HTML("\n      .preset-range-btn {\n        border-radius: 999px !important;\n        font-size: 0.78rem !important;\n        border-color: rgba(255, 255, 255, 0.35) !important;\n        color: rgba(255, 255, 255, 0.78) !important;\n        background: transparent !important;\n        transition: all 0.2s ease !important;\n      }\n      .preset-range-btn:hover {\n        border-color: rgba(255, 255, 255, 0.7) !important;\n        color: #ffffff !important;\n      }\n      .preset-range-btn.active-preset {\n        border-color: rgba(56, 189, 248, 0.95) !important;\n        background: linear-gradient(120deg, rgba(14, 165, 233, 0.98), rgba(56, 189, 248, 0.98)) !important;\n        color: #04111f !important;\n        font-weight: 700 !important;\n        box-shadow: 0 8px 20px rgba(14, 165, 233, 0.28) !important;\n      }\n      .date-range-clean .input-daterange.input-group {\n        gap: 8px;\n      }\n      .date-range-clean .input-daterange.input-group > :not(:first-child) {\n        margin-left: 0 !important;\n      }\n      .date-range-clean .input-daterange .input-group-text {\n        border: none !important;\n        background: transparent !important;\n        color: rgba(232, 234, 237, 0.78) !important;\n        font-weight: 600 !important;\n        padding: 0 2px !important;\n      }\n      .date-range-clean .input-daterange .form-control {\n        border-radius: 12px !important;\n      }\n    ")),
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    div(
      class = "journey-card",
      div(class = "journey-title", t("input_journey_title", lang)),
      p(class = "journey-subtitle", t("input_journey_subtitle", lang)),
      uiOutput(ns("journeySteps"))
    ),
    sliderInput(ns("range"), 
                t("input_ball_range", lang), 
                min = 1, max = 49, value = c(1,49), step = 1),
    selectInput(ns("metric"), 
                t("input_analysis_type", lang), 
                choices = metric_choices, 
                selected = "balls"),
    div(
      class = "date-range-clean",
      dateRangeInput(ns("dateRange"),
               t("input_date_window", lang),
               start = date_bounds$start,
               end = date_bounds$end,
               min = date_bounds$min,
               max = date_bounds$max,
               format = "yyyy-mm-dd",
               startview = "year",
               weekstart = 1,
               separator = if (lang == "de") "bis" else "to")
    ),
    div(
      style = "margin-top: 10px; margin-bottom: 10px;",
      div(
        style = "color: rgba(255, 255, 255, 0.65); font-size: 0.78rem; margin-bottom: 8px; letter-spacing: 0.04em; text-transform: uppercase;",
        t("input_quick_ranges", lang)
      ),
      div(
        class = "d-flex flex-wrap gap-2",
        actionButton(
          ns("preset3m"),
          t("input_preset_3m", lang),
          class = "btn btn-outline-light btn-sm preset-range-btn"
        ),
        actionButton(
          ns("preset6m"),
          t("input_preset_6m", lang),
          class = "btn btn-outline-light btn-sm preset-range-btn"
        ),
        actionButton(
          ns("preset1y"),
          t("input_preset_1y", lang),
          class = "btn btn-outline-light btn-sm preset-range-btn"
        ),
        actionButton(
          ns("presetAll"),
          t("input_preset_all", lang),
          class = "btn btn-outline-light btn-sm preset-range-btn"
        )
      )
    ),
    actionButton(ns("refresh"), 
                 t("input_refresh", lang), 
                 class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;"),
    div(
      class = "share-view-panel",
      actionButton(
        ns("copyView"),
        t("input_copy_view", lang),
        icon = icon("link"),
        class = "btn btn-outline-light w-100 copy-view-btn"
      ),
      p(class = "share-view-help", t("input_copy_view_help", lang))
    )
  )
}

# Module Server - FULLY OPTIMIZED
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    metric_ids <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
    preset_ids <- c("preset3m", "preset6m", "preset1y", "presetAll")
    get_lang <- reactive({
      query <- parseQueryString(isolate(session$clientData$url_search))
      lang <- query$lang %||% "de"
      if (is.null(lang) || !nzchar(lang)) "de" else as.character(lang)
    })
    journey_state <- reactiveValues(
      filters_applied = FALSE,
      metric_chosen = FALSE,
      refreshed = FALSE
    )

    date_domain <- tryCatch({
      input_data <- generate_metrics()
      if (is.null(input_data) || nrow(input_data) == 0 || !"datum" %in% names(input_data)) {
        stop("Date bounds unavailable")
      }

      all_dates <- sort(unique(as.Date(input_data$datum)))
      list(
        min = min(all_dates, na.rm = TRUE),
        max = max(all_dates, na.rm = TRUE)
      )
    }, error = function(e) {
      fallback_end <- Sys.Date()
      list(min = fallback_end - 365, max = fallback_end)
    })

    preset_target_range <- function(days_back = NULL) {
      max_date <- as.Date(date_domain$max)
      min_date <- as.Date(date_domain$min)

      if (is.null(days_back)) {
        start_date <- min_date
      } else {
        start_date <- max(min_date, max_date - as.integer(days_back))
      }

      c(start_date, max_date)
    }

    set_active_preset <- function(active_id = NULL) {
      for (preset_id in preset_ids) {
        shinyjs::removeClass(id = preset_id, class = "active-preset")
      }

      if (!is.null(active_id) && active_id %in% preset_ids) {
        shinyjs::addClass(id = active_id, class = "active-preset")
      }
    }

    apply_preset_range <- function(days_back = NULL, preset_id = NULL) {
      target_range <- preset_target_range(days_back)

      updateDateRangeInput(
        session,
        "dateRange",
        start = target_range[[1]],
        end = target_range[[2]],
        min = as.Date(date_domain$min),
        max = as.Date(date_domain$max)
      )

      set_active_preset(preset_id)
    }

    observeEvent(input$range, {
      journey_state$filters_applied <- TRUE
    }, ignoreInit = TRUE)

    observeEvent(input$dateRange, {
      journey_state$filters_applied <- TRUE
    }, ignoreInit = TRUE)

    observeEvent(input$metric, {
      journey_state$metric_chosen <- TRUE
    }, ignoreInit = TRUE)

    observeEvent(input$refresh, {
      journey_state$refreshed <- TRUE
    }, ignoreInit = TRUE)

    output$journeySteps <- renderUI({
      lang <- get_lang()
      step_done <- c(
        isTRUE(journey_state$filters_applied),
        isTRUE(journey_state$metric_chosen),
        isTRUE(journey_state$refreshed)
      )
      step_labels <- c(
        t("input_journey_step_filters", lang),
        t("input_journey_step_metric", lang),
        t("input_journey_step_refresh", lang)
      )
      first_incomplete <- which(!step_done)
      active_step <- if (length(first_incomplete) > 0) first_incomplete[[1]] else NA_integer_

      step_nodes <- lapply(seq_along(step_labels), function(i) {
        state_class <- if (step_done[[i]]) {
          "is-complete"
        } else if (!is.na(active_step) && i == active_step) {
          "is-current"
        } else {
          "is-upcoming"
        }

        div(
          class = paste("journey-step", state_class),
          span(class = "journey-index", i),
          span(class = "journey-text", step_labels[[i]])
        )
      })

      tagList(
        div(class = "journey-steps", step_nodes),
        if (all(step_done)) {
          div(class = "journey-complete", t("input_journey_complete", lang))
        }
      )
    })

    restore_view_from_query <- function() {
      query <- parseQueryString(isolate(session$clientData$url_search))
      if (length(query) == 0) {
        return(invisible(NULL))
      }

      query_metric <- as.character(query$metric %||% "")
      if (nzchar(query_metric) && query_metric %in% metric_ids) {
        updateSelectInput(session, "metric", selected = query_metric)
      }

      range_min <- suppressWarnings(as.integer(query$range_min))
      range_max <- suppressWarnings(as.integer(query$range_max))
      if (!is.na(range_min) && !is.na(range_max)) {
        range_min <- max(1L, min(49L, range_min))
        range_max <- max(1L, min(49L, range_max))

        if (range_min > range_max) {
          tmp <- range_min
          range_min <- range_max
          range_max <- tmp
        }

        if ((range_max - range_min) < minDistance) {
          range_min <- max(1L, range_max - minDistance)
        }

        updateSliderInput(session, "range", value = c(range_min, range_max))
      }

      query_from <- suppressWarnings(as.Date(query$from))
      query_to <- suppressWarnings(as.Date(query$to))
      if (!is.na(query_from) && !is.na(query_to)) {
        min_date <- as.Date(date_domain$min)
        max_date <- as.Date(date_domain$max)

        query_from <- max(min_date, min(query_from, max_date))
        query_to <- max(min_date, min(query_to, max_date))
        if (query_from > query_to) {
          tmp <- query_from
          query_from <- query_to
          query_to <- tmp
        }

        updateDateRangeInput(
          session,
          "dateRange",
          start = query_from,
          end = query_to,
          min = min_date,
          max = max_date
        )
      }
    }

    session$onFlushed(function() {
      restore_view_from_query()
    }, once = TRUE)

    observeEvent(input$preset3m, {
      apply_preset_range(days_back = 90L, preset_id = "preset3m")
    }, ignoreInit = TRUE)

    observeEvent(input$preset6m, {
      apply_preset_range(days_back = 180L, preset_id = "preset6m")
    }, ignoreInit = TRUE)

    observeEvent(input$preset1y, {
      apply_preset_range(days_back = 365L, preset_id = "preset1y")
    }, ignoreInit = TRUE)

    observeEvent(input$presetAll, {
      apply_preset_range(days_back = NULL, preset_id = "presetAll")
    }, ignoreInit = TRUE)

    observeEvent(input$dateRange, {
      selected_range <- as.Date(input$dateRange)
      if (length(selected_range) != 2 || any(is.na(selected_range))) {
        set_active_preset(NULL)
        return(invisible(NULL))
      }

      matched_preset <- NULL
      preset_3m <- preset_target_range(90L)
      preset_6m <- preset_target_range(180L)
      preset_1y <- preset_target_range(365L)
      preset_all <- preset_target_range(NULL)

      if (identical(selected_range, preset_3m)) {
        matched_preset <- "preset3m"
      } else if (identical(selected_range, preset_6m)) {
        matched_preset <- "preset6m"
      } else if (identical(selected_range, preset_1y)) {
        matched_preset <- "preset1y"
      } else if (identical(selected_range, preset_all)) {
        matched_preset <- "presetAll"
      }

      set_active_preset(matched_preset)
    }, ignoreInit = TRUE)

    observeEvent(input$copyView, {
      req(!is.null(input$range), length(input$range) == 2)
      req(!is.null(input$dateRange), length(input$dateRange) == 2)

      lang <- get_lang()
      selected_dates <- as.Date(input$dateRange)
      selected_metric <- as.character(input$metric %||% "balls")
      if (!(selected_metric %in% metric_ids)) {
        selected_metric <- "balls"
      }

      safe_lang <- as.character(parseQueryString(isolate(session$clientData$url_search))$lang %||% lang)
      safe_lang <- if (nzchar(safe_lang)) safe_lang else "de"

      query_parts <- c(
        paste0("lang=", URLencode(safe_lang, reserved = TRUE)),
        paste0("metric=", URLencode(selected_metric, reserved = TRUE)),
        paste0("range_min=", as.integer(input$range[[1]])),
        paste0("range_max=", as.integer(input$range[[2]])),
        paste0("from=", format(min(selected_dates), "%Y-%m-%d")),
        paste0("to=", format(max(selected_dates), "%Y-%m-%d"))
      )

      base_url <- sub("\\?.*$", "", isolate(session$clientData$url_href))
      share_url <- paste0(base_url, "?", paste(query_parts, collapse = "&"))

      session$sendCustomMessage("copyViewLink", list(
        url = share_url,
        success = t("input_copy_view_success", lang),
        failure = t("input_copy_view_fail", lang)
      ))
    }, ignoreInit = TRUE)
    
    # ✅ FIX 1: Prevent cascade updates - only update if actually out of bounds
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        updateSliderInput(session, "range", value = newRange)
      }
    }, ignoreInit = TRUE)
    
    # ✅ FIX 2: Debounce range slider - waits 300ms after user stops dragging
    range_debounced <- reactive({
      input$range
    }) %>% debounce(300)

    date_range_debounced <- reactive({
      input$dateRange
    }) %>% debounce(300)
    
    # Throttle refresh button
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(300)
    
    # Return debounced and throttled values
    return(reactive({
      list(
        range = range_debounced(),       # ✅ DEBOUNCED - only updates every 300ms
        metric = input$metric,           # Direct - instant response
        dateRange = date_range_debounced(),
        refresh = refresh_throttled(),   # ✅ THROTTLED - only every 300ms
        lang = get_lang()
      )
    }))
  })
}

# Dashboard UI
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Add CSS for fullscreen mode and card interactions
    tags$style(HTML("
      .fullscreen-mode {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        z-index: 99999 !important;
        background-color: #0a0e27 !important; /* Match theme bg */
        padding: 30px !important;
        margin: 0 !important;
        border-radius: 0 !important;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      .fullscreen-mode .chart-wrapper {
        flex-grow: 1;
        height: 100% !important;
        position: relative;
      }
      .fullscreen-mode .plotly {
        height: 100% !important;
      }
      .fullscreen-mode .table-wrapper {
        flex-grow: 1;
        height: 100% !important;
        overflow: auto;
        padding-bottom: 20px;
      }
      .content-card:hover {
        border-color: rgba(34, 211, 238, 0.45);
        box-shadow: 0 8px 24px rgba(2, 11, 24, 0.45);
      }
    ")),
    
    # Skeleton for initial load
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card",
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px; margin-bottom: 20px;"),
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px;")
    ),
    
    # Container for all metrics (all pre-rendered, hidden via CSS)
    div(id = ns("metricsContainer"),
        style = "display: none;",
      uiOutput(ns("experienceStrip")),
        
        div(id = ns("metric-balls"), 
            style = "display: none;",
            ballsMetricUI(ns("balls"))),
        
        div(id = ns("metric-sums"), 
            style = "display: none;",
            sumsMetricUI(ns("sums"))),
        
        div(id = ns("metric-odds_evens"), 
            style = "display: none;",
            oddsEvensMetricUI(ns("odds_evens"))),
        
        div(id = ns("metric-table"), 
            style = "display: none;",
            tableMetricUI(ns("table"))),
        
        div(id = ns("metric-difference"), 
            style = "display: none;",
            differenceMetricUI(ns("difference"))),
        
        div(id = ns("metric-lag"), 
            style = "display: none;",
            lagMetricUI(ns("lag")))
    )
  )
}

# Server Module - FULLY OPTIMIZED FOR FREE TIER (No cachem Ram overhead)
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data once at startup
    metrics_data <- generate_metrics()
    base_row_count <- nrow(metrics_data)
    filter_cache <- reactiveVal(list(key = NULL, data = NULL))
    
    active_metric <- reactive({
      req(input_controls()$metric)
      input_controls()$metric
    })

    get_lang <- reactive({
      lang <- input_controls()$lang %||% "de"
      if (is.null(lang) || !nzchar(lang)) "de" else as.character(lang)
    })

    metric_label <- function(metric_id, lang) {
      switch(
        metric_id,
        balls = t("metric_balls", lang),
        sums = t("metric_sums", lang),
        odds_evens = t("metric_odds_evens", lang),
        table = t("metric_tables", lang),
        difference = t("metric_difference", lang),
        lag = t("metric_lag", lang),
        metric_id
      )
    }
    
    metric_is_active <- function(metric_name) {
      reactive({
        identical(active_metric(), metric_name)
      })
    }
    
    # ✅ FAST FILTERING PIPELINE
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$dateRange,
        input_controls()$range),  # Uses debounced range
      {
        data <- metrics_data
        req(!is.null(data) && nrow(data) > 0)

        date_vals <- input_controls()$dateRange
        if (is.null(date_vals) || length(date_vals) != 2) {
          date_vals <- c(min(data$datum, na.rm = TRUE), max(data$datum, na.rm = TRUE))
        }
        date_vals <- as.Date(date_vals)
        req(length(date_vals) == 2, !any(is.na(date_vals)))

        date_from <- min(date_vals)
        date_to <- max(date_vals)
        range_vals <- input_controls()$range
        cache_key <- paste(as.character(date_from), as.character(date_to), range_vals[1], range_vals[2], sep = "|")
        cached <- filter_cache()
        
        if (!is.null(cached$key) && identical(cached$key, cache_key) && !is.null(cached$data)) {
          return(cached$data)
        }

        data <- data %>% filter(datum >= date_from & datum <= date_to)
        req(nrow(data) > 0)
        
        num_from <- as.numeric(range_vals[1])
        num_to <- as.numeric(range_vals[2])
        
        data <- data %>% filter(ball_1 >= num_from & ball_6 <= num_to)
        req(nrow(data) > 0)
        filter_cache(list(key = cache_key, data = data))
        
        data
      },
      ignoreNULL = TRUE
    )

    output$experienceStrip <- renderUI({
      req(input_controls()$metric)

      lang <- get_lang()
      metric_name <- metric_label(input_controls()$metric, lang)
      date_separator <- if (identical(lang, "de")) " bis " else " to "
      range_vals <- input_controls()$range %||% c(1, 49)
      date_vals <- as.Date(input_controls()$dateRange)
      if (length(date_vals) != 2 || any(is.na(date_vals))) {
        all_dates <- as.Date(metrics_data$datum)
        date_vals <- c(min(all_dates, na.rm = TRUE), max(all_dates, na.rm = TRUE))
      }

      filtered_rows <- tryCatch(nrow(filtered_data()), error = function(e) 0L)

      div(
        class = "experience-strip",
        div(
          class = "experience-pill",
          span(class = "pill-label", t("dashboard_strip_metric", lang)),
          span(class = "pill-value", metric_name)
        ),
        div(
          class = "experience-pill",
          span(class = "pill-label", t("dashboard_strip_window", lang)),
          span(
            class = "pill-value",
            paste(format(min(date_vals), "%Y-%m-%d"), format(max(date_vals), "%Y-%m-%d"), sep = date_separator)
          )
        ),
        div(
          class = "experience-pill",
          span(class = "pill-label", t("dashboard_strip_range", lang)),
          span(class = "pill-value", paste(range_vals[[1]], range_vals[[2]], sep = "-"))
        ),
        div(
          class = "experience-pill",
          span(class = "pill-label", t("dashboard_strip_draws", lang)),
          span(class = "pill-value", format(filtered_rows, big.mark = ",", scientific = FALSE, trim = TRUE))
        ),
        p(class = "experience-strip-hint", t("dashboard_strip_hint", lang))
      )
    })
    
    # Track which servers are initialized
    initialized_servers <- reactiveVal(list())
    
    initialize_server <- function(metric) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return()
      
      switch(metric,
             "balls" = ballsMetricServer(
               "balls", filtered_data, input_controls,
               base_row_count = base_row_count,
               is_active = metric_is_active("balls")
             ),
             "sums" = sumsMetricServer(
               "sums", filtered_data,
               is_active = metric_is_active("sums")
             ),
             "odds_evens" = oddsEvensMetricServer(
               "odds_evens", filtered_data,
               is_active = metric_is_active("odds_evens")
             ),
             "table" = tableMetricServer(
               "table", filtered_data,
               is_active = metric_is_active("table")
             ),
             "difference" = differenceMetricServer(
               "difference", filtered_data,
               is_active = metric_is_active("difference")
             ),
             "lag" = lagMetricServer(
               "lag", filtered_data,
               is_active = metric_is_active("lag")
             )
      )
      
      initialized_servers(c(already_init, metric))
    }
    
    # ✅ STEP 1: Show first metric immediately (high priority = 100)
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Hide skeleton, show container
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      # Initialize current metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, priority = 100) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # ✅ STEP 3: Fast metric switching (priority = 50)
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Hide all other metrics
      lapply(all_metrics, function(m) {
        if (m != metric) {
          shinyjs::hide(id = paste0("metric-", m))
        }
      })
      
      # Initialize and show selected metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 50)
    
  })
}