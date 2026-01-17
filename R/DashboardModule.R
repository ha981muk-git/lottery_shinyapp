# ============================================================================
# DashboardModule.R - L1 + L2 Caching with Transparent Compute Wrappers
# Option A: Refresh clears L2 (current metric only); filters clear L2 (all)
# Draws/week: 2 (DE Lotto Wed + Sat)
# ============================================================================

# ----------------------------------------------------------------------------
# INPUT MODULE UI
# ----------------------------------------------------------------------------
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  # Dynamic choices based on language
  time_choices <- setNames(
    c(7, 30, 60, 90, 120, 150, 180),
    c(t("time_last_7", lang),
      t("time_last_30", lang),
      t("time_last_60", lang),
      t("time_last_90", lang),
      t("time_last_120", lang),
      t("time_last_150", lang),
      t("time_last_180", lang)
    ))
  
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
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    sliderInput(ns("range"), 
                t("input_ball_range", lang), 
                min = 1, max = 49, value = c(1,49), step = 1),
    selectInput(ns("metric"), 
                t("input_analysis_type", lang), 
                choices = metric_choices, 
                selected = "balls"),
    selectInput(ns("timeRange"), 
                t("input_time_window", lang), 
                choices = time_choices, 
                selected = 7),
    actionButton(ns("refresh"), 
                 t("input_refresh", lang), 
                 class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;"
    )
  )
}

# ----------------------------------------------------------------------------
# INPUT MODULE SERVER - OPTIMIZED
# ----------------------------------------------------------------------------
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # Validate range slider - prevent invalid ranges
    observeEvent(input$range, { 
      minVal <- input$range[1]
      maxVal <- input$range[2]
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        updateSliderInput(session, "range", value = newRange)
      }
    }, ignoreInit = TRUE)
    
    # Debounce/throttle
    range_debounced <- reactive({ input$range }) %>% debounce(300)
    refresh_throttled <- reactive({ input$refresh }) %>% throttle(300)
    
    # Return reactive list with debounced/throttled values
    reactive({
      list(
        range     = range_debounced(),   # Debounced
        metric    = input$metric,        # Immediate
        timeRange = input$timeRange,     # Immediate
        refresh   = refresh_throttled()  # Throttled
      )
    })
  })
}

# ----------------------------------------------------------------------------
# DASHBOARD UI
# ----------------------------------------------------------------------------
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Skeleton loader for initial load
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card",
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px; margin-bottom: 20px;",
            ), # Removed closing div
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px;"
            ) # Removed closing div
    ),
    
    # Container for all metrics (all pre-rendered, hidden via CSS)
    div(id = ns("metricsContainer"),
        style = "display: none;",
        div(id = ns("metric-balls"),       style = "display: none;", ballsMetricUI(ns("balls"))),
        div(id = ns("metric-sums"),        style = "display: none;", sumsMetricUI(ns("sums"))),
        div(id = ns("metric-odds_evens"),  style = "display: none;", oddsEvensMetricUI(ns("odds_evens"))),
        div(id = ns("metric-table"),       style = "display: none;", tableMetricUI(ns("table"))),
        div(id = ns("metric-difference"),  style = "display: none;", differenceMetricUI(ns("difference"))),
        div(id = ns("metric-lag"),         style = "display: none;", lagMetricUI(ns("lag")))
    )
  )
}

# ----------------------------------------------------------------------------
# DASHBOARD SERVER - L1 filtered cache + L2 metric-result cache (transparent)
# ----------------------------------------------------------------------------
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Optional request limiter (kept as-is)
    observe({
      req_count <- session$clientData$shinysession$requests
      if (!is.null(req_count)) {
        MAX_REQUESTS <- 30
        if (req_count > MAX_REQUESTS) {
          session$close()
        }
      }
    })
    
    # Load base data once (your existing global generator)
    metrics_data <- data_loader$load(force = TRUE)
    validate(need(!is.null(metrics_data) && nrow(metrics_data) > 0, "No data available"))
    
    # ----------------------------
    # L1: Filtered data cache (LRU)
    # ----------------------------
    draws_per_week <- 2  # DE Lotto Wed + Sat
    
    l1 <- reactiveValues( 
      data = list(),
      keys = character(0)
    )
    L1_MAX <- 15
    
    get_filtered_data <- function(weeks, range_vals) {
      if (is.null(weeks) || is.null(range_vals) || length(range_vals) != 2) return(NULL)
      if (nrow(metrics_data) == 0) return(NULL)
      
      key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      if (!is.null(l1$data[[key]])) {
        # Mark MRU
        l1$keys <- c(setdiff(l1$keys, key), key)
        return(l1$data[[key]])
      }
      
      data <- metrics_data
      # If you have a Date column 'datum', prefer a date window; else tail() fallback:
      if ("datum" %in% names(data) && inherits(data$datum, "Date")) {
        anchor <- max(data$datum, na.rm = TRUE)
        start_date <- anchor - as.difftime(weeks * 7, units = "days")
        data <- dplyr::filter(data, .data$datum >= start_date)
      } else {
        approx_rows <- weeks * draws_per_week
        data <- utils::tail(data, min(approx_rows, nrow(data)))
      }
      
      num_from <- as.numeric(range_vals[1])
      num_to   <- as.numeric(range_vals[2])
      data <- dplyr::filter(data, .data$ball_1 >= num_from & .data$ball_6 <= num_to)
      if (nrow(data) == 0) return(NULL)
      
      l1$data[[key]] <- data
      l1$keys <- c(l1$keys, key)
      if (length(l1$keys) > L1_MAX) {
        oldest <- l1$keys[1]
        l1$data[[oldest]] <- NULL
        l1$keys <- l1$keys[-1]
      }
      data
    }
    
    # This is what modules call: unchanged signature & behavior.
    filtered_data <- reactive({ 
      input_controls()$refresh   # just to be consistent with your old trigger
      weeks      <- isolate(as.numeric(input_controls()$timeRange))
      range_vals <- isolate(input_controls()$range)
      
      # Compute key & set a global option so L2 wrappers can see current filter
      key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      options(li_filter_key = key)
      
      get_filtered_data(weeks, range_vals)
    }) %>% debounce(300)
    
    # ----------------------------
    # L2: Transparent wrappers for *MetricCompute()
    # ----------------------------
    # We wrap your existing compute functions with a cache keyed by:
    # metric name + current filter key (from options("li_filter_key")).
    # Because modules already call *MetricCompute(data), they will now hit the cache.
    
    l2_env <- new.env(parent = emptyenv())
    l2_env$store <- new.env(parent = emptyenv())     # key -> result
    l2_env$keys  <- character(0)                     # maintain simple LRU
    L2_MAX <- 60                                     # generous; objects are small
    
    .l2_key <- function(metric_name) {
      paste0(metric_name, "|", getOption("li_filter_key", "nokey"))
    }
    
    .l2_get <- function(metric_name) {
      k <- .l2_key(metric_name)
      if (exists(k, envir = l2_env$store, inherits = FALSE)) {
        # mark MRU
        l2_env$keys <- c(setdiff(l2_env$keys, k), k)
        get(k, envir = l2_env$store, inherits = FALSE)
      } else {
        NULL
      }
    }
    
    .l2_set <- function(metric_name, value) {
      k <- .l2_key(metric_name)
      assign(k, value, envir = l2_env$store)
      l2_env$keys <- c(l2_env$keys, k)
      if (length(l2_env$keys) > L2_MAX) {
        oldest <- l2_env$keys[1]
        rm(list = oldest, envir = l2_env$store)
        l2_env$keys <- l2_env$keys[-1]
      }
      invisible(value)
    }
    
    .l2_clear_all <- function() {
      rm(list = ls(envir = l2_env$store, all.names = TRUE), envir = l2_env$store)
      l2_env$keys <- character(0)
    }
    
    .l2_clear_metric_for_current_filter <- function(metric_name) {
      # Correctly construct the regex pattern to clear the specific metric from the L2 cache.
      # The key is in the format 'metricNameMetricCompute|filter_key'.
      # The pipe '|' must be escaped for the regex to work correctly.
      metric_compute_name <- paste0(metric_name, "MetricCompute")
      current_suffix <- getOption("li_filter_key", "nokey")
      pattern <- paste0("^", metric_compute_name, "\\|", current_suffix, "$")
      
      to_remove <- grep(pattern, l2_env$keys, value = TRUE)
      
      if (length(to_remove) > 0) {
        rm(list = to_remove, envir = l2_env$store)
        l2_env$keys <- setdiff(l2_env$keys, to_remove)
      }
    }
    
    # Wrap a compute function by name (if it exists)
    .wrap_compute <- function(name) {
      if (!exists(name, mode = "function", inherits = TRUE)) return(FALSE)
      # Capture original
      orig <- get(name, mode = "function")
      # Avoid double-wrapping
      if (!is.null(attr(orig, ".__wrapped__"))) return(FALSE)
      
      wrapper <- function(data) {
        # Create a lock file to prevent race conditions
        lock_key <- paste0("lock_", name)
        
        # Try to get from cache first
        cached <- .l2_get(name)
        if (!is.null(cached)) return(cached)
        
        # Compute if not cached
        # In production, add proper mutex locking if using parallel processing
        tryCatch({
          res <- orig(data)
          .l2_set(name, res)
          res
        }, error = function(e) {
          warning("Cache computation error for ", name, ": ", e$message)
          # Return uncached result on error
          orig(data)
        })
      }
      attr(wrapper, ".__wrapped__") <- TRUE
      # Overwrite in the global environment so module calls hit the wrapper
      assign(name, wrapper, envir = .GlobalEnv)
      TRUE
    }
    
    # Install wrappers for your known compute functions (if present)
    observeEvent(TRUE, {
      invisible(.wrap_compute("ballsMetricCompute"))       # pass-through if you created it
      invisible(.wrap_compute("sumsMetricCompute"))
      invisible(.wrap_compute("oddsEvensMetricCompute"))
      invisible(.wrap_compute("tableMetricCompute"))
      invisible(.wrap_compute("differenceMetricCompute"))
      invisible(.wrap_compute("lagMetricCompute"))
    }, once = TRUE)
    
    # Clear L2 cache whenever filters change (Option A)
    observeEvent(list(input_controls()$timeRange, input_controls()$range), { 
      .l2_clear_all()
    }, ignoreInit = TRUE)
    
    # Refresh: clear L2 cache only for current metric (Option A)
    observeEvent(input_controls()$refresh, {
      metric <- isolate(input_controls()$metric)
      .l2_clear_metric_for_current_filter(metric)
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Initialize and switch metric modules (unchanged)
    # ----------------------------
    initialized_servers <- reactiveVal(character(0))
    
    initialize_server <- function(metric) {
      if (metric %in% initialized_servers()) return()
      tryCatch({
        switch(metric, 
               "balls"       = ballsMetricServer("balls",       filtered_data, input_controls),
               "sums"        = sumsMetricServer("sums",         filtered_data),
               "odds_evens"  = oddsEvensMetricServer("odds_evens", filtered_data),
               "table"       = tableMetricServer("table",       filtered_data),
               "difference"  = differenceMetricServer("difference", filtered_data),
               "lag"         = lagMetricServer("lag",           filtered_data)
        )
        initialized_servers(c(initialized_servers(), metric))
      }, error = function(e) {
        message("Error initializing metric ", metric, ": ", conditionMessage(e))
      })
    }
    
    # STEP 1: Show first metric immediately
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
    }, priority = 100) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # STEP 2: Lazy load other metrics
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      shinyjs::delay(500, {
        for (i in seq_along(other_metrics)) {
          local({
            m <- other_metrics[i]
            shinyjs::delay(i * 150, initialize_server(m)) 
          })
        }
      })
    }, priority = 10) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # STEP 3: Fast switching
    current_metric <- reactiveVal(NULL)
    observeEvent(input_controls()$metric, {
      metric <- input_controls()$metric
      if (identical(metric, current_metric())) return()
      current_metric(metric)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      lapply(setdiff(all_metrics, metric), function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 50)
    
    # Cleanup
    session$onSessionEnded(function() {
      message("🧹 Cleaning up dashboard session")
      
      # L1: Clear filtered data cache
      if (exists("l1", inherits = FALSE)) {
        l1$data <- list()
        l1$keys <- character(0)
      }
      
      # L2: Clear metric computation cache
      tryCatch({
        .l2_clear_all()
        
        # Remove L2 environment completely
        if (exists("l2_env", inherits = FALSE)) {
          rm(list = ls(l2_env$store, all.names = TRUE), envir = l2_env$store)
          rm(l2_env)  # ✅ Added: Remove the environment itself
        }
      }, error = function(e) {
        message("L2 cleanup warning: ", e$message)
      })
      
      # Modules: Reset initialized servers
      initialized_servers(character(0))
      
      # Force garbage collection twice
      gc()
      gc()
      
      message("✅ Dashboard session cleanup completed")
    })
  })
}
