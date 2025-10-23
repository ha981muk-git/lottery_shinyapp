# UI Module - No changes needed
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  time_choices <- setNames(
    c(7, 30, 60, 90, 120, 150, 180),
    c(t("time_last_7", lang), t("time_last_30", lang),
      t("time_last_60", lang), t("time_last_90", lang),
      t("time_last_120", lang), t("time_last_150", lang),
      t("time_last_180", lang))
  )
  
  metric_choices <- setNames(
    c("balls", "sums", "odds_evens", "table", "difference", "lag"),
    c(t("metric_balls", lang), t("metric_sums", lang),
      t("metric_odds_evens", lang), t("metric_tables", lang),
      t("metric_difference", lang), t("metric_lag", lang))
  )
  
  tagList(
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    sliderInput(ns("range"), t("input_ball_range", lang), 
                min = 1, max = 49, value = c(1,49), step = 1),
    selectInput(ns("metric"), t("input_analysis_type", lang), 
                choices = metric_choices, selected = "balls"),
    selectInput(ns("timeRange"), t("input_time_window", lang), 
                choices = time_choices, selected = 30),
    actionButton(ns("refresh"), t("input_refresh", lang), 
                 class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;")
  )
}

# Module Server - FIXED VERSION
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # Create debounced/throttled reactives ONCE
    range_debounced <- debounce(reactive(input$range), 500)
    refresh_throttled <- throttle(reactive(input$refresh), 300)
    
    # Single observer for range validation
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        updateSliderInput(session, "range", value = newRange)
      }
    }, ignoreInit = TRUE)
    
    # Return reactive values
    return(reactive({
      list(
        range = range_debounced(),
        metric = input$metric,
        timeRange = input$timeRange,
        refresh = refresh_throttled()
      )
    }))
  })
}

# Dashboard UI - No changes needed
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(id = ns("skeleton-loader"), style = "padding: 20px;",
        div(class = "skeleton-card",
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px; margin-bottom: 20px;"),
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px;")
    ),
    
    div(id = ns("metricsContainer"), style = "display: none;",
        div(id = ns("metric-balls"), style = "display: none;",
            ballsMetricUI(ns("balls"))),
        div(id = ns("metric-sums"), style = "display: none;",
            sumsMetricUI(ns("sums"))),
        div(id = ns("metric-odds_evens"), style = "display: none;",
            oddsEvensMetricUI(ns("odds_evens"))),
        div(id = ns("metric-table"), style = "display: none;",
            tableMetricUI(ns("table"))),
        div(id = ns("metric-difference"), style = "display: none;",
            differenceMetricUI(ns("difference"))),
        div(id = ns("metric-lag"), style = "display: none;",
            lagMetricUI(ns("lag")))
    )
  )
}

# Server Module - FIXED VERSION
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data ONCE at startup
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # True LRU cache using environment
    cache_env <- new.env(parent = emptyenv())
    cache_keys <- character(0)  # Track order for LRU
    MAX_CACHE_SIZE <- 15
    
    get_filtered_data <- function(weeks, range_vals) {
      cache_key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      
      # --- Step 1: Check cache ---
      if (exists(cache_key, envir = cache_env)) {
        # Move key to end to mark it as most recently used
        cache_keys <<- c(setdiff(cache_keys, cache_key), cache_key)
        return(get(cache_key, envir = cache_env))
      }
      
      # --- Step 2: Filter data if not in cache ---
      req(!is.null(metrics_data) && nrow(metrics_data) > 0)
      
      days <- weeks * draws_per_week
      data <- tail(metrics_data, min(days, nrow(metrics_data)))
      
      num_from <- as.numeric(range_vals[1])
      num_to <- as.numeric(range_vals[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      req(nrow(data) > 0)
      
      # --- Step 3: Add filtered data to cache ---
      assign(cache_key, data, envir = cache_env)
      cache_keys <<- c(cache_keys, cache_key)
      
      # --- Step 4: Evict oldest if cache exceeds MAX_CACHE_SIZE ---
      if (length(cache_keys) > MAX_CACHE_SIZE) {
        oldest_key <- cache_keys[1]
        rm(list = oldest_key, envir = cache_env)
        cache_keys <<- cache_keys[-1]
      }
      
      return(data)
    }
    
    
    # Single reactive for filtered data
    filtered_data <- debounce(reactive({
      input_controls()$refresh
      weeks <- as.numeric(input_controls()$timeRange)
      range_vals <- input_controls()$range
      get_filtered_data(weeks, range_vals)
    }), 500)
    
    
    # Track initialized servers
    initialized_servers <- reactiveVal(character(0))
    
    initialize_server <- function(metric) {
      if (metric %in% initialized_servers()) return()
      
      switch(metric,
             "balls" = ballsMetricServer("balls", filtered_data, input_controls),
             "sums" = sumsMetricServer("sums", filtered_data),
             "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
             "table" = tableMetricServer("table", filtered_data),
             "difference" = differenceMetricServer("difference", filtered_data),
             "lag" = lagMetricServer("lag", filtered_data)
      )
      
      initialized_servers(c(initialized_servers(), metric))
    }
    
    # Initialize first metric and show UI
    observeEvent(input_controls()$metric, {
      metric <- input_controls()$metric
      req(metric)
      
      # Hide skeleton, show content
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      # Initialize and show current metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
      # Hide other metrics
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      lapply(setdiff(all_metrics, metric), function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
    }, ignoreNULL = TRUE)
    
    # Lazy load remaining metrics after first render
    observeEvent(input_controls()$metric, {
      metric <- input_controls()$metric
      req(metric)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, metric)
      
      # Delay initialization of other metrics
      shinyjs::delay(1000, {
        lapply(other_metrics, initialize_server)
      })
    }, once = TRUE)
    
    # Cleanup on session end
    # Cleanup on session end
    session$onSessionEnded(function() {
      rm(list = ls(cache_env), envir = cache_env)  # Clear LRU cache environment
      cache_keys <<- character(0)                    # Reset key tracker
      initialized_servers(character(0))             # Clear initialized servers
    })
    
  })
}