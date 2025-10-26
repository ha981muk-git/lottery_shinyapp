# ============================================================================
# DashboardModule.R - COMPLETE FIXED VERSION
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
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;")
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
    
    # ✅ Debounce range slider - waits 300ms after user stops dragging
    range_debounced <- reactive({
      input$range
    }) %>% debounce(300)
    
    # Throttle refresh button to prevent spam clicks
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(300)
    
    # Return reactive list with debounced/throttled values
    return(reactive({
      list(
        range = range_debounced(),       # Debounced - updates after 300ms delay
        metric = input$metric,           # Direct - instant response
        timeRange = input$timeRange,     # Direct - instant response
        refresh = refresh_throttled()    # Throttled - max once per 300ms
      )
    }))
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
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px; margin-bottom: 20px;"),
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px;")
    ),
    
    # Container for all metrics (all pre-rendered, hidden via CSS)
    div(id = ns("metricsContainer"),
        style = "display: none;",
        
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

# ----------------------------------------------------------------------------
# DASHBOARD SERVER - FULLY OPTIMIZED WITH LRU CACHE
# ----------------------------------------------------------------------------
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ----------------------------
    # Limit requests per session
    # ----------------------------
    observe({
      req_count <- session$clientData$shinysession$requests
      
      if (!is.null(req_count)) {
        MAX_REQUESTS <- 30
        if (req_count > MAX_REQUESTS) {
          session$close()
        }
      }
    })
    
    
    # Load data once at startup
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # ✅ FIX 1: Use reactiveValues for proper scoping (prevents memory leaks)
    cache <- reactiveValues(
      data = list(),      # Store cached filtered data
      keys = character(0) # Track access order for LRU eviction
    )
    MAX_CACHE_SIZE <- 15
    
    # ✅ FIX 2: Memoized filtering function with LRU cache
    get_filtered_data <- function(weeks, range_vals) {
      # Validate inputs first
      if (is.null(weeks) || is.null(range_vals) || 
          length(range_vals) != 2 || 
          is.null(metrics_data) || nrow(metrics_data) == 0) {
        return(NULL)
      }
      
      # Create unique cache key from parameters
      cache_key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      
      # Check if result is cached
      if (!is.null(cache$data[[cache_key]])) {
        # Move key to end (mark as most recently used)
        cache$keys <- c(setdiff(cache$keys, cache_key), cache_key)
        return(cache$data[[cache_key]])
      }
      
      # Perform filtering (not in cache)
      data <- metrics_data
      days <- weeks * draws_per_week
      data <- tail(data, min(days, nrow(data)))
      
      num_from <- as.numeric(range_vals[1])
      num_to <- as.numeric(range_vals[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      if (nrow(data) == 0) return(NULL)
      
      # Store result in cache
      cache$data[[cache_key]] <- data
      cache$keys <- c(cache$keys, cache_key)
      
      # Evict oldest entry if cache exceeds limit (LRU policy)
      if (length(cache$keys) > MAX_CACHE_SIZE) {
        oldest_key <- cache$keys[1]
        cache$data[[oldest_key]] <- NULL
        cache$keys <- cache$keys[-1]
      }
      
      return(data)
    }
    
    # ✅ FIX 3: Use isolate() to prevent cascading updates
    # Only refresh button triggers recalculation, other inputs are isolated
    filtered_data <- reactive({
      # React to refresh button
      input_controls()$refresh
      
      # Isolate other inputs to prevent cascade
      weeks <- isolate(as.numeric(input_controls()$timeRange))
      range_vals <- isolate(input_controls()$range)
      
      get_filtered_data(weeks, range_vals)
    }) %>% debounce(300)  # Single debounce at the end
    
    # Track which metric servers are initialized
    initialized_servers <- reactiveVal(character(0))
    
    # ✅ FIX 4: Initialize server modules with error handling
    initialize_server <- function(metric) {
      if (metric %in% initialized_servers()) return()
      
      tryCatch({
        switch(metric,
               "balls" = ballsMetricServer("balls", filtered_data, input_controls),
               "sums" = sumsMetricServer("sums", filtered_data),
               "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
               "table" = tableMetricServer("table", filtered_data),
               "difference" = differenceMetricServer("difference", filtered_data),
               "lag" = lagMetricServer("lag", filtered_data)
        )
        initialized_servers(c(initialized_servers(), metric))
      }, error = function(e) {
        message("Error initializing metric ", metric, ": ", e$message)
      })
    }
    
    # ✅ STEP 1: Show first metric immediately (high priority)
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Hide skeleton loader, show container
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      # Initialize and show current metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, priority = 100) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # ✅ STEP 2: Lazy load other metrics after initial render (low priority)
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      
      # Delay preloading to not block initial render
      # Stagger initialization to reduce CPU spike
      shinyjs::delay(500, {
        for (i in seq_along(other_metrics)) {
          local({
            m <- other_metrics[i]
            shinyjs::delay(i * 150, initialize_server(m))
          })
        }
      })
      
    }, priority = 10) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # ✅ STEP 3: Fast metric switching (medium priority)
    current_metric <- reactiveVal(NULL)
    
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Only update if metric actually changed
      if (identical(metric, current_metric())) return()
      current_metric(metric)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Hide all other metrics
      lapply(setdiff(all_metrics, metric), function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      
      # Initialize and show selected metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 50)
    
    # ✅ FIX 5: Proper cleanup on session end
    session$onSessionEnded(function() {
      cache$data <- list()
      cache$keys <- character(0)
      initialized_servers(character(0))
      gc()           # memory actually released
    })
    
  })
}