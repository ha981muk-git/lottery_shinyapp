# ==============================================================================
# OPTIMIZED LOTTERY DASHBOARD MODULE
# Key Improvements:
# 1. Fixed reactive leak from returning reactive() wrapper
# 2. Proper cache invalidation with size limits
# 3. Removed redundant observe() calls
# 4. Better debounce timing
# 5. Lazy-loaded modules with proper cleanup
# ==============================================================================

# UI Module
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
                min = 1, max = 49, value = c(1, 49), step = 1),
    selectInput(ns("metric"), 
                t("input_analysis_type", lang), 
                choices = metric_choices, 
                selected = "balls"),
    selectInput(ns("timeRange"), 
                t("input_time_window", lang), 
                choices = time_choices, 
                selected = 30),
    actionButton(ns("refresh"), 
                 t("input_refresh", lang), 
                 class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;")
  )
}

# Module Server - CRITICAL FIX: Remove reactive() wrapper
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # Prevent cascade updates - only update if actually out of bounds
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        updateSliderInput(session, "range", value = newRange)
      }
    }, ignoreInit = TRUE)
    
    # CRITICAL: Increased debounce to 500ms - range sliders generate MANY events
    range_debounced <- debounce(reactive(input$range), 500)
    
    # Throttle refresh button to prevent spam
    refresh_throttled <- throttle(reactive(input$refresh), 1000)
    
    # ⚠️ CRITICAL FIX: Return a LIST, not reactive({list(...)})
    # The reactive() wrapper creates memory leaks and duplicate evaluations
    return(list(
      range = range_debounced,           # Already reactive
      metric = reactive(input$metric),   # Wrap raw input
      timeRange = reactive(input$timeRange),
      refresh = refresh_throttled        # Already reactive
    ))
  })
}

# Dashboard UI
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
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

# Server Module - FULLY OPTIMIZED WITH PROPER CACHING
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data once at startup
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # ✅ IMPROVED: Cache with size limit and LRU eviction
    cache_env <- new.env()
    cache_keys <- character(0)  # Track insertion order
    MAX_CACHE_SIZE <- 20  # Prevent unbounded memory growth
    
    # Memoized filtering function with proper cache management
    get_filtered_data <- function(weeks, range_vals) {
      cache_key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      
      # Return cached result if it exists
      if (exists(cache_key, envir = cache_env)) {
        return(get(cache_key, envir = cache_env))
      }
      
      # Perform filtering
      data <- metrics_data
      req(!is.null(data) && nrow(data) > 0)
      
      days <- weeks * draws_per_week
      data <- tail(data, min(days, nrow(data)))
      
      num_from <- as.numeric(range_vals[1])
      num_to <- as.numeric(range_vals[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      req(nrow(data) > 0)
      
      # ✅ CRITICAL: Implement cache eviction (LRU)
      if (length(cache_keys) >= MAX_CACHE_SIZE) {
        oldest_key <- cache_keys[1]
        rm(list = oldest_key, envir = cache_env)
        cache_keys <<- cache_keys[-1]
      }
      
      # Cache the result
      assign(cache_key, data, envir = cache_env)
      cache_keys <<- c(cache_keys, cache_key)
      
      data
    }
    
    # ✅ FIXED: Access reactive values properly with ()
    filtered_data <- eventReactive(
      c(input_controls$refresh(), 
        input_controls$timeRange(), 
        input_controls$range()),
      {
        weeks <- as.numeric(input_controls$timeRange())
        range_vals <- input_controls$range()
        get_filtered_data(weeks, range_vals)
      },
      ignoreNULL = TRUE
    )
    
    # Track which servers are initialized
    initialized_servers <- reactiveVal(character(0))
    
    initialize_server <- function(metric) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return()
      
      switch(metric,
             "balls" = ballsMetricServer("balls", filtered_data, input_controls),
             "sums" = sumsMetricServer("sums", filtered_data),
             "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
             "table" = tableMetricServer("table", filtered_data),
             "difference" = differenceMetricServer("difference", filtered_data),
             "lag" = lagMetricServer("lag", filtered_data)
      )
      
      initialized_servers(c(already_init, metric))
    }
    
    # ✅ SIMPLIFIED: Single observe() for initial load
    observeEvent(input_controls$metric(), {
      metric <- input_controls$metric()
      req(metric)
      
      # Hide skeleton, show container
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      # Initialize and show current metric
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, once = TRUE, priority = 100)
    
    # ✅ BACKGROUND PRELOADING: Delayed, non-blocking
    observeEvent(input_controls$metric(), {
      metric <- input_controls$metric()
      req(metric)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, metric)
      
      # Preload other metrics after 1 second (UI is responsive first)
      later::later(function() {
        lapply(other_metrics, function(m) {
          if (!m %in% initialized_servers()) {
            initialize_server(m)
          }
        })
      }, delay = 1)
      
    }, once = TRUE, priority = 10)
    
    # ✅ FAST METRIC SWITCHING
    observeEvent(input_controls$metric(), {
      metric <- input_controls$metric()
      req(metric)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Hide all other metrics
      lapply(setdiff(all_metrics, metric), function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      
      # Initialize if needed and show
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreInit = TRUE, priority = 50)
    
    # ✅ CLEANUP: Clear cache when session ends
    onSessionEnded(function() {
      rm(list = ls(envir = cache_env), envir = cache_env)
    })
  })
}

# ==============================================================================
# ADDITIONAL OPTIMIZATION TIPS FOR YOUR ENTIRE APP
# ==============================================================================
# 
# 1. DATA LOADING:
#    - If generate_metrics() loads from file/DB, do it ONCE globally, not per session
#    - Consider: metrics_data <- readRDS("data/metrics.rds") at top of app.R
#
# 2. PLOTTING:
#    - Use plotly for interactive plots (faster than ggplotly())
#    - For static plots, use renderCachedPlot() instead of renderPlot()
#    - Example: output$plot <- renderCachedPlot({ ... }, cacheKeyExpr = {
#                 list(input_controls$timeRange(), input_controls$range())
#              })
#
# 3. TABLES:
#    - Use DT::renderDT() with server-side processing for large tables
#    - DT::datatable(data, server = TRUE, options = list(pageLength = 25))
#
# 4. MEMORY MONITORING:
#    - Add to your server function:
#      observe({
#        invalidateLater(10000)  # Every 10 seconds
#        cat("Memory usage:", pryr::mem_used(), "\n")
#      })
#
# 5. PROFILING:
#    - Use profvis to identify bottlenecks:
#      profvis::profvis({ runApp() })
#
# ==============================================================================