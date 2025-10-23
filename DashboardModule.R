

# At the very top of your app.R or global.R
options(shiny.error = browser)
options(shiny.fullstacktrace = TRUE)
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
                min = 1, max = 49, value = c(1,49), step = 1),
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

# Module Server - MEMORY LEAK FIXED
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # FIX 1: Prevent cascade updates with proper boundary checking
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        # CRITICAL: Use isolate to prevent reactive cascade
        isolate({
          updateSliderInput(session, "range", value = newRange)
        })
      }
    }, ignoreInit = TRUE)
    
    # FIX 2: Proper debouncing (300ms is better for UX than 100ms)
    range_debounced <- debounce(reactive({
      input$range
    }), 300)
    
    # FIX 3: Throttle refresh button (1000ms to prevent abuse)
    refresh_throttled <- throttle(reactive({
      input$refresh
    }), 1000)
    
    # Return reactive list
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
    
    # Container for all metrics
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


# ✅ CORRECTED dashboardServer
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data once at startup (NOT reactive)
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # Cache using reactiveVal
    cache <- reactiveVal(list())
    MAX_CACHE_SIZE <- 50
    
    # Filtering function (NOT reactive itself)
    get_filtered_data <- function(weeks, range_vals) {
      cache_key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      current_cache <- cache()
      
      if (!is.null(current_cache[[cache_key]])) {
        return(current_cache[[cache_key]])
      }
      
      data <- metrics_data
      req(!is.null(data) && nrow(data) > 0)
      
      days <- weeks * draws_per_week
      data <- tail(data, min(days, nrow(data)))
      
      num_from <- as.numeric(range_vals[1])
      num_to <- as.numeric(range_vals[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      req(nrow(data) > 0)
      
      if (length(current_cache) >= MAX_CACHE_SIZE) {
        current_cache <- tail(current_cache, MAX_CACHE_SIZE - 1)
      }
      
      current_cache[[cache_key]] <- data
      cache(current_cache)
      
      return(data)
    }
    
    # ✅ FIXED: Use reactive() instead of eventReactive() to avoid context issues
    # ✅ MINIMAL FIX - Only change this section
    filtered_data <- reactive({
      # Don't use eventReactive - use plain reactive
      # Add req() to ensure input_controls exists
      
      cat("=== filtered_data called ===\n")
      cat("Stack trace:\n")
      print(sys.calls())
      cat("========================\n")
      req(input_controls())
      
      # Access refresh to create dependency
      input_controls()$refresh
      
      weeks <- as.numeric(input_controls()$timeRange)
      range_vals <- input_controls()$range
      
      get_filtered_data(weeks, range_vals)
    })
    
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
    
    # ✅ STEP 1: Initial metric display
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, priority = 100)
    
    # ✅ STEP 2: Lazy-load other metrics - FIXED with observeEvent
    # ✅ FIX 2 - Replace this section (around line 232-245)
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      
      isolate({
        later::later(function() {
          lapply(other_metrics, initialize_server)
        }, delay = 0.5)
      })
      
    }, once = TRUE, ignoreInit = TRUE, priority = 10)
    
    # ✅ STEP 3: Metric switching
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      lapply(setdiff(all_metrics, metric), function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 50)
    
    # Cleanup on session end
    session$onSessionEnded(function() {
      cache(list())
      initialized_servers(character(0))
    })
  })
}