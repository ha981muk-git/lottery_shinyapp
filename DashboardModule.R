# Helper for consistent chart cards to reduce code duplication
create_chart_card <- function(ns, title_id, desc_id = NULL, plot_id, height = "400px", style = "") {
  div(
    class = "content-card",
    style = style,
    uiOutput(ns(title_id)),
    if (!is.null(desc_id)) p(class = "info-text", uiOutput(ns(desc_id))),
    plotlyOutput(ns(plot_id), height = height)
  )
}

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

# Module Server - FULLY OPTIMIZED
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
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
    
    # Throttle refresh button
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(300)
    
    # Return debounced and throttled values
    return(reactive({
      list(
        range = range_debounced(),       # ✅ DEBOUNCED - only updates every 300ms
        metric = input$metric,           # Direct - instant response
        timeRange = input$timeRange,     # Direct - instant response
        refresh = refresh_throttled()    # ✅ THROTTLED - only every 300ms
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

# Server Module - FULLY OPTIMIZED WITH CACHING
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load data once at startup
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # ✅ FIX 3: Caching environment for filtered data to avoid recomputation
    cache_env <- new.env()
    
    # Memoized filtering function - returns cached results if same parameters
    get_filtered_data <- function(weeks, range_vals) {
      # Create unique cache key from parameters
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
      
      # Cache the result for future use
      assign(cache_key, data, envir = cache_env)
      data
    }
    
    # ✅ FIX 4: Use debounced range for filtering - reduces computation
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$timeRange, 
        input_controls()$range),  # Now uses debounced range (300ms delay)
      {
        weeks <- as.numeric(input_controls()$timeRange)
        range_vals <- input_controls()$range
        get_filtered_data(weeks, range_vals)
      },
      ignoreNULL = TRUE
    )
    
    # Track which servers are initialized
    initialized_servers <- reactiveVal(list())
    
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
    
    # ✅ STEP 2: Preload other metrics after 500ms (low priority = 10, non-blocking)
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      
      # Delay preloading to not block initial render
      shinyjs::delay(500, {
        lapply(other_metrics, function(m) {
          initialize_server(m)
        })
      })
      
    }, priority = 10) %>% bindEvent(input_controls()$metric, once = TRUE)
    
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