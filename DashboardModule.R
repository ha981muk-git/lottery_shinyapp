# ============================================================
# ULTRA-SMOOTH UI MODULE - OPTIMIZED FOR BUTTER-LIKE PERFORMANCE
# ============================================================

# UI Module - Enhanced with better debouncing and visual feedback
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  time_choices <- setNames(
    c(7, 30, 60, 90, 120, 150, 180),
    c(t("time_last_7", lang), t("time_last_30", lang), t("time_last_60", lang),
      t("time_last_90", lang), t("time_last_120", lang), t("time_last_150", lang),
      t("time_last_180", lang))
  )
  
  metric_choices <- setNames(
    c("balls", "sums", "odds_evens", "table", "difference", "lag"),
    c(t("metric_balls", lang), t("metric_sums", lang), t("metric_odds_evens", lang),
      t("metric_tables", lang), t("metric_difference", lang), t("metric_lag", lang))
  )
  
  tagList(
    # Add CSS for smooth transitions
    tags$style(HTML("
      .smooth-transition { transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); }
      .slider-updating { opacity: 0.7; pointer-events: none; }
      .btn-primary:active { transform: scale(0.98); }
      @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
      .updating { animation: pulse 1.5s ease-in-out infinite; }
    ")),
    
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    
    # Slider with loading indicator
    div(id = ns("sliderContainer"), class = "smooth-transition",
        sliderInput(ns("range"), 
                    t("input_ball_range", lang), 
                    min = 1, max = 49, value = c(1, 49), step = 1)
    ),
    
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
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600; transition: all 0.2s;")
  )
}


# Module Server - OPTIMIZED with intelligent caching
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    minDistance <- 6
    
    # Slider validation with reduced updates
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        maxVal <- min(minVal + minDistance, 49)
        minVal <- maxVal - minDistance
        updateSliderInput(session, "range", value = c(minVal, maxVal))
      }
    }, ignoreInit = TRUE)
    
    # Ultra-smooth refresh with rate limiting
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(800)  # Increased for smoother experience
    
    # Debounced outputs for smoother updates
    range_debounced <- reactive(input$range) %>% debounce(300)
    metric_debounced <- reactive(input$metric) %>% debounce(100)
    timeRange_debounced <- reactive(input$timeRange) %>% debounce(200)
    
    return(reactive({
      list(
        range = range_debounced(),
        metric = metric_debounced(),
        timeRange = timeRange_debounced(),
        refresh = refresh_throttled()
      )
    }))
  })
}


# Dashboard UI - Optimized skeleton and transitions
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Enhanced CSS for smooth animations
    tags$style(HTML("
      @keyframes shimmer {
        0% { background-position: -200% 0; }
        100% { background-position: 200% 0; }
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .skeleton-card {
        height: 200px;
        background: linear-gradient(90deg, 
          rgba(139,92,246,0.08) 25%, 
          rgba(139,92,246,0.15) 50%, 
          rgba(139,92,246,0.08) 75%);
        background-size: 200% 100%;
        animation: shimmer 2s ease-in-out infinite;
        border-radius: 12px;
        margin-bottom: 20px;
      }
      .metric-container {
        animation: fadeIn 0.4s ease-out;
      }
      #", ns("metricsContainer"), " > div {
        transition: opacity 0.3s ease, transform 0.3s ease;
      }
    ")),
    
    # Improved skeleton loader
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card", style = "height: 180px;"),
        div(class = "skeleton-card", style = "height: 320px; animation-delay: 0.1s;"),
        div(class = "skeleton-card", style = "height: 220px; animation-delay: 0.2s;")
    ),
    
    # Metrics container with smooth transitions
    div(id = ns("metricsContainer"),
        style = "display: none;",
        
        div(id = ns("metric-balls"), class = "metric-container",
            style = "display: none;", ballsMetricUI(ns("balls"))),
        
        div(id = ns("metric-sums"), class = "metric-container",
            style = "display: none;", sumsMetricUI(ns("sums"))),
        
        div(id = ns("metric-odds_evens"), class = "metric-container",
            style = "display: none;", oddsEvensMetricUI(ns("odds_evens"))),
        
        div(id = ns("metric-table"), class = "metric-container",
            style = "display: none;", tableMetricUI(ns("table"))),
        
        div(id = ns("metric-difference"), class = "metric-container",
            style = "display: none;", differenceMetricUI(ns("difference"))),
        
        div(id = ns("metric-lag"), class = "metric-container",
            style = "display: none;", lagMetricUI(ns("lag")))
    )
  )
}


# Dashboard Server - ULTRA-OPTIMIZED with lazy loading
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cached data with memoization
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # Increased debounce for butter-smooth updates
    debounced_range <- reactive(input_controls()$range) %>% debounce(350)
    
    # Smart filtered data with caching
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$timeRange, 
        debounced_range()),
      {
        data <- metrics_data
        req(!is.null(data) && nrow(data) > 0)
        
        weeks <- as.numeric(input_controls()$timeRange)
        days <- weeks * draws_per_week
        data <- tail(data, min(days, nrow(data)))
        
        range_vals <- debounced_range()
        num_from <- as.numeric(range_vals[1])
        num_to <- as.numeric(range_vals[2])
        
        data <- data %>%
          filter(ball_1 >= num_from & ball_6 <= num_to)
        
        req(nrow(data) > 0)
        data
      },
      ignoreNULL = TRUE
    )
    
    # Track initialized servers
    initialized_servers <- reactiveVal(list())
    loading_queue <- reactiveVal(character(0))
    
    # Optimized initialization
    initialize_server <- function(metric, priority = FALSE) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return(TRUE)
      
      tryCatch({
        switch(metric,
               "balls" = ballsMetricServer("balls", filtered_data, input_controls),
               "sums" = sumsMetricServer("sums", filtered_data),
               "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
               "table" = tableMetricServer("table", filtered_data),
               "difference" = differenceMetricServer("difference", filtered_data),
               "lag" = lagMetricServer("lag", filtered_data)
        )
        
        initialized_servers(c(already_init, metric))
        return(TRUE)
      }, error = function(e) {
        message("Error initializing ", metric, ": ", e$message)
        return(FALSE)
      })
    }
    
    # STEP 1: Fast first render
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Smooth transition: hide skeleton, show container
      shinyjs::delay(100, {
        shinyjs::hide("skeleton-loader", anim = TRUE, animType = "fade")
        shinyjs::show("metricsContainer", anim = TRUE, animType = "fade")
      })
      
      # Load and show first metric
      if (initialize_server(metric, priority = TRUE)) {
        shinyjs::show(id = paste0("metric-", metric), anim = TRUE, animType = "fade")
      }
      
    }) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # STEP 2: Background lazy loading with priority queue
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      # Wait before starting background load
      invalidateLater(1500)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      remaining <- setdiff(all_metrics, first_metric)
      
      # Prioritize frequently used metrics
      priority_order <- c("sums", "table", "odds_evens", "difference", "lag", "balls")
      remaining <- intersect(priority_order, remaining)
      
      # Load metrics with breathing room
      for (m in remaining) {
        initialize_server(m)
        Sys.sleep(0.12)  # Smooth spacing between loads
      }
      
    }) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # STEP 3: Instant metric switching with fade transitions
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      new_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Smooth fade out all metrics
      for (m in all_metrics) {
        if (m != new_metric) {
          shinyjs::hide(id = paste0("metric-", m), anim = TRUE, animType = "fade", time = 0.2)
        }
      }
      
      # Initialize if needed (instant for cached)
      initialize_server(new_metric, priority = TRUE)
      
      # Smooth fade in selected metric
      shinyjs::delay(200, {
        shinyjs::show(id = paste0("metric-", new_metric), anim = TRUE, animType = "fade", time = 0.3)
      })
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
  })
}