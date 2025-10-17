# ui module
lotteryInputUI <- function(id) {
  ns <- NS(id)
  time_choices <- c(
    "Last  7 Weeks" = 7,
    "Last 30 Weeks" = 30,
    "Last 60 Weeks" = 60,
    "Last 90 Weeks" = 90,
    "Last 120 Weeks" = 120,
    "Last 150 Weeks" = 150,
    "Last 180 Weeks" = 180
  )
  metric_choices <- c(
    "Balls" = "balls", 
    "Sums" = "sums",
    "Odds Evens" = "odds_evens",
    "Tables" = "table",
    "Difference" = "difference",
    "Lag" = "lag"
  )
  
  tagList(
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), "Live Dashboard"),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", "Real-time analytics")
    ),
    sliderInput(ns("range"), "Ball Range", min = 1, max = 49, value = c(1,49), step = 1),
    selectInput(ns("metric"), "Analysis Type", choices = metric_choices, selected = "balls"),
    selectInput(ns("timeRange"), "Time Window", choices = time_choices, selected = 30),
    actionButton(ns("refresh"), "Refresh Data", class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;")
  )
}


# Module Server
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # Debounce slider updates
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        maxVal <- min(minVal + minDistance, 49)
        minVal <- maxVal - minDistance
        updateSliderInput(session, "range", value = c(minVal, maxVal))
      }
    })
    
    # Throttle refresh button to prevent spam
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(500)  # Max once per 500ms
    
    return(reactive({
      list(
        range = input$range,
        metric = input$metric,
        timeRange = input$timeRange,
        refresh = refresh_throttled()  # Use throttled version
      )
    }))
  })
}


# UI Module - Add all metric UIs at once with skeleton loader
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Skeleton for initial load
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card",
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.5s infinite; border-radius: 12px; margin-bottom: 20px;"),
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.5s infinite; border-radius: 12px;")
    ),
    
    # Container for all metrics (all pre-rendered, hidden via CSS)
    div(id = ns("metricsContainer"),
        style = "display: none;",  # Hidden until first metric loads
        
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

# Server Module - Optimized with background loading
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cache the data generation
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # Increase debounce for better performance
    debounced_range <- reactive(input_controls()$range) %>% debounce(200)
    
    # Use eventReactive for more controlled updates
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
    
    # ============ OPTIMIZED BACKGROUND INITIALIZATION ============
    
    initialized_servers <- reactiveVal(list())
    
    initialize_server <- function(metric, show_message = TRUE) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return()
      
      start_time <- Sys.time()
      
      if (show_message) {
        cat("⏳ Loading:", metric, "...\n")
      }
      
      switch(metric,
             "balls" = ballsMetricServer("balls", filtered_data, input_controls),
             "sums" = sumsMetricServer("sums", filtered_data),
             "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
             "table" = tableMetricServer("table", filtered_data),
             "difference" = differenceMetricServer("difference", filtered_data),
             "lag" = lagMetricServer("lag", filtered_data)
      )
      
      elapsed <- round(as.numeric(Sys.time() - start_time, units = "secs"), 3)
      
      initialized_servers(c(already_init, metric))
      cat(sprintf("✓ %s loaded in %s seconds\n", metric, elapsed))
    }
    
    # 1. Initialize first metric + show container
    observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Hide skeleton, show metrics container
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      # Load first metric
      initialize_server(metric, show_message = TRUE)
      shinyjs::show(id = paste0("metric-", metric))
      
    }) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # 2. Background loading (silent - no notifications)
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      invalidateLater(1000)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      
      cat("🔄 Background loading remaining metrics...\n")
      
      # Load other metrics silently
      for (m in other_metrics) {
        initialize_server(m, show_message = FALSE)
        Sys.sleep(0.08)  # Slightly faster than 0.1s
      }
      
      cat("✅ All metrics ready!\n")
      
    }) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # ============ END BACKGROUND INITIALIZATION ============
    
    # 3. Fast metric switching
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Batch hide for better performance
      lapply(all_metrics, function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      
      # Safety net: initialize if not loaded
      initialize_server(metric, show_message = FALSE)
      
      # Show selected
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
  })
}