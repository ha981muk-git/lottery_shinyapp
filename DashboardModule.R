

# ui module
lotteryInputUI <- function(id) {
  ns <- NS(id)
  time_choices <- c(
    "Last  7 Weeks" = 7,
    "Last 30 Weeks" = 30,
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
    
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        maxVal <- min(minVal + minDistance, 50)
        minVal <- maxVal - minDistance
        updateSliderInput(session, "range", value = c(minVal, maxVal))
      }
    })
    return(reactive({
      list(
        range = input$range,
        metric = input$metric,
        timeRange = input$timeRange,
        refresh = input$refresh
      )
    }))
    
  
  })
}


# IMPROVED VERSION - Initialize all servers once
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure input_controls() is available before anything else
    
    
    
    # Metrics data reactive
    metrics_data <- reactive({
      req(input_controls())
      input_controls()$refresh
      generate_metrics()
    })
    
    draws_per_week <- 2
    # wait for 500ms
    debounced_range <- reactive(input_controls()$range) %>% debounce(300)
    
    
    # observe({
    #   cat("Debounced range:", debounced_range(), "\n")
    # })
    
    # Filtered data reactive (shared across all metrics)
    filtered_data <- reactive({
      data <- metrics_data()
      # ensure data is valid and non-empty
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
      
    })
    
    # ✅ INITIALIZE ALL SERVERS ONCE (not in observe)
    ballsMetricServer("balls", filtered_data, input_controls)
    
    metric_list <- list(
      sums = sumsMetricServer,
      odds_evens = oddsEvensMetricServer,
      table = tableMetricServer,
      difference = differenceMetricServer,
      lag = lagMetricServer
    )
    
    lapply(names(metric_list), function(name) {
      metric_list[[name]](name, filtered_data)
    })
    
    
    # Dynamic UI rendering based on selected metric
    # Match output ID in dashboardServer
    # In dashboardServer, you currently have:
    # output$metricContent <- renderUI({ ... })
    # But in the UI, the ID is now "dashboard1-metricContent".
    # These must match exactly.
    # This ensures the top-level uiOutput() and server renderUI() match.
    
    
    ui_list <- list(
      balls = ballsMetricUI,
      sums = sumsMetricUI,
      odds_evens = oddsEvensMetricUI,
      table = tableMetricUI,
      difference = differenceMetricUI,
      lag = lagMetricUI
    )
    
    output$metricContent <- renderUI({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      if (!metric %in% names(ui_list)) {
        return(div("Invalid metric selected"))
      }
      
      ui_list[[metric]](ns(metric))
    })
    
    
  })
}