# Gets Lottery data to work with
generate_metrics <- function() {
  return(lotto_clean_sorted)
}
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
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;"),
  )
}


# Module Server
inputModuleServer <- function(id) {
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
    
    # Filtered data reactive (shared across all metrics)
    filtered_data <- reactive({
      data <- metrics_data()
      
      weeks <- as.numeric(input_controls()$timeRange)
      days <- weeks * 2
      data <- tail(data, days)
      
      num_from <- as.numeric(input_controls()$range[1])
      num_to <- as.numeric(input_controls()$range[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      return(data)
    })
    
    # ✅ INITIALIZE ALL SERVERS ONCE (not in observe)
    ballsMetricServer("balls", filtered_data, input_controls)
    sumsMetricServer("sums", filtered_data)
    oddsEvensMetricServer("odds", filtered_data)
    tableMetricServer("table", filtered_data)
    differenceMetricServer("difference", filtered_data)
    lagMetricServer("lag",filtered_data)
    
    # Dynamic UI rendering based on selected metric
    output$metricContent <- renderUI({
      metric <- input_controls()$metric
      switch(metric,
             "balls"       = ballsMetricUI(ns("balls")),
             "sums"        = sumsMetricUI(ns("sums")),
             "odds_evens"  = oddsEvensMetricUI(ns("odds")),
             "table"       = tableMetricUI(ns("table")),
             "difference"  = differenceMetricUI(ns("difference")),
             "lag"         = lagMetricUI(ns("lag"))
      )
    })
    
  })
}