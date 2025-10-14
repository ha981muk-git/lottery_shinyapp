

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
    
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        maxVal <- min(minVal + minDistance, 49)
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


# UI Module - Add all metric UIs at once
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Container for all metrics (all pre-rendered, hidden via CSS)
    div(id = ns("metricsContainer"),
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

# Server Module - Toggle visibility instead of rebuilding
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Metrics data reactive
    metrics_data <- reactive({
      req(input_controls())
      input_controls()$refresh
      generate_metrics()
    })
    
    draws_per_week <- 2
    
    # Increase debounce for better performance
    debounced_range <- reactive(input_controls()$range) %>% debounce(500)
    
    # Use eventReactive for more controlled updates
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$timeRange, 
        debounced_range()),
      {
        data <- metrics_data()
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
    
    # Initialize all metric servers once
    ballsMetricServer("balls", filtered_data, input_controls)
    sumsMetricServer("sums", filtered_data)
    oddsEvensMetricServer("odds_evens", filtered_data)
    tableMetricServer("table", filtered_data)
    differenceMetricServer("difference", filtered_data)
    lagMetricServer("lag", filtered_data)
    
    # Toggle visibility based on selected metric (NO UI REBUILD!)
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      # Hide all metrics
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      lapply(all_metrics, function(m) {
        shinyjs::hide(id = paste0("metric-", m))
      })
      
      # Show selected metric
      shinyjs::show(id = paste0("metric-", metric))
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Show initial metric on load
    observe({
      req(input_controls()$metric)
      shinyjs::show(id = paste0("metric-", input_controls()$metric))
    }) %>% 
      bindEvent(input_controls()$metric, once = TRUE)
    
  })
}