
# ui module
lotteryInputUI <- function(id) {
  ns <- NS(id)
  time_choices <- c(
    "Last 7 Days" = 7,
    "Last 30 Days" = 30,
    "Last 90 Days" = 90
  )
  metric_choices <- c(
    "Balls" = "balls", 
    "Users" = "users",
    "Conversion Rate" = "conversion",
    "Engagement" = "engagement"
  )
  
  tagList(
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), "Live Dashboard"),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", "Real-time analytics")
    ),
    sliderInput(ns("range"), "Ball Range", min = 1, max = 50, value = c(1,50), step = 1),
    selectInput(ns("metric"), "Primary Metric", choices = metric_choices, selected = "balls"),
    selectInput(ns("timeRange"), "Time Range", choices = time_choices, selected = 30),
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
    reactive({
      list(
        range = input$range,
        metric = input$metric,
        timeRange = input$timeRange,
        refresh = input$refresh
      )
    })
    
  
  })
}



# -------------------------
# Module: dashboardModule
# -------------------------
dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "Analytics Dashboard"),
        p(class = "header-subtitle", "Monitor your key performance indicators in real-time")
      ),
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3")),
        uiOutput(ns("metricCard4"))
      ),
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "chart-card",
          div(class = "chart-title", "Trend Analysis"),
          plotlyOutput(ns("trendChart"), height = "350px")
        ),
        div(
          class = "chart-card",
          div(class = "chart-title", "Performance Distribution"),
          plotlyOutput(ns("distributionChart"), height = "350px")
        )
      ),
      div(
        class = "chart-card",
        style = "margin-top: 20px;",
        div(class = "chart-title", "Comprehensive Overview"),
        plotlyOutput(ns("overviewChart"), height = "400px")
      )
    )
  )
}

dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # metrics_data reactive: re-generate on refresh click
    metrics_data <- reactive({
      input_controls()$refresh   # <- add ()
      generate_metrics()
    })
    
    # Reactive version for Shiny
    filtered_data <- reactive({
      data <- metrics_data()
      
      # Get number of weeks to show
      weeks <- as.numeric(input_controls()$timeRange)  # e.g., 4 for last 4 weeks
      days <- weeks * 7
      
      # Get last N weeks of data
      data <- tail(data, days)
      
      # Get number range
      num_from <- as.numeric(input_controls()$range[1])  # e.g., 5
      num_to <- as.numeric(input_controls()$range[2])      # e.g., 45
      
      # Filter by number range - check first and last ball
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      return(data)
    })
    
    
    create_metric_card <- function(title, value, change) {
      change_class <- if (change >= 0) "positive" else "negative"
      change_symbol <- if (change >= 0) "↑" else "↓"
      
      div(
        class = "metric-card",
        div(class = "metric-label", title),
        div(class = "metric-value", value),
        div(
          class = paste("metric-change", change_class),
          span(change_symbol),
          span(paste0(abs(round(change, 1)), "%"))
        )
      )
    }
    
    output$metricCard1 <- renderUI({
      data <- filtered_data()
      selected <- nrow(data)
      total_counts <- nrow(metrics_data())
      change <- (selected/total_counts) * 100
      create_metric_card("Total Occurance",
                         paste0("", format(round(change), big.mark = ",")),
                         change)
    })
    
    
    output$trendChart <- renderPlotly({
      data <- filtered_data()
      plot_ly(data, y = ~ball_1, type = "box", name = "Ball 1") %>%
        add_trace(y = ~ball_2, name = "Ball 2", type = "box") %>%
        add_trace(y = ~ball_3, name = "Ball 3", type = "box") %>%
        add_trace(y = ~ball_4, name = "Ball 4", type = "box") %>%
        add_trace(y = ~ball_5, name = "Ball 5", type = "box") %>%
        add_trace(y = ~ball_6, name = "Ball 6", type = "box") %>%
        layout(
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(title = "Ball", color = 'rgba(255,255,255,0.6)'),
          yaxis = list(title = "Value", color = 'rgba(255,255,255,0.6)')
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    

    
    

  

    

    

    

  })
}
