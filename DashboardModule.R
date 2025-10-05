
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
    "Tables" = "table"
  )
  
  tagList(
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), "Live Dashboard"),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", "Real-time analytics")
    ),
    sliderInput(ns("range"), "Ball Range", min = 1, max = 49, value = c(1,49), step = 1),
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
      days <- weeks * 2
      
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
    
    
    create_metric_card <- function(title, value, value_symbol) {
      div(
        class = "metric-card",
        div(class = "metric-label", title),
        div(class = "metric-value",paste0(value, value_symbol))
      )
    }
    
    output$metricCard1 <- renderUI({
      data <- filtered_data()
      selected <- nrow(data)
      total_counts <- nrow(metrics_data())
      change <- (selected/total_counts) * 100
      create_metric_card("Total Coverage",
                         paste0("", format(round(change), big.mark = ",")),
                         "%")
    })
    
    output$metricCard2 <- renderUI({
      data <- filtered_data()
      n_tickets <- nrow(data)
      create_metric_card("Total Occurrence",
                         format(round(n_tickets)),"")
    })
    
    
    output$metricCard3 <- renderUI({
      data <- filtered_data()
      N_Tickets <- 13000000
      n_tickets <- nrow(data)
      chance <- n_tickets /N_Tickets * 100
      create_metric_card(
        "Total Chance",
        paste0(format(chance, digits = 2)),
        "%"
      )
    })
    
    output$metricCard4 <- renderUI({
      # Get number range
      num_from <- as.numeric(input_controls()$range[1])  # e.g., 5
      num_to <- as.numeric(input_controls()$range[2])      # e.g., 45
      difference <- num_to -num_from +1 
      create_metric_card("Range Difference",
                         difference,
                         "")
    })
    
    

    
    # Trend Chart - Box Plot
    output$trendChart <- renderPlotly({
      data <- filtered_data()
      p <- plot_ly()
      for(i in 1:6) {
        ball_name <- paste0("Ball ", i)
        p <- add_trace(p, 
                       x = ball_name,
                       y = data[[paste0("ball_", i)]], 
                       name = ball_name, 
                       type = "box",
                       fillcolor = ball_colors[ball_name],
                       marker = list(color = ball_colors[ball_name]),
                       line = list(color = ball_colors[ball_name]))
      }
      p %>%
        layout(
          title = "Box Plot of Balls 1 to 6",
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(title = "Ball", color = 'rgba(255,255,255,0.6)'),
          yaxis = list(title = "Value", color = 'rgba(255,255,255,0.6)'),
          font = list(color = 'rgba(255,255,255,0.6)'),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Distribution Chart - Full Violin Plot
    output$distributionChart <- renderPlotly({
      data <- filtered_data()
      p <- plot_ly()
      for(i in 1:6) {
        ball_name <- paste0("Ball ", i)
        p <- add_trace(p, 
                       x = ball_name,
                       y = data[[paste0("ball_", i)]], 
                       type = 'violin', 
                       name = ball_name,
                       side = 'both',
                       box = list(
                         visible = TRUE,
                         fillcolor = toRGB(ball_colors[ball_name], alpha = 0.3),
                         line = list(color = ball_colors[ball_name], width = 2)
                       ),
                       meanline = list(visible = TRUE),
                       fillcolor = toRGB(ball_colors[ball_name], alpha = 0.6),
                       line = list(color = ball_colors[ball_name]),
                       opacity = 0.6)
      }
      p %>%
        layout(
          title = "Violin Plot of Balls 1 to 6",
          yaxis = list(title = "Value", color = 'rgba(255,255,255,0.6)'),
          xaxis = list(title = "Ball", color = 'rgba(255,255,255,0.6)'),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'rgba(255,255,255,0.6)'),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Overview Chart - Raincloud / Half Violin
    # Overview Chart - Raincloud / Half Violin
    output$overviewChart <- renderPlotly({
      data <- filtered_data()
      p <- plot_ly()
      
      for(i in 1:6){
        ball_name <- paste0("Ball ", i)
        ball_values <- data[[paste0("ball_", i)]]
        color <- ball_colors[ball_name]
        
        # 1️⃣ Half violin (raincloud)
        p <- add_trace(p,
                       x = rep(i, length(ball_values)),
                       y = ball_values,
                       type = 'violin',
                       side = 'positive',
                       width = 0.6,
                       fillcolor = toRGB(color, alpha = 0.5),
                       line = list(color = color),
                       opacity = 0.6,
                       points = FALSE,
                       showlegend = FALSE,
                       name = ball_name)
        
        # 2️⃣ Box plot (centered)
        p <- add_trace(p,
                       x = rep(i, length(ball_values)),
                       y = ball_values,
                       type = 'box',
                       fillcolor = toRGB(color, alpha = 0.8),
                       line = list(color = color, width = 2),
                       boxpoints = FALSE,
                       width = 0.3,
                       name = ball_name,
                       showlegend = FALSE)
        
        # 3️⃣ Jittered points (manual jitter) - positioned to the left
        set.seed(42 + i)  # Different seed per ball
        jitter_amount <- runif(length(ball_values), -0.35, -0.05)  # Left side only
        x_jittered <- rep(i, length(ball_values)) + jitter_amount
        
        p <- add_trace(p,
                       x = x_jittered,
                       y = ball_values,
                       type = 'scatter',
                       mode = 'markers',
                       marker = list(color = color, size = 4, opacity = 0.6),
                       showlegend = FALSE,
                       hoverinfo = 'y',
                       name = ball_name)
      }
      
      p %>%
        layout(
          title = "Raincloud Plot of Balls 1 to 6",
          yaxis = list(title = "Value", color = 'rgba(255,255,255,0.6)'),
          xaxis = list(
            title = "Ball", 
            color = 'rgba(255,255,255,0.6)',
            tickmode = 'array',
            tickvals = 1:6,
            ticktext = paste0("Ball ", 1:6),
            range = c(0.5, 6.5)
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'rgba(255,255,255,0.6)'),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
