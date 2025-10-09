# balls Metrics UI 

# -------------------------
# Module: dashboardModule
# -------------------------

ballsMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "6/49 Statistical Analysis Demo"),
        p(class = "header-subtitle", "Educational demonstration of probability theory and data visualization techniques")
        
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

ballsMetricServer <- function(id, filtered_data, input_controls) {
  moduleServer(id, function(input, output, session) {
    
    
    # Define consistent colors for each ball (hex codes)
    ball_colors <- c(
      "Ball 1" = "#4169E1",  # royal blue
      "Ball 2" = "#DC143C",  # crimson red
      "Ball 3" = "#32CD32",  # lime green
      "Ball 4" = "#FFD700",  # gold/yellow
      "Ball 5" = "#9370DB",  # medium purple
      "Ball 6" = "#00CED1"   # dark cyan
    )
    
    
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
      total_counts <- nrow(generate_metrics())
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
      N_Tickets <- 13983816
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
