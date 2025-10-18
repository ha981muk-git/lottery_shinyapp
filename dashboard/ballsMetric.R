# balls Metrics UI with Translation Support

# -------------------------
# Module: ballsMetricModule
# -------------------------

ballsMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        # Header will be rendered dynamically in server
        uiOutput(ns("header"))
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
          uiOutput(ns("trendChartTitle")),
          plotlyOutput(ns("trendChart"), height = "350px")
        ),
        div(
          class = "chart-card",
          uiOutput(ns("distributionChartTitle")),
          plotlyOutput(ns("distributionChart"), height = "350px")
        )
      ),
      div(
        class = "chart-card",
        style = "margin-top: 20px;",
        uiOutput(ns("densityChartTitle")),
        plotlyOutput(ns("densityChart"), height = "400px")
      ),
      div(
        class = "chart-card",
        style = "margin-top: 20px;",
        uiOutput(ns("overviewChartTitle")),
        plotlyOutput(ns("overviewChart"), height = "400px")
      )
    )
  )
}

ballsMetricServer <- function(id, filtered_data, input_controls) {
  moduleServer(id, function(input, output, session) {
    
    # Get current language from URL
    get_lang <- reactive({
      query <- parseQueryString(isolate(session$clientData$url_search))
      query$lang %||% "de"
    })
    
    # Define consistent colors for each ball (hex codes)
    ball_colors <- c(
      "Ball 1" = "#4169E1",  # royal blue
      "Ball 2" = "#DC143C",  # crimson red
      "Ball 3" = "#32CD32",  # lime green
      "Ball 4" = "#FFD700",  # gold/yellow
      "Ball 5" = "#9370DB",  # medium purple
      "Ball 6" = "#00CED1"   # dark cyan
    )
    
    # Render header
    output$header <- renderUI({
      lang <- get_lang()
      tagList(
        h1(class = "header-title", t("balls_title", lang)),
        p(class = "header-subtitle", t("balls_subtitle", lang))
      )
    })
    
    # Chart titles
    output$trendChartTitle <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", t("balls_trend_title", lang))
    })
    
    output$distributionChartTitle <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", t("balls_distribution_title", lang))
    })
    
    output$densityChartTitle <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", t("balls_chart_density", lang))
    })
    
    output$overviewChartTitle <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", t("balls_overview_title", lang))
    })
    
    create_metric_card <- function(title, value, value_symbol) {
      div(
        class = "metric-card",
        div(class = "metric-label", title),
        div(class = "metric-value", paste0(value, value_symbol))
      )
    }
    
    output$metricCard1 <- renderUI({
      lang <- get_lang()
      data <- filtered_data()
      selected <- nrow(data)
      total_counts <- nrow(generate_metrics())
      change <- (selected/total_counts) * 100
      create_metric_card(
        t("balls_metric_coverage", lang),
        paste0("", format(round(change), big.mark = ",")),
        "%"
      )
    })
    
    output$metricCard2 <- renderUI({
      lang <- get_lang()
      data <- filtered_data()
      n_tickets <- nrow(data)
      create_metric_card(
        t("balls_metric_occurrence", lang),
        format(round(n_tickets)),
        ""
      )
    })
    
    output$metricCard3 <- renderUI({
      lang <- get_lang()
      data <- filtered_data()
      N_Tickets <- 13983816
      n_tickets <- nrow(data)
      chance <- n_tickets / N_Tickets * 100
      create_metric_card(
        t("balls_metric_chance", lang),
        paste0(format(chance, digits = 2)),
        "%"
      )
    })
    
    output$metricCard4 <- renderUI({
      lang <- get_lang()
      num_from <- as.numeric(input_controls()$range[1])
      num_to <- as.numeric(input_controls()$range[2])
      difference <- num_to - num_from + 1
      create_metric_card(
        t("balls_metric_range", lang),
        difference,
        ""
      )
    })
    
    # Trend Chart - Box Plot
    output$trendChart <- renderPlotly({
      lang <- get_lang()
      data <- filtered_data()
      p <- plot_ly()
      
      for(i in 1:6) {
        ball_name <- paste0("Ball ", i)
        ball_label <- t(paste0("ball_", i), lang)
        p <- add_trace(p, 
                       x = ball_label,
                       y = data[[paste0("ball_", i)]], 
                       name = ball_label, 
                       type = "box",
                       fillcolor = ball_colors[ball_name],
                       marker = list(color = ball_colors[ball_name]),
                       line = list(color = ball_colors[ball_name]))
      }
      
      p %>%
        layout(
          title = t("balls_boxplot_title", lang),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(title = t("ball_label", lang), color = 'rgba(255,255,255,0.6)'),
          yaxis = list(title = t("value_label", lang), color = 'rgba(255,255,255,0.6)'),
          font = list(color = 'rgba(255,255,255,0.6)'),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Distribution Chart - Full Violin Plot
    output$distributionChart <- renderPlotly({
      lang <- get_lang()
      data <- filtered_data()
      p <- plot_ly()
      
      for(i in 1:6) {
        ball_name <- paste0("Ball ", i)
        ball_label <- t(paste0("ball_", i), lang)
        p <- add_trace(p, 
                       x = ball_label,
                       y = data[[paste0("ball_", i)]], 
                       type = 'violin', 
                       name = ball_label,
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
          title = t("balls_violin_title", lang),
          yaxis = list(title = t("value_label", lang), color = 'rgba(255,255,255,0.6)'),
          xaxis = list(title = t("ball_label", lang), color = 'rgba(255,255,255,0.6)'),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'rgba(255,255,255,0.6)'),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    output$densityChart <- renderPlotly({
      # The key is lang <- get_lang() must be inside the renderPlotly, not outside.
      # Otherwise, lang is undefined when t() is called.
      lang <- get_lang()  # <-- add this
      data <- filtered_data()
      
      # Convert data to long format
      df_long <- data %>%
        tidyr::pivot_longer(
          cols = starts_with("ball_"),
          names_to = "ball",
          values_to = "value"
        ) %>%
        dplyr::mutate(ball = stringr::str_replace(ball, "ball_", "Ball "))
      
      p <- plot_ly()
      
      
      # Smooth overlay (optional)
      for (ball_name in unique(df_long$ball)) {
        ball_values <- df_long$value[df_long$ball == ball_name]
        density_data <- density(ball_values)
        p <- add_trace(
          p,
          x = density_data$x,
          y = density_data$y,
          type = "scatter",
          mode = "lines",
          name = paste(ball_name, "(Smooth)"),
          line = list(color = ball_colors[ball_name], width = 2)
        )
      }
      
      p %>%
        layout(
          title = t("balls_chart_density_title", lang),
          xaxis = list(title = t("value_label", lang), color = 'rgba(255,255,255,0.6)'),
          yaxis = list(title = t("ball_label", lang), color = 'rgba(255,255,255,0.6)'),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'rgba(255,255,255,0.6)'),
          barmode = "overlay",
          legend = list(orientation = 'h', y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Overview Chart - Raincloud / Half Violin
    output$overviewChart <- renderPlotly({
      lang <- get_lang()
      data <- filtered_data()
      p <- plot_ly()
      
      ball_labels <- sapply(1:6, function(i) t(paste0("ball_", i), lang))
      
      for(i in 1:6){
        ball_name <- paste0("Ball ", i)
        ball_label <- ball_labels[i]
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
                       name = ball_label)
        
        # 2️⃣ Box plot (centered)
        p <- add_trace(p,
                       x = rep(i, length(ball_values)),
                       y = ball_values,
                       type = 'box',
                       fillcolor = toRGB(color, alpha = 0.8),
                       line = list(color = color, width = 2),
                       boxpoints = FALSE,
                       width = 0.3,
                       name = ball_label,
                       showlegend = FALSE)
        
        # 3️⃣ Jittered points (manual jitter) - positioned to the left
        set.seed(42 + i)
        jitter_amount <- runif(length(ball_values), -0.35, -0.05)
        x_jittered <- rep(i, length(ball_values)) + jitter_amount
        
        p <- add_trace(p,
                       x = x_jittered,
                       y = ball_values,
                       type = 'scatter',
                       mode = 'markers',
                       marker = list(color = color, size = 4, opacity = 0.6),
                       showlegend = FALSE,
                       hoverinfo = 'y',
                       name = ball_label)
      }
      
      p %>%
        layout(
          title = t("balls_raincloud_title", lang),
          yaxis = list(title = t("value_label", lang), color = 'rgba(255,255,255,0.6)'),
          xaxis = list(
            title = t("ball_label", lang), 
            color = 'rgba(255,255,255,0.6)',
            tickmode = 'array',
            tickvals = 1:6,
            ticktext = ball_labels,
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