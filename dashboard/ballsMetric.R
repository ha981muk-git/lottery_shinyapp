# balls Metrics UI with Translation Support

# -------------------------
# Module: ballsMetricModule
# -------------------------

# Define consistent colors for each ball (hex codes)
# Moved to global scope for performance (defined once, not per session)
BALL_COLORS <- c(
  "Ball 1" = "#4169E1",  # royal blue
  "Ball 2" = "#DC143C",  # crimson red
  "Ball 3" = "#32CD32",  # lime green
  "Ball 4" = "#FFD700",  # gold/yellow
  "Ball 5" = "#9370DB",  # medium purple
  "Ball 6" = "#00CED1"   # dark cyan
)

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
      # Consolidated Metric Row (Faster Rendering)
      uiOutput(ns("metricRow")),
      
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        create_chart_card(ns, "trendChartTitle", NULL, "trendChart", height = "350px"),
        create_chart_card(ns, "distributionChartTitle", NULL, "distributionChart", height = "350px")
      ),
      create_chart_card(ns, "densityChartTitle", NULL, "densityChart", height = "400px", style = "margin-top: 20px;"),
      create_chart_card(ns, "overviewChartTitle", NULL, "overviewChart", height = "400px", style = "margin-top: 20px;"),
      create_chart_card(ns, "lineChartTitle", NULL, "lineChart", height = "400px", style = "margin-top: 20px;")
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
    
    # Render header
    output$header <- renderUI({
      lang <- get_lang()
      tagList(
        h1(class = "header-title", t("balls_title", lang)),
        p(class = "header-subtitle", t("balls_subtitle", lang))
      )
    })
    
    # Chart titles
    output$trendChartTitle <- render_title("balls_trend_title", get_lang)
    output$distributionChartTitle <- render_title("balls_distribution_title", get_lang)
    output$densityChartTitle <- render_title("balls_chart_density", get_lang)
    output$overviewChartTitle <- render_title("balls_overview_title", get_lang)
    output$lineChartTitle <- render_title("balls_line_chart_title", get_lang)
    
    create_metric_card <- function(title, value, value_symbol) {
      div(
        class = "metric-card",
        div(class = "metric-label", title),
        div(class = "metric-value", paste0(value, value_symbol))
      )
    }
    
    # Calculate metrics separately (Best Practice: Separation of Concerns)
    metrics_stats <- reactive({
      data <- filtered_data()
      controls <- input_controls()
      
      # Calc 1
      selected <- nrow(data)
      total_counts <- getOption("li_base_row_count") %||% nrow(generate_metrics())
      coverage <- (selected/total_counts) * 100
      
      # Calc 2
      occurrence <- selected
      
      # Calc 3
      N_Tickets <- 13983816
      chance <- occurrence / N_Tickets * 100
      
      # Calc 4
      num_from <- as.numeric(controls$range[1])
      num_to <- as.numeric(controls$range[2])
      difference <- num_to - num_from + 1
      
      list(
        coverage = coverage,
        occurrence = occurrence,
        chance = chance,
        difference = difference
      )
    })
    
    # Consolidated Metric Row Renderer
    output$metricRow <- renderUI({
      lang <- get_lang()
      stats <- metrics_stats()
      
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        create_metric_card(t("balls_metric_coverage", lang), paste0("", format(round(stats$coverage), big.mark = ",")), "%"),
        create_metric_card(t("balls_metric_occurrence", lang), format(round(stats$occurrence)), ""),
        create_metric_card(t("balls_metric_chance", lang), paste0(format(stats$chance, digits = 2)), "%"),
        create_metric_card(t("balls_metric_range", lang), stats$difference, "")
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
                       fillcolor = BALL_COLORS[ball_name],
                       marker = list(color = BALL_COLORS[ball_name]),
                       line = list(color = BALL_COLORS[ball_name]))
      }
      
      p %>%
        toWebGL() %>% layout(
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
                         fillcolor = toRGB(BALL_COLORS[ball_name], alpha = 0.3),
                         line = list(color = BALL_COLORS[ball_name], width = 2)
                       ),
                       meanline = list(visible = TRUE),
                       fillcolor = toRGB(BALL_COLORS[ball_name], alpha = 0.6),
                       line = list(color = BALL_COLORS[ball_name]),
                       opacity = 0.6)
      }
      
      p %>%
        toWebGL() %>% layout(
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
      lang <- get_lang()
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
          line = list(color = BALL_COLORS[ball_name], width = 2)
        )
      }
      
      p %>%
        toWebGL() %>% layout(
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
        color <- BALL_COLORS[ball_name]
        
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
        toWebGL() %>% layout(
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
    
    # Line Chart - Each row represents a line connecting Ball 1 to Ball 6
    output$lineChart <- renderPlotly({
      lang <- get_lang()
      data <- filtered_data()
      
      p <- plot_ly()
      
      # X-axis positions for each ball (1 to 6)
      x_positions <- 1:6
      ball_labels <- sapply(1:6, function(i) t(paste0("ball_", i), lang))
      
      # Optimize: Pivot data to long format for single-trace plotting
      # This replaces the slow for-loop that added a trace per row
      plot_data <- data %>%
        dplyr::mutate(draw_id = row_number()) %>%
        tidyr::pivot_longer(cols = starts_with("ball_"), names_to = "ball", values_to = "value") %>%
        dplyr::mutate(ball_num = as.numeric(gsub("ball_", "", ball)))

      p <- plot_ly(plot_data, x = ~ball_num, y = ~value, split = ~draw_id,
                   type = 'scatter',
                   mode = 'lines+markers',
                   line = list(color = 'rgba(255,255,255,0.3)', width = 1),
                   marker = list(size = 4, color = 'rgba(255,255,255,0.4)'),
                   hoverinfo = 'y+x',
                   showlegend = FALSE,
                   name = ~paste("Row", draw_id)) %>%
        toWebGL() # GPU Acceleration for many lines
      
      p %>%
        toWebGL() %>% layout(
          title = t("balls_line_chart", lang),
          xaxis = list(
            title = t("ball_label", lang),
            color = 'rgba(255,255,255,0.6)',
            tickmode = 'array',
            tickvals = x_positions,
            ticktext = ball_labels,
            range = c(0.5, 6.5)
          ),
          yaxis = list(
            title = t("value_label", lang),
            color = 'rgba(255,255,255,0.6)'
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          font = list(color = 'rgba(255,255,255,0.6)'),
          hovermode = 'closest'
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}