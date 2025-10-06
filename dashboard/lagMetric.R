# ---------- Lag Analysis Module - Number Jump Patterns ----------

lagMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "Lag Analysis - Number Jump Patterns"),
        p(class = "header-subtitle", "Analyze how numbers change between consecutive draws and identify movement patterns")
      ),
      
      # Statistics Row
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3")),
        uiOutput(ns("metricCard4"))
      ),
      
      # Ball Position Selector
      div(
        class = "content-card",
        div(
          style = "margin-bottom: 20px;",
          h4(style = "color: #8b5cf6;", "Select Ball Position to Analyze:"),
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap; justify-content: center;",
            actionButton(ns("ball1"), "Ball 1", class = "btn-action btn-primary", 
                         style = "background: #4169E1; border-color: #4169E1;"),
            actionButton(ns("ball2"), "Ball 2", class = "btn-action btn-primary",
                         style = "background: #DC143C; border-color: #DC143C;"),
            actionButton(ns("ball3"), "Ball 3", class = "btn-action btn-primary",
                         style = "background: #32CD32; border-color: #32CD32;"),
            actionButton(ns("ball4"), "Ball 4", class = "btn-action btn-primary",
                         style = "background: #FFD700; border-color: #FFD700; color: #1a1f3a;"),
            actionButton(ns("ball5"), "Ball 5", class = "btn-action btn-primary",
                         style = "background: #9370DB; border-color: #9370DB;"),
            actionButton(ns("ball6"), "Ball 6", class = "btn-action btn-primary",
                         style = "background: #00CED1; border-color: #00CED1;"),
            actionButton(ns("ballAll"), "All Balls Combined", class = "btn-action btn-success")
          )
        ),
        div(
          style = "text-align: center; margin-top: 15px;",
          uiOutput(ns("selectedBall"))
        )
      ),
      
      # Normal Distribution Chart
      div(
        class = "content-card",
        div(class = "card-title", span("📊"), span("Lag Distribution with Normal Curve")),
        p(class = "info-text", "Distribution of number changes (lag) compared to theoretical normal distribution"),
        plotlyOutput(ns("lagDistribution"), height = "500px")
      ),
      
      # Jump Preference Charts
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "content-card",
          div(class = "card-title", span("⬆️"), span("Positive Jumps (Increases)")),
          p(class = "info-text", "When numbers jump UP from previous draw"),
          plotlyOutput(ns("positiveJumps"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("⬇️"), span("Negative Jumps (Decreases)")),
          p(class = "info-text", "When numbers jump DOWN from previous draw"),
          plotlyOutput(ns("negativeJumps"), height = "400px")
        )
      ),
      
      # Jump Categories
      div(
        class = "content-card",
        div(class = "card-title", span("🎯"), span("Jump Categories Distribution")),
        p(class = "info-text", "Classification of jumps by magnitude"),
        plotlyOutput(ns("jumpCategories"), height = "450px")
      ),
      
      # Heatmap and QQ Plot
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "content-card",
          div(class = "card-title", span("🔥"), span("Lag Frequency Heatmap")),
          p(class = "info-text", "Visual representation of most/least common jumps"),
          plotlyOutput(ns("lagHeatmap"), height = "450px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("📈"), span("Q-Q Plot (Normality Test)")),
          p(class = "info-text", "Check if lag follows normal distribution (points on line = normal)"),
          plotlyOutput(ns("qqPlot"), height = "450px")
        )
      ),
      
      # Preferred Zones
      div(
        class = "content-card",
        div(class = "card-title", span("🎲"), span("Preferred Jump Zones")),
        p(class = "info-text", "Areas where numbers prefer to jump (hot zones) vs avoid (cold zones)"),
        plotlyOutput(ns("preferredZones"), height = "450px")
      ),
      
      # Statistical Summary
      div(
        class = "content-card",
        style = "margin-top: 20px;",
        div(class = "card-title", span("📊"), span("Statistical Summary & Recommendations")),
        uiOutput(ns("statSummary"))
      ),
      
      # Detailed Table
      div(
        class = "content-card",
        style = "margin-top: 20px;",
        div(class = "card-title", span("📋"), span("Detailed Lag Statistics")),
        p(class = "info-text", "Complete lag data with frequencies and probabilities"),
        DT::dataTableOutput(ns("lagTable"))
      )
    )
  )
}

lagMetricServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Track selected ball
    selected_ball <- reactiveVal(0)  # 0 = all balls
    
    # Ball selection handlers
    observeEvent(input$ball1, { selected_ball(1) })
    observeEvent(input$ball2, { selected_ball(2) })
    observeEvent(input$ball3, { selected_ball(3) })
    observeEvent(input$ball4, { selected_ball(4) })
    observeEvent(input$ball5, { selected_ball(5) })
    observeEvent(input$ball6, { selected_ball(6) })
    observeEvent(input$ballAll, { selected_ball(0) })
    
    # Initialize with all balls
    observe({
      if(is.null(selected_ball())) {
        selected_ball(0)
      }
    })
    
    # Display selected ball
    output$selectedBall <- renderUI({
      ball <- selected_ball()
      if(is.null(ball)) ball <- 0
      
      ball_colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      
      text <- if(ball == 0) "All Balls Combined" else paste0("Ball ", ball)
      color <- if(ball == 0) "#8b5cf6" else ball_colors[ball]
      
      div(
        style = paste0("font-size: 24px; font-weight: bold; color: ", color, ";"),
        paste0("Analyzing: ", text)
      )
    })
    
    # Calculate lag statistics
    lag_stats <- reactive({
      data <- filtered_data()
      ball <- selected_ball()
      if(is.null(ball)) ball <- 0
      
      if(nrow(data) < 2) {
        return(list(lags = numeric(0), lag_df = data.frame()))
      }
      
      # Calculate lags (difference from previous draw)
      if(ball == 0) {
        # All balls combined
        all_lags <- c()
        for(b in 1:6) {
          col <- data[[paste0("ball_", b)]]
          lags <- diff(col)
          all_lags <- c(all_lags, lags)
        }
        lags <- all_lags
      } else {
        # Specific ball
        col <- data[[paste0("ball_", ball)]]
        lags <- diff(col)
      }
      
      # Create frequency table
      lag_table <- table(lags)
      lag_df <- data.frame(
        lag = as.numeric(names(lag_table)),
        frequency = as.numeric(lag_table)
      )
      
      # Calculate statistics
      lag_df$percentage <- round((lag_df$frequency / sum(lag_df$frequency)) * 100, 2)
      lag_df$probability <- lag_df$frequency / sum(lag_df$frequency)
      
      # Categorize lags
      lag_df$category <- cut(abs(lag_df$lag),
                             breaks = c(0, 3, 7, 15, Inf),
                             labels = c("Tiny (0-3)", "Small (4-7)", "Medium (8-15)", "Large (>15)"),
                             include.lowest = TRUE)
      
      lag_df$direction <- ifelse(lag_df$lag > 0, "Increase", 
                                 ifelse(lag_df$lag < 0, "Decrease", "No Change"))
      
      list(
        lags = lags,
        lag_df = lag_df,
        mean = mean(lags),
        sd = sd(lags),
        median = median(lags),
        min = min(lags),
        max = max(lags),
        most_common = as.numeric(names(sort(table(lags), decreasing = TRUE))[1])
      )
    })
    
    # Metric Cards
    output$metricCard1 <- renderUI({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📊"),
        div(class = "value-box-value", round(stats$mean, 2)),
        div(class = "value-box-label", "Average Jump")
      )
    })
    
    output$metricCard2 <- renderUI({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📏"),
        div(class = "value-box-value", round(stats$sd, 2)),
        div(class = "value-box-label", "Std Deviation")
      )
    })
    
    output$metricCard3 <- renderUI({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⭐"),
        div(class = "value-box-value", stats$most_common),
        div(class = "value-box-label", "Most Common Jump")
      )
    })
    
    output$metricCard4 <- renderUI({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      range_val <- stats$max - stats$min
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📈"),
        div(class = "value-box-value", range_val),
        div(class = "value-box-label", "Jump Range")
      )
    })
    
    # Lag Distribution with Normal Curve
    output$lagDistribution <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      # Create histogram data
      hist_data <- hist(stats$lags, breaks = 30, plot = FALSE)
      
      # Calculate normal distribution curve
      x_seq <- seq(min(stats$lags), max(stats$lags), length.out = 100)
      y_norm <- dnorm(x_seq, mean = stats$mean, sd = stats$sd)
      # Scale to match histogram
      y_norm_scaled <- y_norm * length(stats$lags) * diff(hist_data$breaks[1:2])
      
      plot_ly() %>%
        add_bars(x = hist_data$mids, y = hist_data$counts,
                 marker = list(
                   color = "#8b5cf6",
                   line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)
                 ),
                 name = "Actual",
                 hovertemplate = "Lag: %{x}<br>Frequency: %{y}<extra></extra>") %>%
        add_lines(x = x_seq, y = y_norm_scaled,
                  line = list(color = "#ec4899", width = 3),
                  name = "Normal Distribution",
                  hovertemplate = "Lag: %{x:.1f}<br>Expected: %{y:.1f}<extra></extra>") %>%
        add_trace(x = c(stats$mean, stats$mean),
                  y = c(0, max(hist_data$counts) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = "Mean",
                  hovertemplate = paste0("Mean: ", round(stats$mean, 2), "<extra></extra>"),
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Lag (Difference from Previous Draw)",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          bargap = 0.05
        )
    })
    
    # Positive Jumps
    output$positiveJumps <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      positive <- df[df$lag > 0, ]
      
      if(nrow(positive) == 0) return(NULL)
      
      positive <- positive[order(-positive$frequency), ][1:min(15, nrow(positive)), ]
      
      plot_ly(positive, x = ~reorder(lag, frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#32CD32", "#10b981"))(nrow(positive)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = "<b>Jump: +%{x}</b><br>Frequency: %{y}<br>Percentage: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Jump Size",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Negative Jumps
    output$negativeJumps <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      negative <- df[df$lag < 0, ]
      
      if(nrow(negative) == 0) return(NULL)
      
      negative <- negative[order(-negative$frequency), ][1:min(15, nrow(negative)), ]
      
      plot_ly(negative, x = ~reorder(lag, -frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#ef4444", "#DC143C"))(nrow(negative)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = "<b>Jump: %{x}</b><br>Frequency: %{y}<br>Percentage: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Jump Size",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Jump Categories
    output$jumpCategories <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      category_summary <- aggregate(frequency ~ category + direction, data = df, sum)
      
      colors <- list(
        "Increase" = c("#32CD32", "#10b981", "#059669", "#047857"),
        "Decrease" = c("#ef4444", "#DC143C", "#b91c1c", "#991b1b"),
        "No Change" = c("#8b5cf6")
      )
      
      plot_ly() %>%
        {
          p <- .
          for(dir in unique(category_summary$direction)) {
            data_dir <- category_summary[category_summary$direction == dir, ]
            color_set <- colors[[dir]]
            p <- add_trace(p, 
                           data = data_dir,
                           x = ~category, 
                           y = ~frequency, 
                           type = "bar",
                           name = dir,
                           marker = list(color = color_set[1]),
                           hovertemplate = paste0("<b>", dir, " - %{x}</b><br>Frequency: %{y}<extra></extra>"))
          }
          p
        } %>%
        layout(
          barmode = "stack",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Jump Category",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          )
        )
    })
    
    # Lag Heatmap
    output$lagHeatmap <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      
      # Create grid
      n_cols <- 10
      n_rows <- ceiling(nrow(df) / n_cols)
      total_cells <- n_rows * n_cols
      
      padded_lag <- c(df$lag, rep(NA, total_cells - nrow(df)))
      padded_freq <- c(df$frequency, rep(0, total_cells - nrow(df)))
      
      mat <- matrix(padded_freq, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      labels <- matrix(padded_lag, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      
      plot_ly(z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
              colorscale = list(
                c(0, "rgba(79, 172, 254, 0.2)"),
                c(0.5, "rgba(139, 92, 246, 0.7)"),
                c(1, "rgba(236, 72, 153, 1)")
              ),
              text = labels,
              hovertemplate = "<b>Lag: %{text}</b><br>Frequency: %{z}<extra></extra>",
              showscale = TRUE,
              colorbar = list(
                title = "Frequency",
                titlefont = list(color = "#e8eaed"),
                tickfont = list(color = "#e8eaed")
              )) %>%
        add_annotations(
          x = rep(1:n_cols, each = n_rows),
          y = rep(1:n_rows, times = n_cols),
          text = as.vector(t(labels)),
          showarrow = FALSE,
          font = list(color = "#FFFFFF", size = 10, family = "Inter", weight = "bold")
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        )
    })
    
    # Q-Q Plot
    output$qqPlot <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      # Calculate Q-Q plot data
      theoretical <- qqnorm(stats$lags, plot.it = FALSE)
      
      plot_ly() %>%
        add_markers(x = theoretical$x, y = theoretical$y,
                    marker = list(color = "#8b5cf6", size = 6),
                    name = "Data Points",
                    hovertemplate = "Theoretical: %{x:.2f}<br>Sample: %{y:.2f}<extra></extra>") %>%
        add_lines(x = range(theoretical$x), y = range(theoretical$x),
                  line = list(color = "#ec4899", width = 3, dash = "dash"),
                  name = "Perfect Normal",
                  hovertemplate = "Perfect Normal Line<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Theoretical Quantiles",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Sample Quantiles",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          )
        )
    })
    
    # Preferred Zones
    output$preferredZones <- renderPlotly({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      df <- df[order(df$lag), ]
      
      # Define zones
      df$zone <- ifelse(df$percentage >= 2, "Hot Zone",
                        ifelse(df$percentage >= 1, "Warm Zone",
                               ifelse(df$percentage >= 0.5, "Cool Zone", "Cold Zone")))
      
      zone_colors <- c("Hot Zone" = "#DC143C", "Warm Zone" = "#ff6b6b",
                       "Cool Zone" = "#4facfe", "Cold Zone" = "#4169E1")
      
      plot_ly(df, x = ~lag, y = ~percentage, type = "bar",
              marker = list(
                color = ~zone,
                colors = zone_colors,
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1)
              ),
              text = ~zone,
              hovertemplate = "<b>Lag: %{x}</b><br>Percentage: %{y}%<br>Zone: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Lag Value",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Percentage (%)",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          )
        )
    })
    
    # Statistical Summary
    output$statSummary <- renderUI({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(p("No data available"))
      
      df <- stats$lag_df
      
      # Hot zones (top occurrences)
      hot_lags <- df[order(-df$frequency), ][1:min(5, nrow(df)), ]
      
      # Calculate confidence interval
      ci_lower <- stats$mean - 1.96 * stats$sd
      ci_upper <- stats$mean + 1.96 * stats$sd
      
      # Shapiro test for normality
      shapiro_result <- if(length(stats$lags) >= 3 && length(stats$lags) <= 5000) {
        shapiro.test(stats$lags)$p.value
      } else NA
      
      tagList(
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; padding: 20px;",
          
          # Top Preferred Jumps
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #ec4899; margin-bottom: 15px;", "Top 5 Preferred Jumps"),
            tags$ul(
              style = "list-style: none; padding: 0;",
              lapply(1:nrow(hot_lags), function(i) {
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid rgba(255,255,255,0.1);",
                  tags$span(
                    style = "font-size: 18px; font-weight: bold; color: #8b5cf6;",
                    hot_lags$lag[i]
                  ),
                  tags$span(
                    style = "margin-left: 15px; color: rgba(255,255,255,0.7);",
                    paste0("(", hot_lags$percentage[i], "%)")
                  )
                )
              })
            )
          ),
          
          # Statistical Properties
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #10b981; margin-bottom: 15px;", "Statistical Properties"),
            div(
              style = "line-height: 1.8;",
              div(
                tags$strong("95% Confidence Interval: "),
                tags$span(paste0("[", round(ci_lower, 1), ", ", round(ci_upper, 1), "]"))
              ),
              div(
                tags$strong("Expected Range: "),
                tags$span(paste0(round(stats$mean - stats$sd, 1), " to ", round(stats$mean + stats$sd, 1)))
              ),
              div(
                tags$strong("Normality: "),
                tags$span(
                  if(!is.na(shapiro_result)) {
                    if(shapiro_result > 0.05) "Follows Normal Distribution ✓" else "Deviates from Normal ✗"
                  } else "Test not applicable"
                )
              )
            )
          ),
          
          # Recommendations
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #FFD700; margin-bottom: 15px;", "Recommendations"),
            div(
              style = "line-height: 1.8; color: rgba(255,255,255,0.8);",
              div("✓ Focus on hot zone jumps (≥2%)"),
              div("✓ Expect jumps within ±", round(stats$sd, 1), " of mean"),
              div("✓ Avoid cold zone jumps (<0.5%)"),
              div("✓ Most likely jump: ", stats$most_common)
            )
          )
        )
      )
    })
    
    # Lag Table
    output$lagTable <- DT::renderDataTable({
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      
      df_display <- data.frame(
        Lag = df$lag,
        Frequency = df$frequency,
        Percentage = paste0(df$percentage, "%"),
        Probability = round(df$probability, 4),
        Category = df$category,
        Direction = df$direction
      )
      
      DT::datatable(
        df_display,
        options = list(
          pageLength = 15,
          order = list(list(1, 'desc')),
          dom = 'frtip',
          scrollX = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().container()).css({'background-color': 'rgba(255,255,255,0.05)', 'color': '#e8eaed'});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          columns = 1:6,
          backgroundColor = 'rgba(255,255,255,0.02)',
          color = '#e8eaed'
        ) %>%
        DT::formatStyle(
          'Frequency',
          background = DT::styleColorBar(range(df$frequency), '#8b5cf6'),
          backgroundSize = '90% 70%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
  })
}