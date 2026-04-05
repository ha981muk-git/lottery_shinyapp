# ---------- Sum Analysis Module with Language Support ----------

sumsMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        uiOutput(ns("header"))
      ),
      
      # Statistics Row (Consolidated)
      uiOutput(ns("metricRow")),
      
      # Distribution and Trend Row
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        create_chart_card(ns, "chartTitle1", "chartDesc1", "hist"),
        create_chart_card(ns, "chartTitle2", "chartDesc2", "trend")
      ),
      
      # Range Analysis and Box Plot Row
      layout_column_wrap(
        width = 1/2,
        gap = "20px",
        heights_equal = "row",
        create_chart_card(ns, "chartTitle3", "chartDesc3", "rangeChart"),
        create_chart_card(ns, "chartTitle4", "chartDesc4", "boxPlot")
      ),
      
      # Moving Average and Volatility
      create_chart_card(ns, "chartTitle5", "chartDesc5", "movingAvg", style = "margin-top: 20px;")
    )
  )
}

sumsMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title", t("sums_title", lang)),
        p(class = "header-subtitle", t("sums_subtitle", lang))
      )
    })
    
    # Chart titles and descriptions
    output$chartTitle1 <- render_title("sums_chart_distribution", get_lang, "📊")
    output$chartDesc1 <- render_desc("sums_chart_distribution_desc", get_lang)
    output$chartTitle2 <- render_title("sums_chart_trend", get_lang, "📈")
    output$chartDesc2 <- render_desc("sums_chart_trend_desc", get_lang)
    output$chartTitle3 <- render_title("sums_chart_range", get_lang, "🎯")
    output$chartDesc3 <- render_desc("sums_chart_range_desc", get_lang)
    output$chartTitle4 <- render_title("sums_chart_boxplot", get_lang, "📦")
    output$chartDesc4 <- render_desc("sums_chart_boxplot_desc", get_lang)
    output$chartTitle5 <- render_title("sums_chart_moving", get_lang, "📉")
    output$chartDesc5 <- render_desc("sums_chart_moving_desc", get_lang)
    
    # Calculate sum statistics
    sum_stats <- reactive({
      data <- filtered_data()
      sums <- rowSums(data[, paste0("ball_", 1:6)])
      
      list(
        sums = sums,
        mean = mean(sums),
        median = median(sums),
        min = min(sums),
        max = max(sums),
        sd = sd(sums),
        most_common = as.numeric(names(sort(table(sums), decreasing = TRUE))[1])
      )
    })
    
    # Consolidated Metric Row
    output$metricRow <- renderUI({
      lang <- get_lang()
      stats <- sum_stats()
      
      create_card <- function(icon, value, label) {
        div(class = "value-box-custom",
            div(class = "value-box-icon", icon),
            div(class = "value-box-value", value),
            div(class = "value-box-label", label))
      }
      
      layout_column_wrap(
        width = 1/5,
        gap = "12px",
        heights_equal = "row",
        create_card("📊", round(stats$mean, 1), t("sums_metric_average", lang)),
        create_card("🎯", stats$median, t("sums_metric_median", lang)),
        create_card("⭐", stats$most_common, t("sums_metric_most_common", lang)),
        create_card("📉", stats$min, t("sums_metric_minimum", lang)),
        create_card("📈", stats$max, t("sums_metric_maximum", lang))
      )
    })
    
    # Histogram
    output$hist <- renderPlotly({
      lang <- get_lang()
      stats <- sum_stats()
      sums <- stats$sums
      
      # Calculate histogram data for proper scaling
      hist_data <- hist(sums, breaks = seq(min(sums), max(sums) + 5, by = 5), plot = FALSE)
      max_count <- max(hist_data$counts)
      
      # Calculate density curve
      density_data <- density(sums, adjust = 1.2)  # adjust = smoothness (higher = smoother)
      
      # Scale density to match histogram height
      # density values are probabilities, so we scale them to histogram counts
      bin_width <- 5  # your bin width
      density_scaled <- density_data$y * length(sums) * bin_width
      
      plot_ly() %>%
        # Histogram
        add_histogram(
          x = ~sums,
          marker = list(
            color = "#8b5cf6",
            line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)
          ),
          xbins = list(size = 5),
          name = t("sums_label_frequency", lang),
          hovertemplate = paste0(
            t("sums_label_sum_value", lang), ": %{x}<br>",
            t("sums_label_frequency", lang), ": %{y}<br>",
            "<extra></extra>"
          )
        ) %>%
        # Density curve (smooth overlay)
        add_trace(
          x = density_data$x,
          y = density_scaled,
          type = "scatter",
          mode = "lines",
          line = list(
            color = "#DC143C",  # Amber color for visibility
            width = 3,
            shape = "spline"  # Makes it extra smooth
          ),
          name = t("sums_density_curve", lang),  # Add translation: "Density Curve" / "Dichtekurve"
          fill = "tozeroy",
          fillcolor = "rgba(251, 191, 36, 0.15)",  # Subtle fill under curve
          hovertemplate = paste0(
            t("sums_label_sum_value", lang), ": %{x:.1f}<br>",
            t("sums_label_density", lang), ": %{y:.1f}<br>",  # Add translation
            "<extra></extra>"
          )
        ) %>%
        # Mean line
        add_trace(
          x = rep(stats$mean, 2),
          y = c(0, max_count * 1.1),
          type = "scatter",
          mode = "lines",
          line = list(color = "#ec4899", width = 3, dash = "dash"),
          name = t("sums_metric_average", lang),
          hovertemplate = paste0(
            t("sums_metric_average", lang), ": ", round(stats$mean, 1),
            "<extra></extra>"
          )
        ) %>%
        # Median line
        add_trace(
          x = rep(stats$median, 2),
          y = c(0, max_count * 1.1),
          type = "scatter",
          mode = "lines",
          line = list(color = "#10b981", width = 3, dash = "dot"),
          name = t("sums_metric_median", lang),
          hovertemplate = paste0(
            t("sums_metric_median", lang), ": ", stats$median,
            "<extra></extra>"
          )
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("sums_label_sum_value", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("sums_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          bargap = 0.1,
          hovermode = "x unified"  # Better hover experience
        )
    })
    
    # Trend Line
    output$trend <- renderPlotly({
      lang <- get_lang()
      stats <- sum_stats()
      sums <- stats$sums
      
      df <- data.frame(
        draw = 1:length(sums),
        sum = sums
      )
      
      plot_ly(df, x = ~draw, y = ~sum, type = "scatter", mode = "lines+markers",
              line = list(color = "#8b5cf6", width = 2),
              marker = list(
                color = "#ec4899",
                size = 6,
                line = list(color = "rgba(255, 255, 255, 0.5)", width = 1)
              ),
              hovertemplate = paste0(
                t("sums_label_draw", lang), " #%{x}<br>",
                t("sums_label_sum_value", lang), ": %{y}<br>",
                "<extra></extra>"
              )) %>%
        toWebGL() %>%
        add_trace(x = range(df$draw), y = rep(stats$mean, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 2, dash = "dash"),
                  name = t("sums_metric_average", lang),
                  hoverinfo = "skip") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("sums_label_draw_number", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("sums_label_sum_value", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          hovermode = "x unified"
        )
    })
    
    # Range Analysis
    output$rangeChart <- renderPlotly({
      lang <- get_lang()
      stats <- sum_stats()
      sums <- stats$sums
      
      # Define ranges
      ranges <- cut(sums, 
                    breaks = seq(floor(min(sums)/10)*10, ceiling(max(sums)/10)*10 + 10, by = 10),
                    include.lowest = TRUE)
      range_counts <- table(ranges)
      
      df <- data.frame(
        range = names(range_counts),
        count = as.numeric(range_counts)
      )
      
      df$percentage <- round(df$count / sum(df$count) * 100, 1)
      
      colors <- colorRampPalette(c("#4169E1", "#8b5cf6", "#ec4899", "#DC143C"))(nrow(df))
      
      plot_ly(df, x = ~range, y = ~count, type = "bar",
              marker = list(
                color = colors,
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(count, " (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 12),
              hovertemplate = paste0(
                "<b>%{x}</b><br>",
                t("sums_hover_count", lang), ": %{y}<br>",
                t("sums_hover_percentage", lang), ": %{text}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("sums_label_sum_range", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            tickangle = -45
          ),
          yaxis = list(
            title = t("sums_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          margin = list(b = 100)
        )
    })
    
    # Box Plot
    output$boxPlot <- renderPlotly({
      lang <- get_lang()
      stats <- sum_stats()
      
      plot_ly(y = ~stats$sums, type = "box",
              marker = list(color = "#8b5cf6"),
              line = list(color = "#ec4899", width = 2),
              fillcolor = "rgba(139, 92, 246, 0.3)",
              name = t("sums_label_sum_value", lang),
              boxmean = "sd",
              hovertemplate = paste0(
                t("sums_hover_value", lang), ": %{y}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          yaxis = list(
            title = t("sums_label_sum_value", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          xaxis = list(
            title = "",
            showticklabels = FALSE
          )
        )
    })
    
    # Moving Average with Bands
    output$movingAvg <- renderPlotly({
      lang <- get_lang()
      stats <- sum_stats()
      sums <- stats$sums
      
      df <- data.frame(draw = 1:length(sums), sum = sums)
      
      # Calculate moving average and standard deviation
      window <- min(20, length(sums))
      if(length(sums) >= window) {
        df$ma <- zoo::rollmean(df$sum, k = window, fill = NA, align = "right")
        df$sd <- zoo::rollapply(df$sum, width = window, FUN = sd, fill = NA, align = "right")
        df$upper <- df$ma + df$sd
        df$lower <- df$ma - df$sd
      }
      
      plot_ly(df, x = ~draw) %>%
        # Actual values
        add_trace(y = ~sum, name = t("sums_label_sum_value", lang), type = "scatter", mode = "lines",
                  line = list(color = "rgba(139, 92, 246, 0.4)", width = 1),
                  hovertemplate = paste0(t("sums_label_draw", lang), ": %{x}<br>", t("sums_label_sum_value", lang), ": %{y}<extra></extra>")) %>%
        # Upper band
        {if("upper" %in% names(df))
          add_trace(., y = ~upper, name = "Upper Band", type = "scatter", mode = "lines",
                    line = list(color = "rgba(236, 72, 153, 0.3)", width = 1, dash = "dot"),
                    hovertemplate = paste0("Upper: %{y:.1f}<extra></extra>"))
          else .
        } %>%
        # Lower band
        {if("lower" %in% names(df))
          add_trace(., y = ~lower, name = "Lower Band", type = "scatter", mode = "lines",
                    line = list(color = "rgba(79, 172, 254, 0.3)", width = 1, dash = "dot"),
                    fill = "tonexty", fillcolor = "rgba(139, 92, 246, 0.1)",
                    hovertemplate = paste0("Lower: %{y:.1f}<extra></extra>"))
          else .
        } %>%
        # Moving average
        {if("ma" %in% names(df))
          add_trace(., y = ~ma, name = t("sums_metric_average", lang), type = "scatter", mode = "lines",
                    line = list(color = "#10b981", width = 3),
                    hovertemplate = paste0("MA: %{y:.1f}<extra></extra>")) %>%
          toWebGL()
          else .
        } %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("sums_label_draw_number", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("sums_label_sum_value", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          )
        )
    })
  })
}