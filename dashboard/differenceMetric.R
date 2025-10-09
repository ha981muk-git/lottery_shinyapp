# ---------- Ball Range Analysis Module (Ball 6 - Ball 1) ----------
differenceMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      gap = "12px",
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "Ball Range Analysis"),
        p(class = "header-subtitle", "Analyze the difference between Ball 6 and Ball 1 to identify winning patterns")
      ),
      
      # Statistics Row - FIXED ORDER
      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",  # ← This must come BEFORE gap
        gap = "12px",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3")),
        uiOutput(ns("metricCard4")),
        uiOutput(ns("metricCard5"))
      ),
      
      # Main Frequency Distribution - ADD margin-top
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("📊"), span("Range Difference Frequency")),
        p(class = "info-text", "How often each difference value occurs (Ball 6 - Ball 1)"),
        plotlyOutput(ns("rangeFreq"), height = "450px")
      ),
      
      # Hot and Cold Ranges - FIXED ORDER + ADD margin-top
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",  # ← This must come BEFORE gap
        gap = "20px",
        fill = FALSE,  # ← ADD THIS to prevent white background
        div(
          class = "content-card",
          div(class = "card-title", span("🔥"), span("Hot Ranges")),
          p(class = "info-text", "Most frequently occurring differences - these ranges appear often"),
          plotlyOutput(ns("hotRanges"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("❄️"), span("Cold Ranges")),
          p(class = "info-text", "Least frequently occurring differences - these ranges are rare"),
          plotlyOutput(ns("coldRanges"), height = "400px")
        )
      ),
      
      # Range Categories - ADD margin-top
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("🎯"), span("Range Categories")),
        p(class = "info-text", "Distribution of ranges by category (Small, Medium, Large, Very Large)"),
        plotlyOutput(ns("rangeCategories"), height = "450px")
      ),
      
      # Trend and Box Plot - FIXED ORDER + ADD margin-top
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",  # ← This must come BEFORE gap
        gap = "20px",
        fill = FALSE,  # ← ADD THIS to prevent white background
        div(
          class = "content-card",
          div(class = "card-title", span("📈"), span("Range Trend Over Time")),
          p(class = "info-text", "How the range difference changes across draws"),
          plotlyOutput(ns("rangeTrend"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("📦"), span("Statistical Distribution")),
          p(class = "info-text", "Box plot showing quartiles, median, and outliers"),
          plotlyOutput(ns("rangeBox"), height = "400px")
        )
      ),
      
      # Rest stays the same with margin-top...
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("🔥"), span("Range Frequency Heatmap")),
        p(class = "info-text", "Visual heatmap showing frequency intensity - darker colors indicate more common ranges"),
        plotlyOutput(ns("rangeHeatmap"), height = "400px")
      ),
      
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("🎲"), span("Range Selection Guide")),
        p(class = "info-text", "Recommended ranges based on historical data"),
        div(
          style = "padding: 20px;",
          uiOutput(ns("rangeGuide"))
        )
      ),
      
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("📋"), span("Detailed Range Statistics")),
        p(class = "info-text", "Complete data table with all range differences and their statistics"),
        DT::dataTableOutput(ns("rangeTable"))
      )
    )
  )
}

differenceMetricServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate range statistics
    range_stats <- reactive({
      data <- filtered_data()
      
      # Calculate difference between ball 6 and ball 1
      ranges <- data$ball_6 - data$ball_1
      
      # Create frequency table
      range_freq <- table(ranges)
      range_df <- data.frame(
        range = as.numeric(names(range_freq)),
        frequency = as.numeric(range_freq)
      )
      
      # Calculate statistics
      range_df$percentage <- round((range_df$frequency / sum(range_df$frequency)) * 100, 2)
      range_df$cumulative <- cumsum(range_df$percentage)
      
      # Categorize ranges
      range_df$category <- cut(range_df$range, 
                               breaks = c(-Inf, 15, 25, 35, Inf),
                               labels = c("Small (≤15)", "Medium (16-25)", "Large (26-35)", "Very Large (>35)"))
      
      list(
        ranges = ranges,
        range_df = range_df,
        mean = mean(ranges),
        median = median(ranges),
        min = min(ranges),
        max = max(ranges),
        sd = sd(ranges),
        most_common = as.numeric(names(sort(table(ranges), decreasing = TRUE))[1])
      )
    })
    
    # Metric Cards
    output$metricCard1 <- renderUI({
      stats <- range_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📊"),
        div(class = "value-box-value", round(stats$mean, 1)),
        div(class = "value-box-label", "Average Range")
      )
    })
    
    output$metricCard2 <- renderUI({
      stats <- range_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "🎯"),
        div(class = "value-box-value", stats$median),
        div(class = "value-box-label", "Median Range")
      )
    })
    
    output$metricCard3 <- renderUI({
      stats <- range_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⭐"),
        div(class = "value-box-value", stats$most_common),
        div(class = "value-box-label", "Most Common")
      )
    })
    
    output$metricCard4 <- renderUI({
      stats <- range_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📉"),
        div(class = "value-box-value", stats$min),
        div(class = "value-box-label", "Minimum Range")
      )
    })
    
    output$metricCard5 <- renderUI({
      stats <- range_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📈"),
        div(class = "value-box-value", stats$max),
        div(class = "value-box-label", "Maximum Range")
      )
    })
    
    # Range Frequency Chart
    output$rangeFreq <- renderPlotly({
      stats <- range_stats()
      df <- stats$range_df
      
      plot_ly(df, x = ~range, y = ~frequency, type = "bar",
              marker = list(
                color = ~frequency,
                colorscale = list(
                  c(0, "#4169E1"),
                  c(0.33, "#8b5cf6"),
                  c(0.66, "#ec4899"),
                  c(1, "#DC143C")
                ),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5),
                showscale = TRUE,
                colorbar = list(
                  title = "Frequency",
                  titlefont = list(color = "#e8eaed"),
                  tickfont = list(color = "#e8eaed")
                )
              ),
              customdata = ~cbind(percentage, category),
              hovertemplate = "<b>Range: %{x}</b><br>Frequency: %{y}<br>Percentage: %{customdata[0]}%<br>Category: %{customdata[1]}<extra></extra>",
              name = "Frequency") %>%
        add_trace(x = c(stats$mean, stats$mean),
                  y = c(0, max(df$frequency) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = "Average",
                  hovertemplate = paste0("Average: ", round(stats$mean, 1), "<extra></extra>"),
                  showlegend = TRUE,
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Range Difference (Ball 6 - Ball 1)",
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
    
    # Hot Ranges
    output$hotRanges <- renderPlotly({
      stats <- range_stats()
      df <- stats$range_df
      hot <- df[order(-df$frequency), ][1:min(10, nrow(df)), ]
      
      plot_ly(hot, x = ~reorder(range, frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#ff6b6b", "#DC143C"))(nrow(hot)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " times (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = "<b>Range: %{x}</b><br>Frequency: %{y}<br>Percentage: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Range",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Cold Ranges
    output$coldRanges <- renderPlotly({
      stats <- range_stats()
      df <- stats$range_df
      cold <- df[order(df$frequency), ][1:min(10, nrow(df)), ]
      
      plot_ly(cold, x = ~reorder(range, -frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#00f2fe", "#4facfe"))(nrow(cold)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " times (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = "<b>Range: %{x}</b><br>Frequency: %{y}<br>Percentage: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Range",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Range Categories
    output$rangeCategories <- renderPlotly({
      stats <- range_stats()
      df <- stats$range_df
      
      category_summary <- aggregate(frequency ~ category, data = df, sum)
      category_summary$percentage <- round((category_summary$frequency / sum(category_summary$frequency)) * 100, 1)
      
      colors <- c("#4169E1", "#8b5cf6", "#ec4899", "#DC143C")
      
      plot_ly(category_summary, 
              labels = ~category, 
              values = ~frequency, 
              type = "pie",
              marker = list(
                colors = colors,
                line = list(color = "#FFFFFF", width = 2)
              ),
              text = ~paste0(percentage, "%"),
              textinfo = "label+text",
              textfont = list(size = 14, color = "#FFFFFF", family = "Inter"),
              hovertemplate = "<b>%{label}</b><br>Frequency: %{value}<br>Percentage: %{text}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            x = 1.05,
            y = 0.5
          )
        )
    })
    
    # Range Trend
    output$rangeTrend <- renderPlotly({
      stats <- range_stats()
      ranges <- stats$ranges
      
      df <- data.frame(
        draw = 1:length(ranges),
        range = ranges
      )
      
      # Add moving average if enough data
      window_size <- min(20, nrow(df))
      if(nrow(df) >= window_size) {
        df$ma <- zoo::rollmean(df$range, k = window_size, fill = NA, align = "right")
      }
      
      plot_ly(df, x = ~draw) %>%
        add_trace(y = ~range, name = "Range", type = "scatter", mode = "lines",
                  line = list(color = "rgba(139, 92, 246, 0.5)", width = 1.5),
                  hovertemplate = "Draw: %{x}<br>Range: %{y}<extra></extra>") %>%
        {if("ma" %in% names(df))
          add_trace(., y = ~ma, name = "Moving Avg", type = "scatter", mode = "lines",
                    line = list(color = "#10b981", width = 3),
                    hovertemplate = "Draw: %{x}<br>MA: %{y:.1f}<extra></extra>")
          else .
        } %>%
        add_trace(x = c(min(df$draw), max(df$draw)),
                  y = rep(stats$mean, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#ec4899", width = 2, dash = "dash"),
                  name = "Overall Avg",
                  hovertemplate = paste0("Average: ", round(stats$mean, 1), "<extra></extra>"),
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Draw Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Range Value",
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
    
    # Box Plot
    output$rangeBox <- renderPlotly({
      stats <- range_stats()
      
      plot_ly(y = ~stats$ranges, type = "box",
              marker = list(color = "#8b5cf6"),
              line = list(color = "#ec4899", width = 2),
              fillcolor = "rgba(139, 92, 246, 0.3)",
              name = "Range Distribution",
              boxmean = "sd",
              hovertemplate = "Value: %{y}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          yaxis = list(
            title = "Range Value",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          xaxis = list(
            title = "",
            showticklabels = FALSE
          )
        )
    })
    
    # Heatmap
    output$rangeHeatmap <- renderPlotly({
      stats <- range_stats()
      df <- stats$range_df
      
      # Create grid for better visualization
      n_cols <- 10
      n_rows <- ceiling(nrow(df) / n_cols)
      total_cells <- n_rows * n_cols
      
      # Pad data
      padded_range <- c(df$range, rep(NA, total_cells - nrow(df)))
      padded_freq <- c(df$frequency, rep(0, total_cells - nrow(df)))
      
      mat <- matrix(padded_freq, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      labels <- matrix(padded_range, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      
      plot_ly(z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
              colorscale = list(
                c(0, "rgba(79, 172, 254, 0.2)"),
                c(0.5, "rgba(139, 92, 246, 0.7)"),
                c(1, "rgba(236, 72, 153, 1)")
              ),
              text = labels,
              hovertemplate = "<b>Range: %{text}</b><br>Frequency: %{z}<extra></extra>",
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
          font = list(color = "#FFFFFF", size = 12, family = "Inter", weight = "bold")
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            showticklabels = FALSE,
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            showticklabels = FALSE,
            showgrid = FALSE,
            zeroline = FALSE
          )
        )
    })
    
    # Range Selection Guide
    output$rangeGuide <- renderUI({
      stats <- range_stats()
      df <- stats$range_df
      
      # Get top 5 ranges
      top_ranges <- df[order(-df$frequency), ][1:5, ]
      
      # Calculate optimal range
      optimal_start <- round(stats$mean - stats$sd)
      optimal_end <- round(stats$mean + stats$sd)
      
      tagList(
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 20px;",
          
          # Top 5 Recommended Ranges
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #ec4899; margin-bottom: 15px;", "🔥 Top 5 Hot Ranges"),
            tags$ul(
              style = "list-style: none; padding: 0;",
              lapply(1:nrow(top_ranges), function(i) {
                tags$li(
                  style = "padding: 8px 0; border-bottom: 1px solid rgba(255,255,255,0.1);",
                  tags$span(
                    style = "font-size: 20px; font-weight: bold; color: #8b5cf6;",
                    top_ranges$range[i]
                  ),
                  tags$span(
                    style = "margin-left: 15px; color: rgba(255,255,255,0.7);",
                    paste0("(", top_ranges$frequency[i], " times, ", top_ranges$percentage[i], "%)")
                  )
                )
              })
            )
          ),
          
          # Optimal Range
          div(
            class = "value-box-custom",
            style = "text-align: center;",
            h4(style = "color: #10b981; margin-bottom: 15px;", "🎯 Optimal Range"),
            div(
              style = "font-size: 32px; font-weight: bold; background: linear-gradient(135deg, #8b5cf6, #ec4899); -webkit-background-clip: text; -webkit-text-fill-color: transparent;",
              paste0(optimal_start, " - ", optimal_end)
            ),
            p(
              style = "margin-top: 10px; color: rgba(255,255,255,0.6);",
              "Based on Mean ± 1 Std Dev"
            ),
            p(
              style = "color: rgba(255,255,255,0.8);",
              paste0("Covers ~68% of all draws")
            )
          ),
          
          # Category Recommendations
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #FFD700; margin-bottom: 15px;", "📊 Category Guide"),
            div(
              style = "padding: 8px 0;",
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #4169E1; font-weight: bold;", "Small (≤15): "),
                tags$span(style = "color: rgba(255,255,255,0.7);", "Compact spread")
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #8b5cf6; font-weight: bold;", "Medium (16-25): "),
                tags$span(style = "color: rgba(255,255,255,0.7);", "Balanced spread")
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #ec4899; font-weight: bold;", "Large (26-35): "),
                tags$span(style = "color: rgba(255,255,255,0.7);", "Wide spread")
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #DC143C; font-weight: bold;", "Very Large (>35): "),
                tags$span(style = "color: rgba(255,255,255,0.7);", "Maximum spread")
              )
            )
          )
        )
      )
    })
    
    # Range Table
    output$rangeTable <- DT::renderDataTable({
      stats <- range_stats()
      df <- stats$range_df
      
      df_display <- data.frame(
        Range = df$range,
        Frequency = df$frequency,
        Percentage = paste0(df$percentage, "%"),
        Cumulative = paste0(round(df$cumulative, 1), "%"),
        Category = df$category
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
          columns = 1:5,
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
