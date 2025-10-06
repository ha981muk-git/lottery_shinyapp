

# ---------- Sum Analysis Module ----------

sumsMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "Sum Analysis"),
        p(class = "header-subtitle", "Analyze the sum of numbers in lottery draws and identify patterns")
      ),
      
      # Statistics Row
      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3")),
        uiOutput(ns("metricCard4")),
        uiOutput(ns("metricCard5"))
      ),
      
      # Distribution and Trend Row
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "content-card",
          div(class = "card-title", span("📊"), span("Sum Distribution")),
          p(class = "info-text", "Frequency distribution of sum values across all draws"),
          plotlyOutput(ns("hist"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("📈"), span("Sum Trend Over Time")),
          p(class = "info-text", "Track how sums change across consecutive draws"),
          plotlyOutput(ns("trend"), height = "400px")
        )
      ),
      
      # Range Analysis and Box Plot Row
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "content-card",
          div(class = "card-title", span("🎯"), span("Sum Range Analysis")),
          p(class = "info-text", "Distribution of sums across different ranges"),
          plotlyOutput(ns("rangeChart"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("📦"), span("Statistical Distribution")),
          p(class = "info-text", "Box plot showing quartiles, median, and outliers"),
          plotlyOutput(ns("boxPlot"), height = "400px")
        )
      ),
      # Moving Average and Volatility
      div(
        class = "content-card",
        style = "margin-top: 20px;",
        div(class = "card-title", span("📉"), span("Moving Average & Volatility")),
        p(class = "info-text", "20-draw moving average with upper and lower bands"),
        plotlyOutput(ns("movingAvg"), height = "400px")
      )
    )
  )
}

sumsMetricServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Metric Cards
    output$metricCard1 <- renderUI({
      stats <- sum_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📊"),
        div(class = "value-box-value", round(stats$mean, 1)),
        div(class = "value-box-label", "Average Sum")
      )
    })
    
    output$metricCard2 <- renderUI({
      stats <- sum_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "🎯"),
        div(class = "value-box-value", stats$median),
        div(class = "value-box-label", "Median Sum")
      )
    })
    
    output$metricCard3 <- renderUI({
      stats <- sum_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⭐"),
        div(class = "value-box-value", stats$most_common),
        div(class = "value-box-label", "Most Common")
      )
    })
    
    output$metricCard4 <- renderUI({
      stats <- sum_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📉"),
        div(class = "value-box-value", stats$min),
        div(class = "value-box-label", "Minimum Sum")
      )
    })
    
    output$metricCard5 <- renderUI({
      stats <- sum_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📈"),
        div(class = "value-box-value", stats$max),
        div(class = "value-box-label", "Maximum Sum")
      )
    })
    
    # Histogram
    output$hist <- renderPlotly({
      stats <- sum_stats()
      sums <- stats$sums
      
      # Create bins
      breaks <- seq(min(sums), max(sums) + 5, by = 5)
      
      plot_ly(x = ~sums, type = "histogram",
              marker = list(
                color = "#8b5cf6",
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)
              ),
              xbins = list(size = 5),
              hovertemplate = paste0(
                "Sum Range: %{x}<br>",
                "Frequency: %{y}<br>",
                "<extra></extra>"
              )) %>%
        add_trace(x = rep(stats$mean, 2), y = c(0, max(hist(sums, plot = FALSE)$counts) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#ec4899", width = 3, dash = "dash"),
                  name = "Mean",
                  hovertemplate = paste0("Mean: ", round(stats$mean, 1), "<extra></extra>")) %>%
        add_trace(x = rep(stats$median, 2), y = c(0, max(hist(sums, plot = FALSE)$counts) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dot"),
                  name = "Median",
                  hovertemplate = paste0("Median: ", stats$median, "<extra></extra>")) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Sum Value",
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
          bargap = 0.1
        )
    })
    
    # Trend Line
    output$trend <- renderPlotly({
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
                "Draw #%{x}<br>",
                "Sum: %{y}<br>",
                "<extra></extra>"
              )) %>%
        add_trace(x = range(df$draw), y = rep(stats$mean, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 2, dash = "dash"),
                  name = "Average",
                  hoverinfo = "skip") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Draw Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Sum Value",
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
                "Count: %{y}<br>",
                "Percentage: %{text}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Sum Range",
            gridcolor = "rgba(255, 255, 255, 0.1)",
            tickangle = -45
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          margin = list(b = 100)
        )
    })
    
    # Box Plot
    output$boxPlot <- renderPlotly({
      stats <- sum_stats()
      
      plot_ly(y = ~stats$sums, type = "box",
              marker = list(color = "#8b5cf6"),
              line = list(color = "#ec4899", width = 2),
              fillcolor = "rgba(139, 92, 246, 0.3)",
              name = "Sum Distribution",
              boxmean = "sd",
              hovertemplate = paste0(
                "Value: %{y}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          yaxis = list(
            title = "Sum Value",
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
        add_trace(y = ~sum, name = "Sum", type = "scatter", mode = "lines",
                  line = list(color = "rgba(139, 92, 246, 0.4)", width = 1),
                  hovertemplate = paste0("Draw: %{x}<br>Sum: %{y}<extra></extra>")) %>%
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
          add_trace(., y = ~ma, name = "Moving Avg", type = "scatter", mode = "lines",
                    line = list(color = "#10b981", width = 3),
                    hovertemplate = paste0("MA: %{y:.1f}<extra></extra>"))
          else .
        } %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Draw Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Sum Value",
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