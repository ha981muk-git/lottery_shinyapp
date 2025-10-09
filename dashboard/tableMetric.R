# ---------- Frequency Table Module ----------

tableMetricUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "padding: 20px;",
      gap = "12px",
      # Header
      div(
        style = "margin-bottom: 32px;",
        h1(class = "header-title", "Number Frequency Analysis"),
        p(class = "header-subtitle", "Comprehensive analysis of how often each number appears in lottery draws")
      ),
      
      # Statistics Row
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        gap = "15px",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3")),
        uiOutput(ns("metricCard4"))
      ),
      
      # Main Frequency Chart
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("📊"), span("Overall Number Frequencies")),
        p(class = "info-text", "How many times each number has been drawn"),
        plotlyOutput(ns("freq"), height = "450px")
      ),
      
      # Hot and Cold Numbers Row
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        div(
          class = "content-card",
          div(class = "card-title", span("🔥"), span("Hot Numbers")),
          p(class = "info-text", "Top 10 most frequently drawn numbers"),
          plotlyOutput(ns("hotNumbers"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("❄️"), span("Cold Numbers")),
          p(class = "info-text", "Top 10 least frequently drawn numbers"),
          plotlyOutput(ns("coldNumbers"), height = "400px")
        )
      ),
      
      # Interactive Visual Grid
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("🎯"), span("Interactive Number Grid")),
        p(class = "info-text", "Visual heatmap of all number frequencies - click on numbers to see details"),
        plotlyOutput(ns("heatGrid"), height = "500px")
      ),
      
      # Frequency Distribution & Ball Position Analysis
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        div(
          class = "content-card",
          div(class = "card-title", span("📈"), span("Frequency Distribution")),
          p(class = "info-text", "How frequencies are distributed across all numbers"),
          plotlyOutput(ns("freqDist"), height = "400px")
        ),
        div(
          class = "content-card",
          div(class = "card-title", span("🎲"), span("Ball Position Analysis")),
          p(class = "info-text", "Average frequency by ball position (1-6)"),
          plotlyOutput(ns("positionAnalysis"), height = "400px")
        )
      ),
      
      # Deviation from Expected Frequency
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("⚖️"), span("Deviation from Expected Frequency")),
        p(class = "info-text", "Numbers above/below their expected frequency (positive = drawn more often than expected)"),
        plotlyOutput(ns("deviation"), height = "450px")
      ),
      
      # Detailed Frequency Table
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(class = "card-title", span("📋"), span("Detailed Frequency Table")),
        p(class = "info-text", "Complete data table with sortable columns"),
        DT::dataTableOutput(ns("freqTable"))
      )
    )
  )
}


tableMetricServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate frequency statistics
    freq_stats <- reactive({
      data <- filtered_data()
      nums <- unlist(data[, paste0("ball_", 1:6)])
      freq_table <- table(nums)
      
      # Ensure all numbers 1-49 are represented
      all_nums <- 1:49
      freq_df <- data.frame(
        number = all_nums,
        frequency = sapply(all_nums, function(x) {
          if(as.character(x) %in% names(freq_table)) {
            freq_table[as.character(x)]
          } else {
            0
          }
        })
      )
      
      # Calculate statistics
      total_draws <- nrow(data)
      expected_freq <- (total_draws * 6) / 49
      
      freq_df$deviation <- freq_df$frequency - expected_freq
      freq_df$percentage <- round((freq_df$frequency / sum(freq_df$frequency)) * 100, 2)
      freq_df$deviation_pct <- round((freq_df$deviation / expected_freq) * 100, 1)
      
      list(
        freq_df = freq_df,
        total_draws = total_draws,
        expected_freq = expected_freq,
        most_common = freq_df$number[which.max(freq_df$frequency)],
        least_common = freq_df$number[which.min(freq_df$frequency)],
        max_freq = max(freq_df$frequency),
        min_freq = min(freq_df$frequency)
      )
    })
    
    # Metric Cards
    output$metricCard1 <- renderUI({
      stats <- freq_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "🔥"),
        div(class = "value-box-value", stats$most_common),
        div(class = "value-box-label", "Hottest Number")
      )
    })
    
    output$metricCard2 <- renderUI({
      stats <- freq_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "❄️"),
        div(class = "value-box-value", stats$least_common),
        div(class = "value-box-label", "Coldest Number")
      )
    })
    
    output$metricCard3 <- renderUI({
      stats <- freq_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📊"),
        div(class = "value-box-value", round(stats$expected_freq, 1)),
        div(class = "value-box-label", "Expected Frequency")
      )
    })
    
    output$metricCard4 <- renderUI({
      stats <- freq_stats()
      range_val <- stats$max_freq - stats$min_freq
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "📏"),
        div(class = "value-box-value", range_val),
        div(class = "value-box-label", "Frequency Range")
      )
    })
    
    # Main Frequency Chart
    output$freq <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      
      # Color based on frequency (gradient)
      colors <- colorRampPalette(c("#4169E1", "#8b5cf6", "#ec4899", "#DC143C"))(49)
      df <- df[order(df$frequency), ]
      df$color <- colors
      df <- df[order(df$number), ]
      
      plot_ly(df, x = ~number, y = ~frequency, type = "bar",
              marker = list(
                color = ~frequency,
                colorscale = list(
                  c(0, "#4169E1"),
                  c(0.33, "#8b5cf6"),
                  c(0.66, "#ec4899"),
                  c(1, "#DC143C")
                ),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1),
                showscale = TRUE,
                colorbar = list(
                  title = "Frequency",
                  titlefont = list(color = "#e8eaed"),
                  tickfont = list(color = "#e8eaed")
                )
              ),
              text = ~paste0(frequency, " (", percentage, "%)"),
              textposition = "none",
              customdata = ~cbind(percentage, deviation),
              hovertemplate = "<b>Number: %{x}</b><br>Frequency: %{y}<br>Percentage: %{customdata[0]}%<br>Deviation: %{customdata[1]:.1f}<extra></extra>",
              name = "Frequency") %>%
        add_trace(x = c(min(df$number), max(df$number)), 
                  y = rep(stats$expected_freq, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = "Expected",
                  hovertemplate = paste0("Expected: ", round(stats$expected_freq, 1), "<extra></extra>"),
                  showlegend = TRUE,
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Number",
            gridcolor = "rgba(255, 255, 255, 0.1)",
            dtick = 1
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
    
    # Hot Numbers
    output$hotNumbers <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      hot <- df[order(-df$frequency), ][1:10, ]
      
      plot_ly(hot, x = ~reorder(number, frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#ff6b6b", "#DC143C"))(10),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " times"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 12),
              customdata = ~deviation,
              hovertemplate = "<b>Number: %{x}</b><br>Frequency: %{y}<br>Deviation: +%{customdata:.1f}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Cold Numbers
    output$coldNumbers <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      cold <- df[order(df$frequency), ][1:10, ]
      
      plot_ly(cold, x = ~reorder(number, -frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#00f2fe", "#4facfe"))(10),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " times"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 12),
              customdata = ~deviation,
              hovertemplate = "<b>Number: %{x}</b><br>Frequency: %{y}<br>Deviation: %{customdata:.1f}<extra></extra>") %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Heat Grid
    output$heatGrid <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      
      # Create 7x7 grid
      mat <- matrix(0, nrow = 7, ncol = 7)
      labels <- matrix("", nrow = 7, ncol = 7)
      
      for(i in 1:49) {
        row <- ((i - 1) %/% 7) + 1
        col <- ((i - 1) %% 7) + 1
        mat[row, col] <- df$frequency[i]
        labels[row, col] <- as.character(i)
      }
      
      plot_ly(z = mat, x = 1:7, y = 1:7, type = "heatmap",
              colorscale = list(
                c(0, "rgba(79, 172, 254, 0.3)"),
                c(0.5, "rgba(139, 92, 246, 0.7)"),
                c(1, "rgba(236, 72, 153, 1)")
              ),
              text = labels,
              hovertemplate = paste0(
                "<b>Number: %{text}</b><br>",
                "Frequency: %{z}<br>",
                "<extra></extra>"
              ),
              showscale = TRUE,
              colorbar = list(
                title = "Frequency",
                titlefont = list(color = "#e8eaed"),
                tickfont = list(color = "#e8eaed")
              )) %>%
        add_annotations(
          x = rep(1:7, each = 7),
          y = rep(1:7, times = 7),
          text = as.vector(t(labels)),
          showarrow = FALSE,
          font = list(color = "#FFFFFF", size = 16, family = "Inter", weight = "bold")
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
    
    # Frequency Distribution
    output$freqDist <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      
      plot_ly(x = ~df$frequency, type = "histogram",
              marker = list(
                color = "#8b5cf6",
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)
              ),
              nbinsx = 20,
              hovertemplate = paste0(
                "Frequency Range: %{x}<br>",
                "Count: %{y}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Number of Numbers",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          bargap = 0.1
        )
    })
    
    # Position Analysis
    output$positionAnalysis <- renderPlotly({
      data <- filtered_data()
      
      # Calculate average frequency per position
      position_freq <- sapply(1:6, function(pos) {
        nums <- data[[paste0("ball_", pos)]]
        freq_table <- table(nums)
        mean(freq_table)
      })
      
      df <- data.frame(
        position = paste0("Ball ", 1:6),
        avg_freq = position_freq
      )
      
      ball_colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      
      plot_ly(df, x = ~position, y = ~avg_freq, type = "bar",
              marker = list(
                color = ball_colors,
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~round(avg_freq, 1),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 14),
              hovertemplate = paste0(
                "<b>%{x}</b><br>",
                "Avg Frequency: %{y:.2f}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Ball Position",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = "Average Frequency",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Deviation Chart
    output$deviation <- renderPlotly({
      stats <- freq_stats()
      df <- stats$freq_df
      
      df$color <- ifelse(df$deviation >= 0, "#10b981", "#ef4444")
      
      plot_ly(df, x = ~number, y = ~deviation, type = "bar",
              marker = list(
                color = ~color,
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 1)
              ),
              customdata = ~deviation_pct,
              hovertemplate = "<b>Number: %{x}</b><br>Deviation: %{y:.2f}<br>Percentage: %{customdata}%<extra></extra>",
              name = "Deviation") %>%
        add_trace(x = c(min(df$number), max(df$number)), 
                  y = c(0, 0),
                  type = "scatter", mode = "lines",
                  line = list(color = "#e8eaed", width = 2),
                  name = "Expected",
                  hovertemplate = "Expected: 0<extra></extra>",
                  showlegend = FALSE,
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Number",
            gridcolor = "rgba(255, 255, 255, 0.1)",
            dtick = 1
          ),
          yaxis = list(
            title = "Deviation from Expected",
            gridcolor = "rgba(255, 255, 255, 0.1)",
            zeroline = TRUE,
            zerolinecolor = "rgba(255, 255, 255, 0.3)"
          )
        )
    })
    
    # Data Table
    output$freqTable <- DT::renderDataTable({
      stats <- freq_stats()
      df <- stats$freq_df
      
      df_display <- data.frame(
        Number = df$number,
        Frequency = df$frequency,
        Percentage = paste0(df$percentage, "%"),
        Deviation = round(df$deviation, 2),
        `Deviation %` = paste0(df$deviation_pct, "%")
      )
      
      DT::datatable(
        df_display,
        options = list(
          pageLength = 10,
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
        )
    })
  })
}

