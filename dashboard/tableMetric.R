# ---------- Frequency Table Module with Language Support ----------

tableMetricUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header Section
    div(
      style = "padding: 20px; margin-bottom: 18px;",
      uiOutput(ns("header"))
    ),
    
    # Statistics Row (Consolidated)
    uiOutput(ns("metricRow")),
    
    # Frequency and Heatmap (2 columns)
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      create_chart_card(ns, "chartTitle1", "chartDesc1", "freq", height = "350px"),
      create_chart_card(ns, "chartTitle4", "chartDesc4", "heatGrid", height = "350px")
    ),
    
    # Hot and Cold Numbers (2 columns)
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      create_chart_card(ns, "chartTitle2", "chartDesc2", "hotNumbers", height = "400px", style = "margin-top: 20px;"),
      create_chart_card(ns, "chartTitle3", "chartDesc3", "coldNumbers", height = "400px", style = "margin-top: 20px;")
    ),

    div(
      style = "margin-top: 20px;",
      bslib::accordion(
        id = ns("advancedInsightsAccordion"),
        open = FALSE,
        multiple = TRUE,
        bslib::accordion_panel(
          title = uiOutput(ns("advancedPanelTitle")),
          value = "table-advanced",
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            create_chart_card(ns, "chartTitle5", "chartDesc5", "freqDist", height = "320px"),
            create_chart_card(ns, "chartTitle6", "chartDesc6", "positionAnalysis", height = "320px")
          ),
          create_chart_card(ns, "chartTitle7", "chartDesc7", "deviation", height = "360px", style = "margin-top: 16px;"),
          div(
            class = "content-card",
            style = "margin-top: 16px;",
            div(class = "d-flex justify-content-between align-items-start",
                div(style = "flex-grow: 1;", uiOutput(ns("chartTitle8")))
            ),
            p(class = "info-text", uiOutput(ns("chartDesc8"))),
            div(class = "table-wrapper", DT::dataTableOutput(ns("freqTable")))
          )
        )
      )
    )
  )
}


tableMetricServer <- function(id, filtered_data, is_active = reactive(TRUE)) {
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
        h1(class = "header-title", t("table_title", lang)),
        p(class = "header-subtitle", t("table_subtitle", lang))
      )
    })
    
    # Chart titles
    output$chartTitle1 <- render_title("table_chart_frequencies", get_lang)
    output$chartDesc1 <- render_desc("table_chart_frequencies_desc", get_lang)
    output$chartTitle2 <- render_title("table_chart_hot", get_lang)
    output$chartDesc2 <- render_desc("table_chart_hot_desc", get_lang)
    output$chartTitle3 <- render_title("table_chart_cold", get_lang)
    output$chartDesc3 <- render_desc("table_chart_cold_desc", get_lang)
    output$chartTitle4 <- render_title("table_chart_grid", get_lang)
    output$chartDesc4 <- render_desc("table_chart_grid_desc", get_lang)
    output$chartTitle5 <- render_title("table_chart_dist", get_lang)
    output$chartDesc5 <- render_desc("table_chart_dist_desc", get_lang)
    output$chartTitle6 <- render_title("table_chart_position", get_lang)
    output$chartDesc6 <- render_desc("table_chart_position_desc", get_lang)
    output$chartTitle7 <- render_title("table_chart_deviation", get_lang)
    output$chartDesc7 <- render_desc("table_chart_deviation_desc", get_lang)
    output$chartTitle8 <- render_title("table_chart_table", get_lang)
    output$chartDesc8 <- render_desc("table_chart_table_desc", get_lang)
    output$advancedPanelTitle <- renderUI({
      t("advanced_insights_title", get_lang())
    })
    session$onFlushed(function() {
      lapply(c("metricRow", "freq", "hotNumbers", "coldNumbers", "heatGrid", "freqDist", "positionAnalysis", "deviation", "freqTable"), function(id) {
        try(outputOptions(output, id, suspendWhenHidden = TRUE), silent = TRUE)
      })
    }, once = TRUE)
    
    # Calculate frequency statistics
    freq_stats <- reactive({
      req(is_active())
      data <- filtered_data()
      nums <- as.vector(as.matrix(data[, paste0("ball_", 1:6)]))
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
    
    # Consolidated Metric Row
    output$metricRow <- renderUI({
      lang <- get_lang()
      stats <- freq_stats()
      range_val <- stats$max_freq - stats$min_freq
      
      create_metric_card <- function(title, value, value_symbol = "") {
        div(
          class = "metric-card",
          div(class = "metric-label", title),
          div(class = "metric-value", paste0(value, value_symbol))
        )
      }
      
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        create_metric_card(t("table_metric_hottest", lang), stats$most_common),
        create_metric_card(t("table_metric_coldest", lang), stats$least_common),
        create_metric_card(t("table_metric_expected", lang), round(stats$expected_freq, 1)),
        create_metric_card(t("table_metric_range", lang), range_val)
      )
    })
    
    # Main Frequency Chart
    output$freq <- renderPlotly({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      
      plot_ly(df, x = ~number, y = ~frequency, type = "bar",
              marker = list(
                color = ~frequency,
                colorscale = list(
                  c(0, "#4169E1"),
                  c(0.33, "#8b5cf6"),
                  c(0.66, "#ec4899"),
                  c(1, "#DC143C")
                ),
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 1),
                showscale = TRUE,
                colorbar = list(
                  title = t("table_label_frequency", lang),
                  titlefont = list(color = "#4f3d2d"),
                  tickfont = list(color = "#4f3d2d")
                )
              ),
              customdata = ~cbind(percentage, deviation),
              hovertemplate = paste0(
                "<b>", t("table_label_number", lang), ": %{x}</b><br>",
                t("table_label_frequency", lang), ": %{y}<br>",
                t("table_label_percentage", lang), ": %{customdata[0]}%<br>",
                t("table_label_deviation", lang), ": %{customdata[1]:.1f}<extra></extra>"
              )) %>%
        add_trace(x = c(min(df$number), max(df$number)), 
                  y = rep(stats$expected_freq, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = t("table_label_expected", lang),
                  hovertemplate = paste0(t("table_label_expected", lang), ": ", round(stats$expected_freq, 1), "<extra></extra>"),
                  showlegend = TRUE,
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_number", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            dtick = 1,
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_label_frequency", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            font = list(color = "#4f3d2d")
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Hot Numbers
    output$hotNumbers <- renderPlotly({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      hot <- df[order(-df$frequency), ][1:10, ]
      
      plot_ly(hot, x = ~reorder(number, frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#ff6b6b", "#DC143C"))(10),
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 2)
              ),
              text = ~frequency,
              textposition = "auto",
              textfont = list(color = "#4f3d2d", size = 12),
              customdata = ~deviation,
              hovertemplate = paste0(
                "<b>", t("table_label_number", lang), ": %{x}</b><br>",
                t("table_label_frequency", lang), ": %{y}<br>",
                t("table_label_deviation", lang), ": +%{customdata:.1f}<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_number", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_label_frequency", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Cold Numbers
    output$coldNumbers <- renderPlotly({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      cold <- df[order(df$frequency), ][1:10, ]
      
      plot_ly(cold, x = ~reorder(number, -frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#00f2fe", "#4facfe"))(10),
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 2)
              ),
              text = ~frequency,
              textposition = "auto",
              textfont = list(color = "#4f3d2d", size = 12),
              customdata = ~deviation,
              hovertemplate = paste0(
                "<b>", t("table_label_number", lang), ": %{x}</b><br>",
                t("table_label_frequency", lang), ": %{y}<br>",
                t("table_label_deviation", lang), ": %{customdata:.1f}<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_number", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_label_frequency", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Heat Grid
    # Example for tableMetric.R Heat Grid (line ~300):
    output$heatGrid <- renderPlotly({
      lang <- get_lang()
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

      
      n_rows <- nrow(mat)
      n_cols <- ncol(mat)
      
      # Get label text BEFORE plotly
      label_number <- as.character(t("table_label_number", lang))
      label_frequency <- as.character(t("table_label_frequency", lang))
      

      # Create annotation text
      anno_text <- as.vector(base::t(labels))
      
      plot_ly(z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
              colorscale = list(
                c(0, "rgba(79, 172, 254, 0.2)"),
                c(0.5, "rgba(139, 92, 246, 0.7)"),
                c(1, "rgba(236, 72, 153, 1)")
              ),
              text = labels,
              hovertemplate = paste0(
                "<b>", label_number, ": %{text}</b><br>",
                label_frequency, ": %{z}<br>",
                "<extra></extra>"
              ),
              showscale = TRUE,
              colorbar = list(
                title = label_frequency,
                titlefont = list(color = "#4f3d2d"),
                tickfont = list(color = "#4f3d2d")
              )) %>%
        add_annotations(
          x = rep(1:n_cols, each = n_rows),
          y = rep(1:n_rows, times = n_cols),
          text = anno_text,
          textfont = list(color = "#FFFFFF", size = 14, family = "Instrument Sans"),
          showarrow = FALSE,
          xref = "x",
          yref = "y"
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Frequency Distribution
    output$freqDist <- renderPlotly({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      
      plot_ly(x = ~df$frequency, type = "histogram",
              marker = list(
                color = "#8b5cf6",
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 1.5)
              ),
              nbinsx = 20,
              hovertemplate = paste0(
                t("table_label_frequency", lang), " Range: %{x}<br>",
                t("table_label_count", lang), ": %{y}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_frequency", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_label_numbers", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          bargap = 0.1
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Position Analysis
    output$positionAnalysis <- renderPlotly({
      lang <- get_lang()
      data <- filtered_data()
      
      # Calculate average frequency per position
      position_freq <- sapply(1:6, function(pos) {
        nums <- data[[paste0("ball_", pos)]]
        freq_table <- table(nums)
        mean(freq_table)
      })
      
      df <- data.frame(
        position = paste0(t("ball_label", lang), " ", 1:6),
        avg_freq = position_freq
      )
      
      ball_colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      
      plot_ly(df, x = ~position, y = ~avg_freq, type = "bar",
              marker = list(
                color = ball_colors,
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 2)
              ),
              text = ~round(avg_freq, 1),
              textposition = "auto",
              textfont = list(color = "#4f3d2d", size = 14),
              hovertemplate = paste0(
                "<b>%{x}</b><br>",
                t("table_label_avg_frequency", lang), ": %{y:.2f}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_ball_position", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_label_avg_frequency", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            color = "#4f3d2d"
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Deviation Chart
    output$deviation <- renderPlotly({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      
      df$color <- ifelse(df$deviation >= 0, "#10b981", "#ef4444")
      
      plot_ly(df, x = ~number, y = ~deviation, type = "bar",
              marker = list(
                color = ~color,
                line = list(color = "rgba(126, 95, 66, 0.28)", width = 1)
              ),
              customdata = ~deviation_pct,
              hovertemplate = paste0(
                "<b>", t("table_label_number", lang), ": %{x}</b><br>",
                t("table_label_deviation", lang), ": %{y:.2f}<br>",
                t("table_label_deviation_pct", lang), ": %{customdata}%<extra></extra>"
              )) %>%
        add_trace(x = c(min(df$number), max(df$number)), 
                  y = c(0, 0),
                  type = "scatter", mode = "lines",
                  line = list(color = "#4f3d2d", width = 2),
                  name = t("table_label_expected", lang),
                  hovertemplate = paste0(t("table_label_expected", lang), ": 0<extra></extra>"),
                  showlegend = FALSE,
                  inherit = FALSE) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#4f3d2d", family = "Instrument Sans"),
          xaxis = list(
            title = t("table_label_number", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            dtick = 1,
            color = "#4f3d2d"
          ),
          yaxis = list(
            title = t("table_chart_deviation", lang),
            gridcolor = "rgba(126, 95, 66, 0.18)",
            zeroline = TRUE,
            zerolinecolor = "rgba(126, 95, 66, 0.28)",
            color = "#4f3d2d"
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Data Table
    output$freqTable <- DT::renderDataTable({
      lang <- get_lang()
      stats <- freq_stats()
      df <- stats$freq_df
      
      df_display <- data.frame(
        df$number,
        df$frequency,
        paste0(df$percentage, "%"),
        round(df$deviation, 2),
        paste0(df$deviation_pct, "%"),
        check.names = FALSE
      )
      names(df_display) <- c(
        t("table_label_number", lang),
        t("table_label_frequency", lang),
        t("table_label_percentage", lang),
        t("table_label_deviation", lang),
        t("table_label_deviation_pct", lang)
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
            "$(this.api().table().container()).css({'background-color': 'rgba(255,255,255,0.05)', 'color': '#4f3d2d'});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          columns = 1:5,
          backgroundColor = 'rgba(255,255,255,0.02)',
          color = '#4f3d2d'
        )
    })
  })
}