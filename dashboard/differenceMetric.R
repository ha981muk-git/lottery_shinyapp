# ---------- Ball Range Analysis Module with Language Support ----------

differenceMetricUI <- function(id) {
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
      
      # ---------- Add to differenceMetricUI ----------
      
      # Add this section after the "Range Categories" div and before "Trend and Box Plot"
      
      # Density Distribution Analysis
      create_chart_card(ns, "chartTitle_density", "chartDesc_density", "densityDistribution", height = "450px", style = "margin-top: 25px;"),
      
      # Main Frequency Distribution
      create_chart_card(ns, "chartTitle1", "chartDesc1", "rangeFreq", height = "450px", style = "margin-top: 25px;"),
      
      # Hot and Cold Ranges
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        create_chart_card(ns, "chartTitle2", "chartDesc2", "hotRanges"),
        create_chart_card(ns, "chartTitle3", "chartDesc3", "coldRanges")
      ),
      
      # Range Categories
      create_chart_card(ns, "chartTitle4", "chartDesc4", "rangeCategories", height = "450px", style = "margin-top: 25px;"),
      
      # Trend and Box Plot
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        create_chart_card(ns, "chartTitle5", "chartDesc5", "rangeTrend"),
        create_chart_card(ns, "chartTitle6", "chartDesc6", "rangeBox")
      ),
      
      # Heatmap
      create_chart_card(ns, "chartTitle7", "chartDesc7", "rangeHeatmap", style = "margin-top: 25px;"),
      
      # Range Guide
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        uiOutput(ns("chartTitle8")),
        p(class = "info-text", uiOutput(ns("chartDesc8"))),
        div(style = "padding: 20px;",
            uiOutput(ns("rangeGuide")))
      ),
      
      # Table
      create_table_card(ns, "chartTitle9", "chartDesc9", "rangeTable", style = "margin-top: 25px;")
    )
  )
}

differenceMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title", t("difference_title", lang)),
        p(class = "header-subtitle", t("difference_subtitle", lang))
      )
    })
    
    # Chart titles and descriptions
    output$chartTitle1 <- render_title("difference_chart_freq", get_lang, "📊")
    output$chartDesc1 <- render_desc("difference_chart_freq_desc", get_lang)
    output$chartTitle2 <- render_title("difference_chart_hot", get_lang, "🔥")
    output$chartDesc2 <- render_desc("difference_chart_hot_desc", get_lang)
    output$chartTitle3 <- render_title("difference_chart_cold", get_lang, "❄️")
    output$chartDesc3 <- render_desc("difference_chart_cold_desc", get_lang)
    output$chartTitle4 <- render_title("difference_chart_categories", get_lang, "🎯")
    output$chartDesc4 <- render_desc("difference_chart_categories_desc", get_lang)
    output$chartTitle5 <- render_title("difference_chart_trend", get_lang, "📈")
    output$chartDesc5 <- render_desc("difference_chart_trend_desc", get_lang)
    output$chartTitle6 <- render_title("difference_chart_box", get_lang, "📦")
    output$chartDesc6 <- render_desc("difference_chart_box_desc", get_lang)
    output$chartTitle7 <- render_title("difference_chart_heatmap", get_lang, "🔥")
    output$chartDesc7 <- render_desc("difference_chart_heatmap_desc", get_lang)
    output$chartTitle8 <- render_title("difference_chart_guide", get_lang, "🎲")
    output$chartDesc8 <- render_desc("difference_chart_guide_desc", get_lang)
    output$chartTitle9 <- render_title("difference_chart_table", get_lang, "📋")
    output$chartDesc9 <- render_desc("difference_chart_table_desc", get_lang)
    
    # Calculate range statistics
    range_stats <- reactive({
      data <- filtered_data()
      
      ranges <- data$ball_6 - data$ball_1
      
      range_freq <- table(ranges)
      range_df <- data.frame(
        range = as.numeric(names(range_freq)),
        frequency = as.numeric(range_freq)
      )
      
      range_df$percentage <- round((range_df$frequency / sum(range_df$frequency)) * 100, 2)
      range_df$cumulative <- cumsum(range_df$percentage)
      
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
    
    # Consolidated Metric Row
    output$metricRow <- renderUI({
      lang <- get_lang()
      stats <- range_stats()
      
      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",
        gap = "12px",
        create_stat_card("📊", round(stats$mean, 1), t("difference_metric_avg", lang)),
        create_stat_card("🎯", stats$median, t("difference_metric_median", lang)),
        create_stat_card("⭐", stats$most_common, t("difference_metric_most_common", lang)),
        create_stat_card("📉", stats$min, t("difference_metric_min", lang)),
        create_stat_card("📈", stats$max, t("difference_metric_max", lang))
      )
    })
    
    # ---------- Add to differenceMetricServer ----------
    
    # Add these chart title/description outputs with your other chart titles:
    
    output$chartTitle_density <- render_title("difference_chart_density", get_lang, "📊")
    output$chartDesc_density <- render_desc("difference_chart_density_desc", get_lang)
    
    # Add this renderPlotly section with your other chart outputs:
    
    # Density Distribution
    output$densityDistribution <- renderPlotly({
      lang <- get_lang()
      stats <- range_stats()
      
      # Calculate difference (same as your code)
      diff <- stats$ranges
      
      # Remove NAs
      diff_clean <- diff[!is.na(diff)]
      
      # Create histogram data
      hist_data <- hist(diff_clean, breaks = 10, plot = FALSE)
      
      # Calculate density
      dens <- density(diff_clean)
      
      # Scale density to histogram height
      dens_scaled <- data.frame(
        x = dens$x,
        y = dens$y * length(diff_clean) * diff(hist_data$breaks)[1]
      )
      
      plot_ly() %>%
        add_histogram(
          x = diff_clean,
          nbinsx = 10,
          name = t("difference_label_histogram", lang),
          marker = list(
            color = "rgba(135, 206, 250, 0.7)",  # lightblue
            line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)
          ),
          hovertemplate = paste0(
            t("difference_label_difference", lang), ": %{x}<br>",
            t("difference_label_frequency", lang), ": %{y}<extra></extra>"
          )
        ) %>%
        add_lines(
          data = dens_scaled,
          x = ~x,
          y = ~y,
          name = t("difference_label_density", lang),
          line = list(color = "#DC143C", width = 3),  # red
          hovertemplate = paste0(
            t("difference_label_density", lang), ": %{y:.2f}<extra></extra>"
          )
        ) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("difference_label_difference", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("difference_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.15
          ),
          barmode = "overlay"
        )
    })
    
    # Range Frequency Chart
    output$rangeFreq <- renderPlotly({
      lang <- get_lang()
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
                  title = t("difference_label_frequency", lang),
                  titlefont = list(color = "#e8eaed"),
                  tickfont = list(color = "#e8eaed")
                )
              ),
              customdata = ~cbind(percentage, category),
              hovertemplate = paste0(
                "<b>", t("difference_label_range", lang), ": %{x}</b><br>",
                t("difference_label_frequency", lang), ": %{y}<br>",
                t("difference_hover_percentage", lang), ": %{customdata[0]}%<br>",
                t("difference_label_category", lang), ": %{customdata[1]}<extra></extra>"
              ),
              name = t("difference_label_frequency", lang)) %>%
        add_trace(x = c(stats$mean, stats$mean),
                  y = c(0, max(df$frequency) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = "Average",
                  hovertemplate = paste0("Average: ", round(stats$mean, 1), "<extra></extra>"),
                  showlegend = TRUE,
                  inherit = FALSE) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("difference_label_range", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("difference_label_frequency", lang),
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
      lang <- get_lang()
      stats <- range_stats()
      df <- stats$range_df
      hot <- df[order(-df$frequency), ][1:min(10, nrow(df)), ]
      
      plot_ly(hot, x = ~reorder(range, frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#ff6b6b", "#DC143C"))(nrow(hot)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " ", t("difference_hover_times", lang), " (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = paste0(
                "<b>", t("difference_label_range", lang), ": %{x}</b><br>",
                t("difference_label_frequency", lang), ": %{y}<br>",
                t("difference_hover_percentage", lang), ": %{text}<extra></extra>"
              )) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("difference_label_range", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("difference_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Cold Ranges
    output$coldRanges <- renderPlotly({
      lang <- get_lang()
      stats <- range_stats()
      df <- stats$range_df
      cold <- df[order(df$frequency), ][1:min(10, nrow(df)), ]
      
      plot_ly(cold, x = ~reorder(range, -frequency), y = ~frequency, type = "bar",
              marker = list(
                color = colorRampPalette(c("#00f2fe", "#4facfe"))(nrow(cold)),
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(frequency, " ", t("difference_hover_times", lang), " (", percentage, "%)"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = paste0(
                "<b>", t("difference_label_range", lang), ": %{x}</b><br>",
                t("difference_label_frequency", lang), ": %{y}<br>",
                t("difference_hover_percentage", lang), ": %{text}<extra></extra>"
              )) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("difference_label_range", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("difference_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          )
        )
    })
    
    # Range Categories
    output$rangeCategories <- renderPlotly({
      lang <- get_lang()
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
              hovertemplate = paste0(
                "<b>%{label}</b><br>",
                t("difference_label_frequency", lang), ": %{value}<br>",
                t("difference_hover_percentage", lang), ": %{text}<extra></extra>"
              )) %>%
        toWebGL() %>% layout(
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
      lang <- get_lang()
      stats <- range_stats()
      ranges <- stats$ranges
      
      df <- data.frame(
        draw = 1:length(ranges),
        range = ranges
      )
      
      window_size <- min(20, nrow(df))
      if(nrow(df) >= window_size) {
        df$ma <- zoo::rollmean(df$range, k = window_size, fill = NA, align = "right")
      }
      
      plot_ly(df, x = ~draw) %>%
        add_trace(y = ~range, name = t("difference_label_range", lang), type = "scatter", mode = "lines",
                  line = list(color = "rgba(139, 92, 246, 0.5)", width = 1.5),
                  hovertemplate = paste0(t("difference_label_range", lang), " #%{x}: %{y}<extra></extra>")) %>%
        toWebGL() %>%
        {if("ma" %in% names(df))
          add_trace(., y = ~ma, name = "Moving Avg", type = "scatter", mode = "lines",
                    line = list(color = "#10b981", width = 3),
                    hovertemplate = "MA: %{y:.1f}<extra></extra>")
          else .
        } %>%
        add_trace(x = c(min(df$draw), max(df$draw)),
                  y = rep(stats$mean, 2),
                  type = "scatter", mode = "lines",
                  line = list(color = "#ec4899", width = 2, dash = "dash"),
                  name = "Overall Avg",
                  hovertemplate = paste0("Average: ", round(stats$mean, 1), "<extra></extra>"),
                  inherit = FALSE) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = "Draw Number",
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("difference_label_range", lang),
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
      lang <- get_lang()
      stats <- range_stats()
      
      plot_ly(y = ~stats$ranges, type = "box",
              marker = list(color = "#8b5cf6"),
              line = list(color = "#ec4899", width = 2),
              fillcolor = "rgba(139, 92, 246, 0.3)",
              name = t("difference_label_range", lang),
              boxmean = "sd",
              hovertemplate = paste0(
                t("difference_label_range", lang), ": %{y}<extra></extra>"
              )) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          yaxis = list(
            title = t("difference_label_range", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          xaxis = list(
            title = "",
            showticklabels = FALSE
          )
        )
    })
    
    # Heatmap
    # Example for differenceMetric.R Range Heatmap (line ~584):
    output$rangeHeatmap <- renderPlotly({
      lang <- get_lang()
      stats <- range_stats()
      df <- stats$range_df
      
      n_cols <- 10
      n_rows <- ceiling(nrow(df) / n_cols)
      total_cells <- n_rows * n_cols
      
      padded_range <- c(df$range, rep(NA, total_cells - nrow(df)))
      padded_freq <- c(df$frequency, rep(0, total_cells - nrow(df)))
      
      mat <- matrix(padded_freq, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      labels <- matrix(padded_range, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      
      # Get label text BEFORE plotly
      label_range <- t("difference_label_range", lang)
      label_frequency <- t("difference_label_frequency", lang)
      
      # Create annotation text - convert NA to empty string
      anno_text <- as.character(as.vector(base::t(labels)))
      anno_text[is.na(anno_text)] <- ""
      
      plot_ly(z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
              colorscale = list(
                c(0, "rgba(79, 172, 254, 0.2)"),
                c(0.5, "rgba(139, 92, 246, 0.7)"),
                c(1, "rgba(236, 72, 153, 1)")
              ),
              text = labels,
              hovertemplate = paste0(
                "<b>", label_range, ": %{text}</b><br>",
                label_frequency, ": %{z}<br>",
                "<extra></extra>"
              ),
              showscale = TRUE,
              colorbar = list(
                title = label_frequency,
                titlefont = list(color = "#e8eaed"),
                tickfont = list(color = "#e8eaed")
              )) %>%
        add_annotations(
          x = rep(1:n_cols, each = n_rows),
          y = rep(1:n_rows, times = n_cols),
          text = anno_text,
          textfont = list(color = "#FFFFFF", size = 12, family = "Inter"),
          showarrow = FALSE,
          xref = "x",
          yref = "y"
        ) %>%
        toWebGL() %>% layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Range Selection Guide
    output$rangeGuide <- renderUI({
      lang <- get_lang()
      stats <- range_stats()
      df <- stats$range_df
      
      top_ranges <- df[order(-df$frequency), ][1:5, ]
      
      optimal_start <- round(stats$mean - stats$sd)
      optimal_end <- round(stats$mean + stats$sd)
      
      tagList(
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 20px;",
          
          # Top 5 Recommended Ranges
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #ec4899; margin-bottom: 15px;", paste0("🔥 ", t("difference_guide_top_hot", lang))),
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
                    paste0("(", top_ranges$frequency[i], " ", t("difference_hover_times", lang), ", ", top_ranges$percentage[i], "%)")
                  )
                )
              })
            )
          ),
          
          # Optimal Range
          div(
            class = "value-box-custom",
            style = "text-align: center;",
            h4(style = "color: #10b981; margin-bottom: 15px;", paste0("🎯 ", t("difference_label_optimal", lang))),
            div(
              style = "font-size: 32px; font-weight: bold; background: linear-gradient(135deg, #8b5cf6, #ec4899); -webkit-background-clip: text; -webkit-text-fill-color: transparent;",
              paste0(optimal_start, " - ", optimal_end)
            ),
            p(
              style = "margin-top: 10px; color: rgba(255,255,255,0.6);",
              t("difference_guide_optimal", lang)
            ),
            p(
              style = "color: rgba(255,255,255,0.8);",
              t("difference_guide_coverage", lang)
            )
          ),
          
          # Category Guide
          div(
            class = "value-box-custom",
            style = "text-align: left;",
            h4(style = "color: #FFD700; margin-bottom: 15px;", "📊 Category Guide"),
            div(
              style = "padding: 8px 0;",
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #4169E1; font-weight: bold;", paste0(t("difference_label_small", lang), ": ")),
                tags$span(style = "color: rgba(255,255,255,0.7);", t("difference_label_compact", lang))
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #8b5cf6; font-weight: bold;", paste0(t("difference_label_medium", lang), ": ")),
                tags$span(style = "color: rgba(255,255,255,0.7);", t("difference_label_balanced", lang))
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #ec4899; font-weight: bold;", paste0(t("difference_label_large", lang), ": ")),
                tags$span(style = "color: rgba(255,255,255,0.7);", t("difference_label_wide", lang))
              ),
              tags$div(
                style = "margin: 8px 0;",
                tags$span(style = "color: #DC143C; font-weight: bold;", paste0(t("difference_label_very_large", lang), ": ")),
                tags$span(style = "color: rgba(255,255,255,0.7);", t("difference_label_maximum", lang))
              )
            )
          )
        )
      )
    })
    
    # Range Table
    output$rangeTable <- DT::renderDataTable({
      lang <- get_lang()
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