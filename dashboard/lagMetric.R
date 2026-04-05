# ---------- Lag Analysis Module UI with Language Support (COMPLETE) ----------

lagMetricUI <- function(id) {
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
      
      # Ball Position Selector
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        div(
          style = "margin-bottom: 20px;",
          h4(style = "color: #8b5cf6;", uiOutput(ns("selectorTitle"))),
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
            actionButton(ns("ballAll"), uiOutput(ns("allBallsText")), class = "btn-action btn-success")
          )
        ),
        div(
          style = "text-align: center; margin-top: 15px;",
          uiOutput(ns("selectedBall"))
        )
      ),
      
      # Distribution Chart
      create_chart_card(ns, "chartTitle1", "chartDesc1", "lagDistribution", height = "500px", style = "margin-top: 25px;"),
      
      # Jump Preference Charts
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        create_chart_card(ns, "chartTitle2", "chartDesc2", "positiveJumps"),
        create_chart_card(ns, "chartTitle3", "chartDesc3", "negativeJumps")
      ),
      
      # Jump Categories
      create_chart_card(ns, "chartTitle4", "chartDesc4", "jumpCategories", height = "450px", style = "margin-top: 25px;"),
      
      # Heatmap and Q-Q Plot
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        gap = "20px",
        fill = FALSE,
        create_chart_card(ns, "chartTitle5", "chartDesc5", "lagHeatmap", height = "450px"),
        create_chart_card(ns, "chartTitle6", "chartDesc6", "qqPlot", height = "450px")
      ),
      
      # Preferred Zones
      create_chart_card(ns, "chartTitle7", "chartDesc7", "preferredZones", height = "450px", style = "margin-top: 25px;"),
      
      # Statistical Summary
      div(
        class = "content-card",
        style = "margin-top: 25px;",
        uiOutput(ns("chartTitle8")),
        uiOutput(ns("statSummary"))
      ),
      
      # Table
      create_table_card(ns, "chartTitle9", "chartDesc9", "lagTable", style = "margin-top: 25px;")
    )
  )
}

lagMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title", t("lag_title", lang)),
        p(class = "header-subtitle", t("lag_subtitle", lang))
      )
    })
    
    # Render selector title
    output$selectorTitle <- renderUI({
      lang <- get_lang()
      t("lag_selector_title", lang)
    })
    
    # Chart titles
    output$chartTitle1 <- render_title("lag_chart_distribution", get_lang, "📊")
    output$chartDesc1 <- render_desc("lag_chart_distribution_desc", get_lang)
    output$chartTitle2 <- render_title("lag_chart_positive", get_lang, "⬆️")
    output$chartDesc2 <- render_desc("lag_chart_positive_desc", get_lang)
    output$chartTitle3 <- render_title("lag_chart_negative", get_lang, "⬇️")
    output$chartDesc3 <- render_desc("lag_chart_negative_desc", get_lang)
    output$chartTitle4 <- render_title("lag_chart_categories", get_lang, "🎯")
    output$chartDesc4 <- render_desc("lag_chart_categories_desc", get_lang)
    output$chartTitle5 <- render_title("lag_chart_heatmap", get_lang, "🔥")
    output$chartDesc5 <- render_desc("lag_chart_heatmap_desc", get_lang)
    output$chartTitle6 <- render_title("lag_chart_qq", get_lang, "📈")
    output$chartDesc6 <- render_desc("lag_chart_qq_desc", get_lang)
    output$chartTitle7 <- render_title("lag_chart_zones", get_lang, "🎲")
    output$chartDesc7 <- render_desc("lag_chart_zones_desc", get_lang)
    output$chartTitle8 <- render_title("lag_chart_summary", get_lang, "📊")
    output$chartTitle9 <- render_title("lag_chart_table", get_lang, "📋")
    output$chartDesc9 <- render_desc("lag_chart_table_desc", get_lang)
    
    # Track selected ball
    selected_ball <- reactiveVal(0)
    
    observeEvent(input$ball1, { selected_ball(1) })
    observeEvent(input$ball2, { selected_ball(2) })
    observeEvent(input$ball3, { selected_ball(3) })
    observeEvent(input$ball4, { selected_ball(4) })
    observeEvent(input$ball5, { selected_ball(5) })
    observeEvent(input$ball6, { selected_ball(6) })
    observeEvent(input$ballAll, { selected_ball(0) })
    
    # Display selected ball
    output$selectedBall <- renderUI({
      lang <- get_lang()
      ball <- selected_ball()
      if(is.null(ball)) ball <- 0
      
      ball_colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      text <- if(ball == 0) t("lag_selector_all", lang) else paste0("Ball ", ball)
      color <- if(ball == 0) "#8b5cf6" else ball_colors[ball]
      
      div(
        style = paste0("font-size: 24px; font-weight: bold; color: ", color, ";"),
        paste0(t("lag_selector_analyzing", lang), ": ", text)
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
      
      if(ball == 0) {
        all_lags <- c()
        for(b in 1:6) {
          col <- data[[paste0("ball_", b)]]
          lags <- diff(col)
          all_lags <- c(all_lags, lags)
        }
        lags <- all_lags
      } else {
        col <- data[[paste0("ball_", ball)]]
        lags <- diff(col)
      }
      
      lag_table <- table(lags)
      lag_df <- data.frame(
        lag = as.numeric(names(lag_table)),
        frequency = as.numeric(lag_table)
      )
      
      lag_df$percentage <- round((lag_df$frequency / sum(lag_df$frequency)) * 100, 2)
      lag_df$probability <- lag_df$frequency / sum(lag_df$frequency)
      
      lag_df$category <- cut(abs(lag_df$lag),
                             breaks = c(0, 3, 7, 15, Inf),
                             labels = c("Tiny (0-3)", "Small (4-7)", "Medium (8-15)", "Large (>15)"),
                             include.lowest = TRUE)
      
      lag_df$direction <- ifelse(lag_df$lag > 0, t("lag_label_increase", get_lang()),
                                 ifelse(lag_df$lag < 0, t("lag_label_decrease", get_lang()), t("lag_label_no_change", get_lang())))
      
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
    
    # Consolidated Metric Row
    output$metricRow <- renderUI({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      range_val <- stats$max - stats$min
      
      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        gap = "15px",
        create_stat_card("📊", round(stats$mean, 2), t("lag_metric_avg", lang)),
        create_stat_card("📏", round(stats$sd, 2), t("lag_metric_sd", lang)),
        create_stat_card("⭐", stats$most_common, t("lag_metric_most_common", lang)),
        create_stat_card("📈", range_val, t("lag_metric_range", lang))
      )
    })
    
    # Lag Distribution
    output$lagDistribution <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      hist_data <- hist(stats$lags, breaks = 30, plot = FALSE)
      x_seq <- seq(min(stats$lags), max(stats$lags), length.out = 100)
      y_norm <- dnorm(x_seq, mean = stats$mean, sd = stats$sd)
      y_norm_scaled <- y_norm * length(stats$lags) * diff(hist_data$breaks[1:2])
      
      plot_ly() %>%
        add_bars(x = hist_data$mids, y = hist_data$counts,
                 marker = list(color = "#8b5cf6", line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)),
                 name = t("lag_chart_actual", lang),
                 hovertemplate = paste0(t("lag_label_lag", lang), ": %{x}<br>", t("lag_label_frequency", lang), ": %{y}<extra></extra>")) %>%
        add_lines(x = x_seq, y = y_norm_scaled,
                  line = list(color = "#ec4899", width = 3),
                  name = t("lag_chart_normal", lang),
                  hovertemplate = paste0(t("lag_label_lag", lang), ": %{x:.1f}<br>", t("lag_hover_expected", lang), ": %{y:.1f}<extra></extra>")) %>%
        add_trace(x = c(stats$mean, stats$mean), y = c(0, max(hist_data$counts) * 1.1),
                  type = "scatter", mode = "lines",
                  line = list(color = "#10b981", width = 3, dash = "dash"),
                  name = t("lag_chart_mean", lang),
                  hovertemplate = paste0(t("lag_chart_mean", lang), ": ", round(stats$mean, 2), "<extra></extra>"),
                  inherit = FALSE) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_lag", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_frequency", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
               bargap = 0.05)
    })
    
    # Positive Jumps
    output$positiveJumps <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      positive <- df[df$lag > 0, ]
      if(nrow(positive) == 0) return(NULL)
      
      positive <- positive[order(-positive$frequency), ][1:min(15, nrow(positive)), ]
      
      plot_ly(positive, x = ~reorder(lag, frequency), y = ~frequency, type = "bar",
              marker = list(color = colorRampPalette(c("#32CD32", "#10b981"))(nrow(positive)),
                            line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)),
              text = ~paste0(frequency, " (", percentage, "%)"),
              textposition = "outside", textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = paste0("<b>", t("lag_hover_jump", lang), ": +%{x}</b><br>", t("lag_label_frequency", lang), ": %{y}<br>", t("lag_label_percentage", lang), ": %{text}<extra></extra>")) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_jump_size", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_frequency", lang), gridcolor = "rgba(255, 255, 255, 0.1)"))
    })
    
    # Negative Jumps
    output$negativeJumps <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      negative <- df[df$lag < 0, ]
      if(nrow(negative) == 0) return(NULL)
      
      negative <- negative[order(-negative$frequency), ][1:min(15, nrow(negative)), ]
      
      plot_ly(negative, x = ~reorder(lag, -frequency), y = ~frequency, type = "bar",
              marker = list(color = colorRampPalette(c("#ef4444", "#DC143C"))(nrow(negative)),
                            line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)),
              text = ~paste0(frequency, " (", percentage, "%)"),
              textposition = "outside", textfont = list(color = "#e8eaed", size = 11),
              hovertemplate = paste0("<b>", t("lag_hover_jump", lang), ": %{x}</b><br>", t("lag_label_frequency", lang), ": %{y}<br>", t("lag_label_percentage", lang), ": %{text}<extra></extra>")) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_jump_size", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_frequency", lang), gridcolor = "rgba(255, 255, 255, 0.1)"))
    })
    
    # Jump Categories
    output$jumpCategories <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      category_summary <- aggregate(frequency ~ category + direction, data = df, sum)
      
      plot_ly() %>%
        {
          p <- .
          for(dir in unique(category_summary$direction)) {
            data_dir <- category_summary[category_summary$direction == dir, ]
            color <- if(dir == t("lag_label_increase", lang)) "#10b981" else if(dir == t("lag_label_decrease", lang)) "#ef4444" else "#8b5cf6"
            p <- add_trace(p, data = data_dir, x = ~category, y = ~frequency, type = "bar", name = dir,
                           marker = list(color = color),
                           hovertemplate = paste0("<b>", dir, " - %{x}</b><br>", t("lag_label_frequency", lang), ": %{y}<extra></extra>"))
          }
          p
        } %>%
        layout(barmode = "stack", paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_jump_category", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_frequency", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
    })
    
    # Lag Heatmap
    # Example for lagMetric.R Lag Heatmap (line ~506):
    output$lagHeatmap <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      n_cols <- 10
      n_rows <- ceiling(nrow(df) / n_cols)
      total_cells <- n_rows * n_cols
      
      padded_lag <- c(df$lag, rep(NA, total_cells - nrow(df)))
      padded_freq <- c(df$frequency, rep(0, total_cells - nrow(df)))
      
      mat <- matrix(padded_freq, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      labels <- matrix(padded_lag, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      
      # Get label text BEFORE plotly
      label_lag <- t("lag_label_lag", lang)
      label_frequency <- t("lag_label_frequency", lang)
      
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
                "<b>", label_lag, ": %{text}</b><br>",
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
          textfont = list(color = "#FFFFFF", size = 10, family = "Inter"),
          showarrow = FALSE,
          xref = "x",
          yref = "y"
        ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Q-Q Plot
    output$qqPlot <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      theoretical <- qqnorm(stats$lags, plot.it = FALSE)
      
      plot_ly() %>%
        add_markers(x = theoretical$x, y = theoretical$y, marker = list(color = "#8b5cf6", size = 6),
                    name = t("lag_chart_data_points", lang),
                    hovertemplate = paste0(t("lag_label_theoretical", lang), ": %{x:.2f}<br>", t("lag_label_sample", lang), ": %{y:.2f}<extra></extra>")) %>%
        toWebGL() %>%
        add_lines(x = range(theoretical$x), y = range(theoretical$x),
                  line = list(color = "#ec4899", width = 3, dash = "dash"),
                  name = t("lag_chart_perfect_normal", lang),
                  hovertemplate = paste0(t("lag_chart_perfect_normal", lang), "<extra></extra>")) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_theoretical", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_sample", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
    })
    
    # Preferred Zones
    output$preferredZones <- renderPlotly({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(NULL)
      
      df <- stats$lag_df
      df <- df[order(df$lag), ]
      df$zone <- ifelse(df$percentage >= 2, "Hot Zone",
                        ifelse(df$percentage >= 1, "Warm Zone",
                               ifelse(df$percentage >= 0.5, "Cool Zone", "Cold Zone")))
      
      zone_colors <- c("Hot Zone" = "#DC143C", "Warm Zone" = "#ff6b6b", "Cool Zone" = "#4facfe", "Cold Zone" = "#4169E1")
      
      plot_ly(df, x = ~lag, y = ~percentage, type = "bar",
              marker = list(color = ~zone, colors = zone_colors, line = list(color = "rgba(255, 255, 255, 0.3)", width = 1)),
              text = ~zone,
              hovertemplate = paste0("<b>", t("lag_label_lag", lang), ": %{x}</b><br>", t("lag_label_percentage", lang), ": %{y}%<br>Zone: %{text}<extra></extra>")) %>%
        layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = "#e8eaed", family = "Inter"),
               xaxis = list(title = t("lag_label_lag_value", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               yaxis = list(title = t("lag_label_percentage", lang), gridcolor = "rgba(255, 255, 255, 0.1)"),
               showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
    })
    
    # Statistical Summary
    output$statSummary <- renderUI({
      lang <- get_lang()
      stats <- lag_stats()
      if(length(stats$lags) == 0) return(p("No data available"))
      
      df <- stats$lag_df
      hot_lags <- df[order(-df$frequency), ][1:min(5, nrow(df)), ]
      ci_lower <- stats$mean - 1.96 * stats$sd
      ci_upper <- stats$mean + 1.96 * stats$sd
      shapiro_result <- if(length(stats$lags) >= 3 && length(stats$lags) <= 5000) shapiro.test(stats$lags)$p.value else NA
      
      tagList(
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; padding: 20px;",
          div(class = "value-box-custom", style = "text-align: left;",
              h4(style = "color: #ec4899; margin-bottom: 15px;", paste0("🔥 ", t("lag_summary_top_jumps", lang))),
              tags$ul(
                style = "list-style: none; padding: 0;",
                lapply(1:nrow(hot_lags), function(i) {
                  tags$li(
                    style = "padding: 8px 0; border-bottom: 1px solid rgba(255,255,255,0.1);",
                    tags$span(style = "font-size: 18px; font-weight: bold; color: #8b5cf6;", hot_lags$lag[i]),
                    tags$span(style = "margin-left: 15px; color: rgba(255,255,255,0.7);", paste0("(", hot_lags$percentage[i], "%)"))
                  )
                })
              )
          ),
          div(class = "value-box-custom", style = "text-align: left;",
              h4(style = "color: #10b981; margin-bottom: 15px;", paste0("📊 ", t("lag_summary_statistical", lang))),
              div(style = "line-height: 1.8;",
                  div(tags$strong(paste0(t("lag_summary_ci", lang), ": ")), tags$span(paste0("[", round(ci_lower, 1), ", ", round(ci_upper, 1), "]"))),
                  div(tags$strong(paste0(t("lag_summary_expected", lang), ": ")), tags$span(paste0(round(stats$mean - stats$sd, 1), " to ", round(stats$mean + stats$sd, 1)))),
                  div(tags$strong(paste0(t("lag_summary_normality", lang), ": ")), tags$span(
                    if(!is.na(shapiro_result)) {
                      if(shapiro_result > 0.05) t("lag_summary_follows", lang) else t("lag_summary_deviates", lang)
                    } else "Test not applicable"
                  ))
              )
          ),
          div(class = "value-box-custom", style = "text-align: left;",
              h4(style = "color: #FFD700; margin-bottom: 15px;", paste0("💡 ", t("lag_summary_recommendations", lang))),
              div(style = "line-height: 1.8; color: rgba(255,255,255,0.8);",
                  div(paste0("✓ ", t("lag_summary_rec_hot", lang))),
                  div(paste0("✓ ", t("lag_summary_rec_within", lang), " ±", round(stats$sd, 1), " ", t("lag_summary_rec_of_mean", lang))),
                  div(paste0("✓ ", t("lag_summary_rec_avoid", lang))),
                  div(paste0("✓ ", t("lag_summary_rec_likely", lang), ": ", stats$most_common))
              )
          )
        )
      )
    })
    
    # Lag Table
    output$lagTable <- DT::renderDataTable({
      lang <- get_lang()
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