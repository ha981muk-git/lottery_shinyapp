# ---------- Odd/Even Analysis Module with Language Support ----------

oddsEvensMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      div(
        style = "margin-bottom: 32px;",
        uiOutput(ns("header"))
      ),
      
      # Statistics Row
      layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        uiOutput(ns("metricCard1")),
        uiOutput(ns("metricCard2")),
        uiOutput(ns("metricCard3"))
      ),
      
      # Pascal Triangle Distribution Card
      div(
        class = "content-card",
        uiOutput(ns("chartTitle1")),
        p(class = "info-text", uiOutput(ns("chartDesc1"))),
        plotlyOutput(ns("pascalChart"), height = "450px")
      ),
      
      # Charts Row
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        div(
          class = "content-card",
          uiOutput(ns("chartTitle2")),
          plotlyOutput(ns("pie"), height = "350px")
        ),
        div(
          class = "content-card",
          uiOutput(ns("chartTitle3")),
          plotlyOutput(ns("trendLine"), height = "350px")
        )
      ),
      
      # Stacked Bar Chart
      div(
        class = "content-card",
        style = "margin-top: 20px;",
        uiOutput(ns("chartTitle4")),
        plotlyOutput(ns("stacked"), height = "400px")
      )
    )
  )
}

oddsEvensMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title", t("odds_evens_title", lang)),
        p(class = "header-subtitle", t("odds_evens_subtitle", lang))
      )
    })
    
    # Chart titles
    output$chartTitle1 <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", span("📊"), span(t("odds_evens_chart_pascal", lang)))
    })
    
    output$chartDesc1 <- renderUI({
      lang <- get_lang()
      t("odds_evens_chart_pascal_desc", lang)
    })
    
    output$chartTitle2 <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", span("🥧"), span(t("odds_evens_chart_pie", lang)))
    })
    
    output$chartTitle3 <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", span("📈"), span(t("odds_evens_chart_trend", lang)))
    })
    
    output$chartTitle4 <- renderUI({
      lang <- get_lang()
      div(class = "chart-title", span("📊"), span(t("odds_evens_chart_stacked", lang)))
    })
    
    # Calculate odd/even statistics
    odds_evens_stats <- reactive({
      data <- filtered_data()
      odds <- rowSums(data[, paste0("ball_", 1:6)] %% 2 == 1)
      evens <- 6 - odds
      
      # Create combination labels
      combinations <- paste0(odds, " Odds / ", evens, " Evens")
      
      list(
        odds = odds,
        evens = evens,
        combinations = combinations,
        total_odds = sum(odds),
        total_evens = sum(evens),
        avg_odds = mean(odds),
        most_common = names(sort(table(combinations), decreasing = TRUE))[1]
      )
    })
    
    # Metric Cards
    output$metricCard1 <- renderUI({
      lang <- get_lang()
      stats <- odds_evens_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "🎲"),
        div(class = "value-box-value", round(stats$avg_odds, 2)),
        div(class = "value-box-label", t("odds_evens_metric_avg_odds", lang))
      )
    })
    
    output$metricCard2 <- renderUI({
      lang <- get_lang()
      stats <- odds_evens_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⚖️"),
        div(class = "value-box-value", round(6 - stats$avg_odds, 2)),
        div(class = "value-box-label", t("odds_evens_metric_avg_evens", lang))
      )
    })
    
    output$metricCard3 <- renderUI({
      lang <- get_lang()
      stats <- odds_evens_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⭐"),
        div(class = "value-box-value", style = "font-size: 20px;", stats$most_common),
        div(class = "value-box-label", t("odds_evens_metric_most_common", lang))
      )
    })
    
    # Pascal Triangle Distribution Chart
    output$pascalChart <- renderPlotly({
      lang <- get_lang()
      stats <- odds_evens_stats()
      
      # Count frequency of each combination
      combo_counts <- table(stats$combinations)
      combo_df <- data.frame(
        combination = names(combo_counts),
        count = as.numeric(combo_counts)
      )
      
      # Extract odds count for ordering
      combo_df$odds_num <- as.numeric(sub(" Odds.*", "", combo_df$combination))
      combo_df <- combo_df[order(combo_df$odds_num), ]
      
      # Create color palette - gradient from blue (all evens) to red (all odds)
      colors <- c("#00CED1", "#4169E1", "#9370DB", "#8b5cf6", "#ec4899", "#DC143C", "#FFD700")
      
      # Calculate percentage
      combo_df$percentage <- round(combo_df$count / sum(combo_df$count) * 100, 1)
      
      plot_ly(combo_df, 
              x = ~reorder(combination, odds_num), 
              y = ~count,
              type = "bar",
              marker = list(
                color = colors,
                line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
              ),
              text = ~paste0(count, " ", t("odds_evens_hover_draws", lang), "<br>", percentage, "%"),
              textposition = "outside",
              textfont = list(color = "#e8eaed", size = 14, family = "Inter"),
              hovertemplate = paste0(
                "<b>%{x}</b><br>",
                t("odds_evens_label_frequency", lang), ": %{y} ", t("odds_evens_hover_draws", lang), "<br>",
                t("odds_evens_hover_percentage", lang), ": %{text}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("odds_evens_label_combination", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            tickangle = -45,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = t("odds_evens_label_frequency", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          margin = list(b = 100, t = 40)
        )
    })
    
    # Pie Chart
    output$pie <- renderPlotly({
      lang <- get_lang()
      stats <- odds_evens_stats()
      df <- data.frame(
        category = c(t("odds_evens_label_odds", lang), t("odds_evens_label_evens", lang)),
        count = c(stats$total_odds, stats$total_evens)
      )
      
      plot_ly(df, 
              labels = ~category, 
              values = ~count, 
              type = "pie",
              marker = list(colors = c("#ec4899", "#4169E1"),
                            line = list(color = "#FFFFFF", width = 2)),
              textinfo = "label+percent",
              textfont = list(size = 16, color = "#FFFFFF", family = "Inter"),
              hovertemplate = paste0(
                "<b>%{label}</b><br>",
                t("odds_evens_label_count", lang), ": %{value}<br>",
                t("odds_evens_hover_percentage", lang), ": %{percent}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.1
          )
        )
    })
    
    # Trend Line Chart
    output$trendLine <- renderPlotly({
      lang <- get_lang()
      stats <- odds_evens_stats()
      data <- filtered_data()
      
      df <- data.frame(
        draw = 1:length(stats$odds),
        odds = stats$odds,
        evens = stats$evens
      )
      
      # Add moving average
      window_size <- min(20, nrow(df))
      if(nrow(df) >= window_size) {
        df$ma_odds <- zoo::rollmean(df$odds, k = window_size, fill = NA, align = "right")
      }
      
      plot_ly(df, x = ~draw) %>%
        add_trace(y = ~odds, name = t("odds_evens_label_odds", lang), type = "scatter", mode = "lines",
                  line = list(color = "#ec4899", width = 2),
                  hovertemplate = paste0(t("odds_evens_label_draw_number", lang), ": %{x}<br>", t("odds_evens_label_odds", lang), ": %{y}<extra></extra>")) %>%
        add_trace(y = ~evens, name = t("odds_evens_label_evens", lang), type = "scatter", mode = "lines",
                  line = list(color = "#4169E1", width = 2),
                  hovertemplate = paste0(t("odds_evens_label_draw_number", lang), ": %{x}<br>", t("odds_evens_label_evens", lang), ": %{y}<extra></extra>")) %>%
        {if("ma_odds" %in% names(df)) 
          add_trace(., y = ~ma_odds, name = paste("MA (", t("odds_evens_label_odds", lang), ")"), type = "scatter", mode = "lines",
                    line = list(color = "#8b5cf6", width = 3, dash = "dash"),
                    hovertemplate = paste0(t("odds_evens_label_draw_number", lang), ": %{x}<br>MA: %{y:.2f}<extra></extra>"))
          else .
        } %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("odds_evens_label_draw_number", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("odds_evens_label_count", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            range = c(0, 6)
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
    
    # Stacked Bar Chart
    output$stacked <- renderPlotly({
      lang <- get_lang()
      stats <- odds_evens_stats()
      data <- filtered_data()
      
      # Show last 50 draws or all if less
      n_draws <- min(50, nrow(data))
      start_idx <- max(1, length(stats$odds) - n_draws + 1)
      
      df <- data.frame(
        draw = start_idx:length(stats$odds),
        odds = stats$odds[start_idx:length(stats$odds)],
        evens = stats$evens[start_idx:length(stats$evens)]
      )
      
      plot_ly(df, x = ~draw) %>%
        add_trace(y = ~odds, name = t("odds_evens_label_odds", lang), type = "bar",
                  marker = list(color = "#ec4899"),
                  hovertemplate = paste0(t("odds_evens_label_draw_number", lang), ": %{x}<br>", t("odds_evens_label_odds", lang), ": %{y}<extra></extra>")) %>%
        add_trace(y = ~evens, name = t("odds_evens_label_evens", lang), type = "bar",
                  marker = list(color = "#4169E1"),
                  hovertemplate = paste0(t("odds_evens_label_draw_number", lang), ": %{x}<br>", t("odds_evens_label_evens", lang), ": %{y}<extra></extra>")) %>%
        layout(
          barmode = "stack",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          font = list(color = "#e8eaed", family = "Inter"),
          xaxis = list(
            title = t("odds_evens_label_draw_number", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          yaxis = list(
            title = t("odds_evens_label_count", lang),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            range = c(0, 6)
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