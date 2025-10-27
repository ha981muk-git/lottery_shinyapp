# =============================================================================
# Frequency Table Module — Performance Edition (Global Cache + Empty State)
# =============================================================================

# -----------------------------------------------------------------------------
# OPTIONAL: Global cache invalidation helper for the hosting app.
# Call this after your data source refreshes (e.g., nightly ETL).
# Or set options(metrics_data_version = "<new-version>") to rotate namespaces.
# -----------------------------------------------------------------------------
table_clear_cache <- function() {
  if (exists(".table_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".table_plot_cache", envir = .GlobalEnv, inherits = FALSE)
  }
  invisible(TRUE)
}

# Global cache env (shared between users)
.table_get_cache_env <- function() {
  if (!exists(".table_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".table_plot_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".table_plot_cache", envir = .GlobalEnv, inherits = FALSE)
}

.table_cache_get <- function(key) {
  env <- .table_get_cache_env()
  if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL
}

.table_cache_set <- function(key, value) {
  env <- .table_get_cache_env()
  assign(key, value, envir = env)
  invisible(value)
}

# -----------------------------------------------------------------------------
# UI — uses conditionalPanel via output$hasData to show charts or empty state
# -----------------------------------------------------------------------------
tableMetricUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "padding: 20px;",
      
      # Header
      div(style = "margin-bottom: 32px;", uiOutput(ns("header"))),
      
      # -------------------- WHEN DATA EXISTS ----------------------------------
      conditionalPanel(
        condition = sprintf("output['%s']", ns("hasData")),
        
        # Statistics Row - 4 Metric Cards
        layout_column_wrap(
          width = 1/4,
          heights_equal = "row",
          uiOutput(ns("metricCard1")),
          uiOutput(ns("metricCard2")),
          uiOutput(ns("metricCard3")),
          uiOutput(ns("metricCard4"))
        ),
        
        # Main Frequency Chart
        div(
          class = "content-card",
          style = "margin-top: 20px;",
          uiOutput(ns("chartTitle1")),
          p(class = "info-text", uiOutput(ns("chartDesc1"))),
          plotlyOutput(ns("freq"), height = "450px")
        ),
        
        # Hot and Cold Numbers Row
        layout_column_wrap(
          width = 1/2,
          heights_equal = "row",
          div(
            class = "content-card",
            style = "margin-top: 20px;",
            uiOutput(ns("chartTitle2")),
            p(class = "info-text", uiOutput(ns("chartDesc2"))),
            plotlyOutput(ns("hotNumbers"), height = "400px")
          ),
          div(
            class = "content-card",
            style = "margin-top: 20px;",
            uiOutput(ns("chartTitle3")),
            p(class = "info-text", uiOutput(ns("chartDesc3"))),
            plotlyOutput(ns("coldNumbers"), height = "400px")
          )
        ),
        
        # Interactive Visual Grid
        div(
          class = "content-card",
          style = "margin-top: 20px;",
          uiOutput(ns("chartTitle4")),
          p(class = "info-text", uiOutput(ns("chartDesc4"))),
          plotlyOutput(ns("heatGrid"), height = "500px")
        ),
        
        # Frequency Distribution & Ball Position Analysis
        layout_column_wrap(
          width = 1/2,
          heights_equal = "row",
          div(
            class = "content-card",
            style = "margin-top: 20px;",
            uiOutput(ns("chartTitle5")),
            p(class = "info-text", uiOutput(ns("chartDesc5"))),
            plotlyOutput(ns("freqDist"), height = "400px")
          ),
          div(
            class = "content-card",
            style = "margin-top: 20px;",
            uiOutput(ns("chartTitle6")),
            p(class = "info-text", uiOutput(ns("chartDesc6"))),
            plotlyOutput(ns("positionAnalysis"), height = "400px")
          )
        ),
        
        # Deviation from Expected Frequency
        div(
          class = "content-card",
          style = "margin-top: 20px;",
          uiOutput(ns("chartTitle7")),
          p(class = "info-text", uiOutput(ns("chartDesc7"))),
          plotlyOutput(ns("deviation"), height = "450px")
        ),
        
        # Detailed Frequency Table
        div(
          class = "content-card",
          style = "margin-top: 20px;",
          uiOutput(ns("chartTitle8")),
          p(class = "info-text", uiOutput(ns("chartDesc8"))),
          DT::dataTableOutput(ns("freqTable"))
        )
      ),
      
      # -------------------- WHEN NO DATA --------------------------------------
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("hasData")),
        div(
          class = "empty-state",
          style = "padding: 32px; border-radius: 12px; background: rgba(255,255,255,0.04); text-align: center;",
          h3(style = "margin-bottom: 8px;", uiOutput(ns("emptyTitle"))),
          p(style = "opacity: 0.8;", uiOutput(ns("emptySubtitle")))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# SERVER
# - Global plot caching (keys include data_version + lang + chart)
# - Friendly empty state
# - Shared chart theme + title/desc helpers
# -----------------------------------------------------------------------------
tableMetricServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------
    # i18n helpers
    # ---------------------------
    get_lang <- reactive({
      query <- parseQueryString(isolate(session$clientData$url_search))
      query$lang %||% "de"
    })
    
    tr <- reactive({
      lang <- get_lang()
      function(key) t(key, lang)
    })
    
    # Header + Empty state texts
    output$header <- renderUI({
      tagList(
        h1(class = "header-title",    tr()("table_title")),
        p (class = "header-subtitle", tr()("table_subtitle"))
      )
    })
    output$emptyTitle    <- renderUI({ HTML(tr()("no_data_title")    %||% "No data for selected filters") })
    output$emptySubtitle <- renderUI({ HTML(tr()("no_data_subtitle") %||% "Try expanding your number range or time window.") })
    
    # ---------------------------
    # Common chart theme + title/desc helpers
    # ---------------------------
    chart_theme <- function(p) {
      p %>% plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(color = "#e8eaed", family = "Inter"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
      ) %>% plotly::config(displayModeBar = FALSE)
    }
    
    render_title <- function(id, emoji, key) {
      output[[id]] <- renderUI({
        div(class = "chart-title", span(emoji), span(tr()(key)))
      })
    }
    render_desc <- function(id, key) {
      output[[id]] <- renderUI({ tr()(key) })
    }
    
    # Titles + descriptions
    render_title("chartTitle1", "📊", "table_chart_frequencies")
    render_desc ("chartDesc1",  "table_chart_frequencies_desc")
    render_title("chartTitle2", "🔥", "table_chart_hot")
    render_desc ("chartDesc2",  "table_chart_hot_desc")
    render_title("chartTitle3", "❄️", "table_chart_cold")
    render_desc ("chartDesc3",  "table_chart_cold_desc")
    render_title("chartTitle4", "🎯", "table_chart_grid")
    render_desc ("chartDesc4",  "table_chart_grid_desc")
    render_title("chartTitle5", "📈", "table_chart_dist")
    render_desc ("chartDesc5",  "table_chart_dist_desc")
    render_title("chartTitle6", "🎲", "table_chart_position")
    render_desc ("chartDesc6",  "table_chart_position_desc")
    render_title("chartTitle7", "⚖️", "table_chart_deviation")
    render_desc ("chartDesc7",  "table_chart_deviation_desc")
    render_title("chartTitle8", "📋", "table_chart_table")
    render_desc ("chartDesc8",  "table_chart_table_desc")
    
    # ---------------------------
    # Has data? (exported for conditionalPanel)
    # ---------------------------
    has_data <- reactive({
      d <- filtered_data()
      !is.null(d) && nrow(d) > 0
    })
    output$hasData <- reactive({ has_data() })
    outputOptions(output, "hasData", suspendWhenHidden = FALSE)
    
    # ---------------------------
    # Frequency statistics (guarded + defensive)
    # ---------------------------
    freq_stats <- reactive({
      req(has_data())
      d <- filtered_data()
      cols <- paste0("ball_", 1:6)
      req(all(cols %in% names(d)))
      
      balls <- d[, cols, drop = FALSE]
      nums <- unlist(balls, use.names = FALSE)
      
      # Accept only finite numeric
      nums <- nums[is.finite(nums)]
      
      freq_table <- table(nums)
      
      # Ensure all numbers 1..49 are represented (adjust if your lottery differs)
      all_nums <- 1:49
      freq_df <- data.frame(
        number = all_nums,
        frequency = as.numeric(freq_table[as.character(all_nums)]),
        stringsAsFactors = FALSE
      )
      freq_df$frequency[is.na(freq_df$frequency)] <- 0
      
      total_draws   <- nrow(d)
      expected_freq <- (total_draws * 6) / length(all_nums)
      
      freq_df$deviation    <- freq_df$frequency - expected_freq
      freq_df$percentage   <- round((freq_df$frequency / sum(freq_df$frequency)) * 100, 2)
      freq_df$deviation_pct <- if (expected_freq > 0) round((freq_df$deviation / expected_freq) * 100, 1) else 0
      
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
    
    # ---------------------------
    # Metric cards
    # ---------------------------
    card <- function(icon, value, label) {
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", icon),
        div(class = "value-box-value", value),
        div(class = "value-box-label", label)
      )
    }
    
    output$metricCard1 <- renderUI({ req(has_data()); s <- freq_stats(); card("🔥", s$most_common, tr()("table_metric_hottest")) })
    output$metricCard2 <- renderUI({ req(has_data()); s <- freq_stats(); card("❄️", s$least_common, tr()("table_metric_coldest")) })
    output$metricCard3 <- renderUI({ req(has_data()); s <- freq_stats(); card("🎯", round(s$expected_freq, 1), tr()("table_metric_expected")) })
    output$metricCard4 <- renderUI({
      req(has_data()); s <- freq_stats()
      card("↕️", s$max_freq - s$min_freq, tr()("table_metric_range"))
    })
    
    # ---------------------------
    # Cache-key helpers
    # ---------------------------
    compute_light_version <- function(d) {
      if (is.null(d) || !nrow(d)) return("empty")
      cols <- paste0("ball_", 1:6)
      cols <- cols[cols %in% names(d)]
      if (!length(cols)) return(paste0("rows", nrow(d), "_maxF0"))
      nums <- unlist(d[, cols, drop = FALSE], use.names = FALSE)
      nums <- nums[is.finite(nums)]
      # lightweight signature: rows + max observed number + approx mean
      paste0(
        "rows", nrow(d),
        "_maxN", suppressWarnings(max(nums, na.rm = TRUE)),
        "_mean", sprintf("%.2f", suppressWarnings(mean(nums, na.rm = TRUE)))
      )
    }
    
    cache_key <- function(chart_id) {
      lang <- get_lang()
      data_version <- getOption("metrics_data_version", NULL)
      if (is.null(data_version)) {
        d <- filtered_data()
        data_version <- compute_light_version(d)
      }
      sprintf("v=%s|lang=%s|chart=%s", data_version, lang, chart_id)
    }
    
    # ---------------------------
    # Plot builders
    # ---------------------------
    build_freq <- function(s) {
      df <- s$freq_df
      plotly::plot_ly(
        df, x = ~number, y = ~frequency, type = "bar",
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
            title = tr()("table_label_frequency"),
            titlefont = list(color = "#e8eaed"),
            tickfont = list(color = "#e8eaed")
          )
        ),
        customdata = ~cbind(df$percentage, df$deviation),
        hovertemplate = "<b>Number: %{x}</b><br>Frequency: %{y}<br>Percentage: %{customdata[0]}%<br>Deviation: %{customdata[1]:.1f}<extra></extra>"
      ) %>%
        plotly::add_trace(
          x = c(min(df$number), max(df$number)),
          y = rep(s$expected_freq, 2),
          type = "scatter", mode = "lines",
          line = list(color = "#10b981", width = 3, dash = "dash"),
          name = tr()("table_label_expected"),
          hovertemplate = paste0(tr()("table_label_expected"), ": ", round(s$expected_freq, 1), "<extra></extra>"),
          showlegend = TRUE,
          inherit = FALSE
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(
            title = tr()("table_label_number"),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            dtick = 1,
            color = "#e8eaed"
          ),
          yaxis = list(
            title = tr()("table_label_frequency"),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            color = "#e8eaed"
          ),
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
        )
    }
    
    build_hot <- function(s) {
      df <- s$freq_df
      hot <- df[order(-df$frequency), ][seq_len(min(10, nrow(df))), ]
      plotly::plot_ly(
        hot, x = ~reorder(number, frequency), y = ~frequency, type = "bar",
        marker = list(
          color = grDevices::colorRampPalette(c("#ff6b6b", "#DC143C"))(nrow(hot)),
          line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
        ),
        text = ~frequency, textposition = "outside",
        textfont = list(color = "#e8eaed", size = 12),
        customdata = ~deviation,
        hovertemplate = paste0(
          "<b>", tr()("table_label_number"), ": %{x}</b><br>",
          tr()("table_label_frequency"), ": %{y}<br>",
          tr()("table_label_deviation"), ": +%{customdata:.1f}<extra></extra>"
        )
      ) %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("table_label_number"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed"),
          yaxis = list(title = tr()("table_label_frequency"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed")
        )
    }
    
    build_cold <- function(s) {
      df <- s$freq_df
      cold <- df[order(df$frequency), ][seq_len(min(10, nrow(df))), ]
      plotly::plot_ly(
        cold, x = ~reorder(number, -frequency), y = ~frequency, type = "bar",
        marker = list(
          color = grDevices::colorRampPalette(c("#00f2fe", "#4facfe"))(nrow(cold)),
          line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
        ),
        text = ~frequency, textposition = "outside",
        textfont = list(color = "#e8eaed", size = 12),
        customdata = ~deviation,
        hovertemplate = paste0(
          "<b>", tr()("table_label_number"), ": %{x}</b><br>",
          tr()("table_label_frequency"), ": %{y}<br>",
          tr()("table_label_deviation"), ": %{customdata:.1f}<extra></extra>"
        )
      ) %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("table_label_number"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed"),
          yaxis = list(title = tr()("table_label_frequency"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed")
        )
    }
    
    build_heat <- function(s) {
      df <- s$freq_df
      # Create 7x7 grid
      mat <- matrix(0, nrow = 7, ncol = 7)
      labels <- matrix("", nrow = 7, ncol = 7)
      for (i in 1:49) {
        row <- ((i - 1) %/% 7) + 1
        col <- ((i - 1) %% 7) + 1
        mat[row, col] <- df$frequency[i]
        labels[row, col] <- as.character(i)
      }
      n_rows <- nrow(mat); n_cols <- ncol(mat)
      label_number    <- as.character(tr()("table_label_number"))
      label_frequency <- as.character(tr()("table_label_frequency"))
      anno_text <- as.vector(base::t(labels))
      
      plotly::plot_ly(
        z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
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
          titlefont = list(color = "#e8eaed"),
          tickfont = list(color = "#e8eaed")
        )
      ) %>%
        plotly::add_annotations(
          x = rep(1:n_cols, each = n_rows),
          y = rep(1:n_rows, times = n_cols),
          text = anno_text,
          textfont = list(color = "#FFFFFF", size = 14, family = "Inter"),
          showarrow = FALSE, xref = "x", yref = "y"
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        )
    }
    
    build_freq_dist <- function(s) {
      df <- s$freq_df
      plotly::plot_ly(
        x = ~df$frequency, type = "histogram",
        marker = list(color = "#8b5cf6", line = list(color = "rgba(255, 255, 255, 0.3)", width = 1.5)),
        nbinsx = 20,
        hovertemplate = paste0(
          tr()("table_label_frequency"), " Range: %{x}<br>",
          tr()("table_label_count"), ": %{y}<br><extra></extra>"
        )
      ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("table_label_frequency"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed"),
          yaxis = list(title = paste0(tr()("table_label_number"), "s"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed"),
          bargap = 0.1
        )
    }
    
    build_position <- function(s) {
      # Average frequency per position
      d <- filtered_data()
      cols <- paste0("ball_", 1:6)
      balls <- d[, cols, drop = FALSE]
      position_freq <- sapply(seq_along(cols), function(pos) {
        nums <- balls[[pos]]
        nums <- nums[is.finite(nums)]
        ft <- table(nums)
        if (length(ft)) mean(as.numeric(ft)) else 0
      })
      df <- data.frame(
        position = paste0(tr()("ball_label"), " ", 1:6),
        avg_freq = as.numeric(position_freq)
      )
      ball_colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      
      plotly::plot_ly(
        df, x = ~position, y = ~avg_freq, type = "bar",
        marker = list(color = ball_colors, line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)),
        text = ~round(avg_freq, 1), textposition = "outside",
        textfont = list(color = "#e8eaed", size = 14),
        hovertemplate = paste0("<b>%{x}</b><br>", tr()("table_label_avg_frequency"), ": %{y:.2f}<extra></extra>")
      ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("table_label_ball_position"), gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed"),
          yaxis = list(title = tr()("table_label_avg_frequency"),  gridcolor = "rgba(255,255,255,0.1)", color = "#e8eaed")
        )
    }
    
    build_deviation <- function(s) {
      df <- s$freq_df
      df$color <- ifelse(df$deviation >= 0, "#10b981", "#ef4444")
      plotly::plot_ly(
        df, x = ~number, y = ~deviation, type = "bar",
        marker = list(color = ~color, line = list(color = "rgba(255, 255, 255, 0.3)", width = 1)),
        customdata = ~deviation_pct,
        hovertemplate = paste0(
          "<b>", tr()("table_label_number"), ": %{x}</b><br>",
          tr()("table_label_deviation"), ": %{y:.2f}<br>",
          tr()("table_label_deviation_pct"), ": %{customdata}%<extra></extra>"
        )
      ) %>%
        plotly::add_trace(
          x = c(min(df$number), max(df$number)), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = "#e8eaed", width = 2),
          name = tr()("table_label_expected"),
          hovertemplate = paste0(tr()("table_label_expected"), ": 0<extra></extra>"),
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("table_label_number"), gridcolor = "rgba(255,255,255,0.1)", dtick = 1, color = "#e8eaed"),
          yaxis = list(
            title = tr()("table_chart_deviation"),
            gridcolor = "rgba(255,255,255,0.1)",
            zeroline = TRUE, zerolinecolor = "rgba(255,255,255,0.3)",
            color = "#e8eaed"
          )
        )
    }
    
    # ---------------------------
    # Cached render helper
    # ---------------------------
    render_cached_plot <- function(output_id, chart_id, builder) {
      output[[output_id]] <- plotly::renderPlotly({
        req(has_data())
        key <- cache_key(chart_id)
        cached <- .table_cache_get(key)
        if (!is.null(cached)) return(cached)
        s <- freq_stats()
        plt <- builder(s)
        .table_cache_set(key, plt)
        plt
      })
    }
    
    # Wire up cached plots
    render_cached_plot("freq",             "freq",      build_freq)
    render_cached_plot("hotNumbers",       "hot",       build_hot)
    render_cached_plot("coldNumbers",      "cold",      build_cold)
    render_cached_plot("heatGrid",         "heat",      build_heat)
    render_cached_plot("freqDist",         "freqDist",  build_freq_dist)
    render_cached_plot("positionAnalysis", "position",  build_position)
    render_cached_plot("deviation",        "deviation", build_deviation)
    
    # Data Table (not cached; lightweight and depends on DataTables JS)
    output$freqTable <- DT::renderDataTable({
      req(has_data())
      s  <- freq_stats()
      df <- s$freq_df
      df_display <- data.frame(
        Number      = df$number,
        Frequency   = df$frequency,
        Percentage  = paste0(df$percentage, "%"),
        Deviation   = round(df$deviation, 2),
        `Deviation %` = paste0(df$deviation_pct, "%"),
        check.names = FALSE
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
