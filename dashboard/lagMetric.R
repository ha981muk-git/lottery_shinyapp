# =============================================================================
# Lag Metric Module — Performance Edition (Global Cache + Empty State)
# =============================================================================

# -----------------------------------------------------------------------------
# OPTIONAL: Global cache invalidation helper for the hosting app.
# Call this after your data source refreshes (e.g., nightly ETL).
# Or set options(metrics_data_version = "<new-version>") to rotate namespaces.
# -----------------------------------------------------------------------------
lag_clear_cache <- function() {
  if (exists(".lag_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".lag_plot_cache", envir = .GlobalEnv, inherits = FALSE)
  }
  invisible(TRUE)
}

# Global cache env (shared between users)
.lag_get_cache_env <- function() {
  if (!exists(".lag_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".lag_plot_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".lag_plot_cache", envir = .GlobalEnv, inherits = FALSE)
}
.lag_cache_get <- function(key) {
  env <- .lag_get_cache_env()
  if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL
}
.lag_cache_set <- function(key, value) {
  env <- .lag_get_cache_env()
  assign(key, value, envir = env)
  invisible(value)
}

# -----------------------------------------------------------------------------
# UI — uses conditionalPanel via output$hasData to show charts or empty state
# -----------------------------------------------------------------------------
lagMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      
      # Header
      div(style = "margin-bottom: 32px;", uiOutput(ns("header"))),
      
      # -------------------- WHEN DATA EXISTS ----------------------------------
      conditionalPanel(
        condition = sprintf("output['%s']", ns("hasData")),
        
        # Statistics Row
        layout_column_wrap(
          width = 1/4, heights_equal = "row", gap = "15px",
          uiOutput(ns("metricCard1")),
          uiOutput(ns("metricCard2")),
          uiOutput(ns("metricCard3")),
          uiOutput(ns("metricCard4"))
        ),
        
        # Ball Position Selector
        div(
          class = "content-card", style = "margin-top: 25px;",
          div(
            style = "margin-bottom: 20px;",
            h4(style = "color: #8b5cf6;", uiOutput(ns("selectorTitle"))),
            div(
              style = "display: flex; gap: 10px; flex-wrap: wrap; justify-content: center;",
              actionButton(ns("ball1"),  "Ball 1", class = "btn-action btn-primary", style = "background:#4169E1;border-color:#4169E1;"),
              actionButton(ns("ball2"),  "Ball 2", class = "btn-action btn-primary", style = "background:#DC143C;border-color:#DC143C;"),
              actionButton(ns("ball3"),  "Ball 3", class = "btn-action btn-primary", style = "background:#32CD32;border-color:#32CD32;"),
              actionButton(ns("ball4"),  "Ball 4", class = "btn-action btn-primary", style = "background:#FFD700;border-color:#FFD700;color:#1a1f3a;"),
              actionButton(ns("ball5"),  "Ball 5", class = "btn-action btn-primary", style = "background:#9370DB;border-color:#9370DB;"),
              actionButton(ns("ball6"),  "Ball 6", class = "btn-action btn-primary", style = "background:#00CED1;border-color:#00CED1;"),
              actionButton(ns("ballAll"), uiOutput(ns("allBallsText")), class = "btn-action btn-success")
            )
          ),
          div(style = "text-align: center; margin-top: 15px;", uiOutput(ns("selectedBall")))
        ),
        
        # Distribution
        div(
          class = "content-card", style = "margin-top: 25px;",
          uiOutput(ns("chartTitle1")), p(class = "info-text", uiOutput(ns("chartDesc1"))),
          plotlyOutput(ns("lagDistribution"), height = "500px")
        ),
        
        # Jump Preference Charts
        layout_column_wrap(
          width = 1/2, heights_equal = "row", gap = "20px", fill = FALSE,
          div(class = "content-card", uiOutput(ns("chartTitle2")), p(class = "info-text", uiOutput(ns("chartDesc2"))), plotlyOutput(ns("positiveJumps"), height = "400px")),
          div(class = "content-card", uiOutput(ns("chartTitle3")), p(class = "info-text", uiOutput(ns("chartDesc3"))), plotlyOutput(ns("negativeJumps"), height = "400px"))
        ),
        
        # Jump Categories
        div(
          class = "content-card", style = "margin-top: 25px;",
          uiOutput(ns("chartTitle4")), p(class = "info-text", uiOutput(ns("chartDesc4"))),
          plotlyOutput(ns("jumpCategories"), height = "450px")
        ),
        
        # Heatmap and Q-Q Plot
        layout_column_wrap(
          width = 1/2, heights_equal = "row", gap = "20px", fill = FALSE,
          div(class = "content-card", uiOutput(ns("chartTitle5")), p(class = "info-text", uiOutput(ns("chartDesc5"))), plotlyOutput(ns("lagHeatmap"), height = "450px")),
          div(class = "content-card", uiOutput(ns("chartTitle6")), p(class = "info-text", uiOutput(ns("chartDesc6"))), plotlyOutput(ns("qqPlot"), height = "450px"))
        ),
        
        # Preferred Zones
        div(
          class = "content-card", style = "margin-top: 25px;",
          uiOutput(ns("chartTitle7")), p(class = "info-text", uiOutput(ns("chartDesc7"))),
          plotlyOutput(ns("preferredZones"), height = "450px")
        ),
        
        # Statistical Summary
        div(
          class = "content-card", style = "margin-top: 25px;",
          uiOutput(ns("chartTitle8")), uiOutput(ns("statSummary"))
        ),
        
        # Table
        div(
          class = "content-card", style = "margin-top: 25px;",
          uiOutput(ns("chartTitle9")), p(class = "info-text", uiOutput(ns("chartDesc9"))),
          DT::dataTableOutput(ns("lagTable"))
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
lagMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title",    tr()("lag_title")),
        p (class = "header-subtitle", tr()("lag_subtitle"))
      )
    })
    output$emptyTitle    <- renderUI({ HTML(tr()("no_data_title")    %||% "No data for selected filters") })
    output$emptySubtitle <- renderUI({ HTML(tr()("no_data_subtitle") %||% "Try expanding your number range or time window.") })
    output$allBallsText  <- renderUI({ tr()("lag_selector_all_button") %||% "All Balls" })
    
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
      output[[id]] <- renderUI({ div(class = "chart-title", span(emoji), span(tr()(key))) })
    }
    render_desc <- function(id, key) { output[[id]] <- renderUI({ tr()(key) }) }
    
    # Titles + descriptions
    render_title("chartTitle1", "📊", "lag_chart_distribution")
    render_desc ("chartDesc1",  "lag_chart_distribution_desc")
    render_title("chartTitle2", "⬆️", "lag_chart_positive")
    render_desc ("chartDesc2",  "lag_chart_positive_desc")
    render_title("chartTitle3", "⬇️", "lag_chart_negative")
    render_desc ("chartDesc3",  "lag_chart_negative_desc")
    render_title("chartTitle4", "🎯", "lag_chart_categories")
    render_desc ("chartDesc4",  "lag_chart_categories_desc")
    render_title("chartTitle5", "🔥", "lag_chart_heatmap")
    render_desc ("chartDesc5",  "lag_chart_heatmap_desc")
    render_title("chartTitle6", "📈", "lag_chart_qq")
    render_desc ("chartDesc6",  "lag_chart_qq_desc")
    render_title("chartTitle7", "🎲", "lag_chart_zones")
    render_desc ("chartDesc7",  "lag_chart_zones_desc")
    render_title("chartTitle8", "📊", "lag_chart_summary")
    render_title("chartTitle9", "📋", "lag_chart_table")
    render_desc ("chartDesc9",  "lag_chart_table_desc")
    
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
    # Selected ball
    # ---------------------------
    selected_ball <- reactiveVal(0)
    observeEvent(input$ball1,  { selected_ball(1) })
    observeEvent(input$ball2,  { selected_ball(2) })
    observeEvent(input$ball3,  { selected_ball(3) })
    observeEvent(input$ball4,  { selected_ball(4) })
    observeEvent(input$ball5,  { selected_ball(5) })
    observeEvent(input$ball6,  { selected_ball(6) })
    observeEvent(input$ballAll,{ selected_ball(0) })
    
    # Selected ball display
    output$selectedBall <- renderUI({
      ball <- selected_ball() %||% 0
      colors <- c("#4169E1", "#DC143C", "#32CD32", "#FFD700", "#9370DB", "#00CED1")
      text   <- if (ball == 0) (tr()("lag_selector_all") %||% "All Balls") else paste0("Ball ", ball)
      color  <- if (ball == 0) "#8b5cf6" else colors[ball]
      div(style = paste0("font-size:24px;font-weight:bold;color:", color, ";"),
          paste0(tr()("lag_selector_analyzing"), ": ", text))
    })
    
    # ---------------------------
    # Lag statistics (guarded + defensive)
    # ---------------------------
    lag_stats <- reactive({
      req(has_data())
      d <- filtered_data()
      cols <- paste0("ball_", 1:6)
      req(all(cols %in% names(d)))
      
      ball <- selected_ball() %||% 0
      # Build vector of lags (diffs) based on selection
      get_diffs <- function(v) {
        v <- as.numeric(v)
        v <- v[is.finite(v)]
        if (length(v) < 2) numeric(0) else diff(v)
      }
      
      if (nrow(d) < 2) return(list(lags = numeric(0), lag_df = data.frame()))
      if (ball == 0) {
        lags <- unlist(lapply(cols, function(cn) get_diffs(d[[cn]])), use.names = FALSE)
      } else {
        lags <- get_diffs(d[[cols[ball]]])
      }
      
      if (!length(lags)) return(list(lags = numeric(0), lag_df = data.frame()))
      
      lag_tab <- table(lags)
      lag_df <- data.frame(
        lag = as.numeric(names(lag_tab)),
        frequency = as.numeric(lag_tab),
        stringsAsFactors = FALSE
      )
      tot <- sum(lag_df$frequency)
      lag_df$percentage  <- if (tot > 0) round(lag_df$frequency / tot * 100, 2) else 0
      lag_df$probability <- if (tot > 0) lag_df$frequency / tot else 0
      lag_df$category <- cut(abs(lag_df$lag),
                             breaks = c(0, 3, 7, 15, Inf),
                             labels = c("Tiny (0-3)", "Small (4-7)", "Medium (8-15)", "Large (>15)"),
                             include.lowest = TRUE)
      # Direction strings localized via tr()
      inc <- tr()("lag_label_increase"); dec <- tr()("lag_label_decrease"); none <- tr()("lag_label_no_change")
      lag_df$direction <- ifelse(lag_df$lag > 0, inc, ifelse(lag_df$lag < 0, dec, none))
      
      list(
        lags = lags,
        lag_df = lag_df,
        mean = mean(lags),
        sd = stats::sd(lags),
        median = stats::median(lags),
        min = min(lags),
        max = max(lags),
        most_common = as.numeric(names(lag_tab)[which.max(lag_tab)])
      )
    })
    
    # ---------------------------
    # Metric cards
    # ---------------------------
    card <- function(icon, value, label) {
      div(class = "value-box-custom",
          div(class = "value-box-icon", icon),
          div(class = "value-box-value", value),
          div(class = "value-box-label", label))
    }
    output$metricCard1 <- renderUI({ req(has_data()); s <- lag_stats(); if(!length(s$lags)) return(NULL); card("📊", round(s$mean, 2), tr()("lag_metric_avg")) })
    output$metricCard2 <- renderUI({ req(has_data()); s <- lag_stats(); if(!length(s$lags)) return(NULL); card("📏", round(s$sd,   2), tr()("lag_metric_sd")) })
    output$metricCard3 <- renderUI({ req(has_data()); s <- lag_stats(); if(!length(s$lags)) return(NULL); card("⭐",  s$most_common,          tr()("lag_metric_most_common")) })
    output$metricCard4 <- renderUI({
      req(has_data()); s <- lag_stats(); if(!length(s$lags)) return(NULL)
      card("📈", s$max - s$min, tr()("lag_metric_range"))
    })
    
    # ---------------------------
    # Cache-key helpers
    # ---------------------------
    compute_light_version <- function(d) {
      if (is.null(d) || !nrow(d)) return("empty")
      cols <- paste0("ball_", 1:6); cols <- cols[cols %in% names(d)]
      if (!length(cols)) return(paste0("rows", nrow(d), "_lagSig0"))
      vals <- unlist(d[, cols, drop = FALSE], use.names = FALSE)
      vals <- vals[is.finite(vals)]
      paste0(
        "rows", nrow(d),
        "_max", suppressWarnings(max(vals, na.rm = TRUE)),
        "_mean", sprintf("%.2f", suppressWarnings(mean(vals, na.rm = TRUE)))
      )
    }
    cache_key <- function(chart_id) {
      lang <- get_lang()
      data_version <- getOption("metrics_data_version", NULL)
      if (is.null(data_version)) {
        d <- filtered_data()
        data_version <- compute_light_version(d)
      }
      sprintf("v=%s|lang=%s|chart=%s|ball=%s", data_version, lang, chart_id, as.integer(selected_ball() %||% 0))
    }
    
    # ---------------------------
    # Plot builders
    # ---------------------------
    build_distribution <- function(s) {
      hist_meta <- graphics::hist(s$lags, breaks = 30, plot = FALSE)
      x_seq <- seq(min(s$lags), max(s$lags), length.out = 100)
      y_norm <- stats::dnorm(x_seq, mean = s$mean, sd = s$sd)
      binw <- diff(hist_meta$breaks[1:2])
      y_norm_scaled <- y_norm * length(s$lags) * binw
      
      plotly::plot_ly() %>%
        plotly::add_bars(
          x = hist_meta$mids, y = hist_meta$counts,
          marker = list(color = "#8b5cf6", line = list(color = "rgba(255,255,255,0.3)", width = 1.5)),
          name = tr()("lag_chart_actual"),
          hovertemplate = paste0(tr()("lag_label_lag"), ": %{x}<br>", tr()("lag_label_frequency"), ": %{y}<extra></extra>")
        ) %>%
        plotly::add_lines(
          x = x_seq, y = y_norm_scaled,
          line = list(color = "#ec4899", width = 3),
          name = tr()("lag_chart_normal"),
          hovertemplate = paste0(tr()("lag_label_lag"), ": %{x:.1f}<br>", tr()("lag_hover_expected"), ": %{y:.1f}<extra></extra>")
        ) %>%
        plotly::add_trace(
          x = rep(s$mean, 2), y = c(0, max(hist_meta$counts) * 1.1),
          type = "scatter", mode = "lines",
          line = list(color = "#10b981", width = 3, dash = "dash"),
          name = tr()("lag_chart_mean"),
          hovertemplate = paste0(tr()("lag_chart_mean"), ": ", round(s$mean, 2), "<extra></extra>")
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("lag_label_lag"),      gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_frequency"),gridcolor = "rgba(255,255,255,0.1)"),
          bargap = 0.05
        )
    }
    
    build_positive <- function(s) {
      df <- subset(s$lag_df, lag > 0)
      if (!nrow(df)) return(NULL)
      df <- df[order(-df$frequency), ][seq_len(min(15, nrow(df))), ]
      plotly::plot_ly(
        df, x = ~reorder(lag, frequency), y = ~frequency, type = "bar",
        marker = list(color = grDevices::colorRampPalette(c("#32CD32", "#10b981"))(nrow(df)),
                      line = list(color = "rgba(255,255,255,0.3)", width = 2)),
        text = ~paste0(frequency, " (", percentage, "%)"),
        textposition = "outside", textfont = list(color = "#e8eaed", size = 11),
        hovertemplate = paste0("<b>", tr()("lag_hover_jump"), ": +%{x}</b><br>", tr()("lag_label_frequency"), ": %{y}<br>", tr()("lag_label_percentage"), ": %{text}<extra></extra>")
      ) %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("lag_label_jump_size"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_frequency"), gridcolor = "rgba(255,255,255,0.1)")
        )
    }
    
    build_negative <- function(s) {
      df <- subset(s$lag_df, lag < 0)
      if (!nrow(df)) return(NULL)
      df <- df[order(-df$frequency), ][seq_len(min(15, nrow(df))), ]
      plotly::plot_ly(
        df, x = ~reorder(lag, -frequency), y = ~frequency, type = "bar",
        marker = list(color = grDevices::colorRampPalette(c("#ef4444", "#DC143C"))(nrow(df)),
                      line = list(color = "rgba(255,255,255,0.3)", width = 2)),
        text = ~paste0(frequency, " (", percentage, "%)"),
        textposition = "outside", textfont = list(color = "#e8eaed", size = 11),
        hovertemplate = paste0("<b>", tr()("lag_hover_jump"), ": %{x}</b><br>", tr()("lag_label_frequency"), ": %{y}<br>", tr()("lag_label_percentage"), ": %{text}<extra></extra>")
      ) %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("lag_label_jump_size"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_frequency"), gridcolor = "rgba(255,255,255,0.1)")
        )
    }
    
    build_categories <- function(s) {
      df <- s$lag_df
      if (!nrow(df)) return(NULL)
      agg <- aggregate(frequency ~ category + direction, data = df, sum)
      plotly::plot_ly() %>%
        { p <- .
        for (dir in unique(agg$direction)) {
          data_dir <- agg[agg$direction == dir, ]
          col <- if (identical(dir, tr()("lag_label_increase"))) "#10b981"
          else if (identical(dir, tr()("lag_label_decrease"))) "#ef4444"
          else "#8b5cf6"
          p <- plotly::add_trace(p, data = data_dir, x = ~category, y = ~frequency,
                                 type = "bar", name = dir,
                                 marker = list(color = col),
                                 hovertemplate = paste0("<b>", dir, " - %{x}</b><br>", tr()("lag_label_frequency"), ": %{y}<extra></extra>"))
        }
        p
        } %>%
        chart_theme() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = tr()("lag_label_jump_category"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_frequency"),     gridcolor = "rgba(255,255,255,0.1)")
        )
    }
    
    build_heatmap <- function(s) {
      df <- s$lag_df
      if (!nrow(df)) return(NULL)
      n_cols <- 10
      n_rows <- ceiling(nrow(df) / n_cols)
      total  <- n_rows * n_cols
      pad_lag <- c(df$lag,       rep(NA, total - nrow(df)))
      pad_frq <- c(df$frequency, rep(0,   total - nrow(df)))
      mat <- matrix(pad_frq, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      labels <- matrix(pad_lag, nrow = n_rows, ncol = n_cols, byrow = TRUE)
      
      label_lag <- tr()("lag_label_lag"); label_freq <- tr()("lag_label_frequency")
      anno_text <- as.character(as.vector(base::t(labels))); anno_text[is.na(anno_text)] <- ""
      
      plotly::plot_ly(
        z = mat, x = 1:n_cols, y = 1:n_rows, type = "heatmap",
        colorscale = list(
          c(0, "rgba(79, 172, 254, 0.2)"),
          c(0.5, "rgba(139, 92, 246, 0.7)"),
          c(1, "rgba(236, 72, 153, 1)")
        ),
        text = labels,
        hovertemplate = paste0("<b>", label_lag, ": %{text}</b><br>", label_freq, ": %{z}<br><extra></extra>"),
        showscale = TRUE,
        colorbar = list(title = label_freq, titlefont = list(color = "#e8eaed"), tickfont = list(color = "#e8eaed"))
      ) %>%
        plotly::add_annotations(
          x = rep(1:n_cols, each = n_rows), y = rep(1:n_rows, times = n_cols),
          text = anno_text, textfont = list(color = "#FFFFFF", size = 10, family = "Inter"),
          showarrow = FALSE, xref = "x", yref = "y"
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        )
    }
    
    build_qq <- function(s) {
      th <- qqnorm(s$lags, plot.it = FALSE)
      plotly::plot_ly() %>%
        plotly::add_markers(
          x = th$x, y = th$y,
          marker = list(color = "#8b5cf6", size = 6),
          name = tr()("lag_chart_data_points"),
          hovertemplate = paste0(tr()("lag_label_theoretical"), ": %{x:.2f}<br>", tr()("lag_label_sample"), ": %{y:.2f}<extra></extra>")
        ) %>%
        plotly::add_lines(
          x = range(th$x), y = range(th$x),
          line = list(color = "#ec4899", width = 3, dash = "dash"),
          name = tr()("lag_chart_perfect_normal"),
          hovertemplate = paste0(tr()("lag_chart_perfect_normal"), "<extra></extra>")
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("lag_label_theoretical"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_sample"),      gridcolor = "rgba(255,255,255,0.1)")
        )
    }
    
    build_zones <- function(s) {
      df <- s$lag_df
      if (!nrow(df)) return(NULL)
      df <- df[order(df$lag), ]
      df$zone <- ifelse(df$percentage >= 2, "Hot Zone",
                        ifelse(df$percentage >= 1, "Warm Zone",
                               ifelse(df$percentage >= 0.5, "Cool Zone", "Cold Zone")))
      zone_colors <- c("Hot Zone"="#DC143C","Warm Zone"="#ff6b6b","Cool Zone"="#4facfe","Cold Zone"="#4169E1")
      plotly::plot_ly(
        df, x = ~lag, y = ~percentage, type = "bar",
        marker = list(color = ~zone, colors = zone_colors, line = list(color = "rgba(255,255,255,0.3)", width = 1)),
        text = ~zone,
        hovertemplate = paste0("<b>", tr()("lag_label_lag"), ": %{x}</b><br>", tr()("lag_label_percentage"), ": %{y}%<br>Zone: %{text}<extra></extra>")
      ) %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("lag_label_lag_value"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("lag_label_percentage"), gridcolor = "rgba(255,255,255,0.1)")
        )
    }
    
    # ---------------------------
    # Cached render helper
    # ---------------------------
    render_cached_plot <- function(output_id, chart_id, builder) {
      output[[output_id]] <- plotly::renderPlotly({
        req(has_data())
        s <- lag_stats()
        if (!length(s$lags)) return(NULL)
        key <- cache_key(chart_id)
        cached <- .lag_cache_get(key)
        if (!is.null(cached)) return(cached)
        plt <- builder(s)
        .lag_cache_set(key, plt)
        plt
      })
    }
    
    # Wire up cached plots
    render_cached_plot("lagDistribution", "dist",     build_distribution)
    render_cached_plot("positiveJumps",  "pos",      build_positive)
    render_cached_plot("negativeJumps",  "neg",      build_negative)
    render_cached_plot("jumpCategories", "cats",     build_categories)
    render_cached_plot("lagHeatmap",     "heat",     build_heatmap)
    render_cached_plot("qqPlot",         "qq",       build_qq)
    render_cached_plot("preferredZones", "zones",    build_zones)
    
    # ---------------------------
    # Statistical Summary (lightweight UI)
    # ---------------------------
    output$statSummary <- renderUI({
      s <- lag_stats()
      if (!length(s$lags)) return(p("No data available"))
      df <- s$lag_df
      hot <- df[order(-df$frequency), ][seq_len(min(5, nrow(df))), ]
      ci_lower <- s$mean - 1.96 * s$sd
      ci_upper <- s$mean + 1.96 * s$sd
      pval <- if (length(s$lags) >= 3 && length(s$lags) <= 5000) shapiro.test(s$lags)$p.value else NA
      
      tagList(
        div(
          style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:20px;padding:20px;",
          div(class = "value-box-custom", style = "text-align:left;",
              h4(style = "color:#ec4899;margin-bottom:15px;", paste0("🔥 ", tr()("lag_summary_top_jumps"))),
              tags$ul(style = "list-style:none;padding:0;",
                      lapply(seq_len(nrow(hot)), function(i) {
                        tags$li(
                          style = "padding:8px 0;border-bottom:1px solid rgba(255,255,255,0.1);",
                          tags$span(style = "font-size:18px;font-weight:bold;color:#8b5cf6;", hot$lag[i]),
                          tags$span(style = "margin-left:15px;color:rgba(255,255,255,0.7);", paste0("(", hot$percentage[i], "%)"))
                        )
                      }))
          ),
          div(class = "value-box-custom", style = "text-align:left;",
              h4(style = "color:#10b981;margin-bottom:15px;", paste0("📊 ", tr()("lag_summary_statistical"))),
              div(style = "line-height:1.8;",
                  div(tags$strong(paste0(tr()("lag_summary_ci"), ": ")),   tags$span(sprintf("[%.1f, %.1f]", ci_lower, ci_upper))),
                  div(tags$strong(paste0(tr()("lag_summary_expected"), ": ")), tags$span(sprintf("%.1f to %.1f", s$mean - s$sd, s$mean + s$sd))),
                  div(tags$strong(paste0(tr()("lag_summary_normality"), ": ")),
                      tags$span(if (!is.na(pval)) { if (pval > 0.05) tr()("lag_summary_follows") else tr()("lag_summary_deviates") } else "Test not applicable"))
              )
          ),
          div(class = "value-box-custom", style = "text-align:left;",
              h4(style = "color:#FFD700;margin-bottom:15px;", paste0("💡 ", tr()("lag_summary_recommendations"))),
              div(style = "line-height:1.8;color:rgba(255,255,255,0.8);",
                  div(paste0("✓ ", tr()("lag_summary_rec_hot"))),
                  div(paste0("✓ ", tr()("lag_summary_rec_within"), " ±", round(s$sd, 1), " ", tr()("lag_summary_rec_of_mean"))),
                  div(paste0("✓ ", tr()("lag_summary_rec_avoid"))),
                  div(paste0("✓ ", tr()("lag_summary_rec_likely"), ": ", s$most_common))
              )
          )
        )
      )
    })
    
    # ---------------------------
    # Data Table (light; not cached)
    # ---------------------------
    output$lagTable <- DT::renderDataTable({
      s <- lag_stats()
      if (!length(s$lags)) return(NULL)
      df <- s$lag_df
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
            "$(this.api().table().container()).css({'background-color':'rgba(255,255,255,0.05)','color':'#e8eaed'});",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(columns = 1:6, backgroundColor = 'rgba(255,255,255,0.02)', color = '#e8eaed') %>%
        DT::formatStyle('Frequency',
                        background = DT::styleColorBar(range(df$frequency), '#8b5cf6'),
                        backgroundSize = '90% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
    })
    
  })
}
