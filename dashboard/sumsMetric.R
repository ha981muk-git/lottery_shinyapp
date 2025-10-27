# =============================================================================
# Sums Metric Module — Performance Edition (Global Cache + Empty State)
# =============================================================================

# -----------------------------------------------------------------------------
# OPTIONAL: Global cache invalidation helper for the hosting app.
# Call this after your data source refreshes (e.g., nightly ETL).
# Or set options(metrics_data_version = "<new-version>") to rotate namespaces.
# -----------------------------------------------------------------------------
sums_clear_cache <- function() {
  if (exists(".sums_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".sums_plot_cache", envir = .GlobalEnv, inherits = FALSE)
  }
  invisible(TRUE)
}

# Global cache env (shared between users)
.sums_get_cache_env <- function() {
  if (!exists(".sums_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".sums_plot_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".sums_plot_cache", envir = .GlobalEnv, inherits = FALSE)
}

.sums_cache_get <- function(key) {
  env <- .sums_get_cache_env()
  if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL
}

.sums_cache_set <- function(key, value) {
  env <- .sums_get_cache_env()
  assign(key, value, envir = env)
  invisible(value)
}

# -----------------------------------------------------------------------------
# UI — uses conditionalPanel via output$hasData to show charts or empty state
# -----------------------------------------------------------------------------
sumsMetricUI <- function(id) {
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
          width = 1/5, gap = "12px", heights_equal = "row",
          uiOutput(ns("metricCard1")),
          uiOutput(ns("metricCard2")),
          uiOutput(ns("metricCard3")),
          uiOutput(ns("metricCard4")),
          uiOutput(ns("metricCard5"))
        ),
        
        # Distribution and Trend Row
        layout_column_wrap(
          width = 1/2, heights_equal = "row", gap = "20px",
          div(
            class = "content-card",
            uiOutput(ns("chartTitle1")),
            p(class = "info-text", uiOutput(ns("chartDesc1"))),
            plotlyOutput(ns("hist"), height = "400px")
          ),
          div(
            class = "content-card",
            uiOutput(ns("chartTitle2")),
            p(class = "info-text", uiOutput(ns("chartDesc2"))),
            plotlyOutput(ns("trend"), height = "400px")
          )
        ),
        
        # Range Analysis and Box Plot Row
        layout_column_wrap(
          width = 1/2, gap = "20px", heights_equal = "row",
          div(
            class = "content-card",
            uiOutput(ns("chartTitle3")),
            p(class = "info-text", uiOutput(ns("chartDesc3"))),
            plotlyOutput(ns("rangeChart"), height = "400px")
          ),
          div(
            class = "content-card",
            uiOutput(ns("chartTitle4")),
            p(class = "info-text", uiOutput(ns("chartDesc4"))),
            plotlyOutput(ns("boxPlot"), height = "400px")
          )
        ),
        
        # Moving Average and Volatility
        div(
          class = "content-card",
          style = "margin-top: 20px;",
          uiOutput(ns("chartTitle5")),
          p(class = "info-text", uiOutput(ns("chartDesc5"))),
          plotlyOutput(ns("movingAvg"), height = "400px")
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
sumsMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title",    tr()("sums_title")),
        p (class = "header-subtitle", tr()("sums_subtitle"))
      )
    })
    output$emptyTitle <- renderUI({ HTML(tr()("no_data_title") %||% "No data for selected filters") })
    output$emptySubtitle <- renderUI({ HTML(tr()("no_data_subtitle") %||% "Try expanding your number range or time window.") })
    
    # ---------------------------
    # Common chart theme
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
    render_title("chartTitle1", "📊", "sums_chart_distribution")
    render_desc ("chartDesc1",  "sums_chart_distribution_desc")
    render_title("chartTitle2", "📈", "sums_chart_trend")
    render_desc ("chartDesc2",  "sums_chart_trend_desc")
    render_title("chartTitle3", "🎯", "sums_chart_range")
    render_desc ("chartDesc3",  "sums_chart_range_desc")
    render_title("chartTitle4", "📦", "sums_chart_boxplot")
    render_desc ("chartDesc4",  "sums_chart_boxplot_desc")
    render_title("chartTitle5", "📉", "sums_chart_moving")
    render_desc ("chartDesc5",  "sums_chart_moving_desc")
    
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
    # Sum statistics (guarded)
    # ---------------------------
    sum_stats <- reactive({
      req(has_data())
      d <- filtered_data()
      cols <- paste0("ball_", 1:6)
      req(all(cols %in% names(d)))
      sums <- rowSums(d[, cols, drop = FALSE])
      
      tab <- table(sums)
      mc  <- as.numeric(names(tab)[which.max(tab)])
      
      list(
        sums = sums,
        mean = mean(sums),
        median = stats::median(sums),
        min = min(sums),
        max = max(sums),
        sd  = stats::sd(sums),
        most_common = mc
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
    
    output$metricCard1 <- renderUI({ req(has_data()); s <- sum_stats(); card("📊", round(s$mean, 1), tr()("sums_metric_average")) })
    output$metricCard2 <- renderUI({ req(has_data()); s <- sum_stats(); card("🎯", s$median,      tr()("sums_metric_median")) })
    output$metricCard3 <- renderUI({ req(has_data()); s <- sum_stats(); card("⭐", s$most_common,  tr()("sums_metric_most_common")) })
    output$metricCard4 <- renderUI({ req(has_data()); s <- sum_stats(); card("📉", s$min,         tr()("sums_metric_minimum")) })
    output$metricCard5 <- renderUI({ req(has_data()); s <- sum_stats(); card("📈", s$max,         tr()("sums_metric_maximum")) })
    
    # ---------------------------
    # Cache-key helpers
    # Use options(metrics_data_version) if provided; otherwise fallback
    # to a lightweight signature from current filtered data.
    # ---------------------------
    compute_light_version <- function(d) {
      if (is.null(d) || !nrow(d)) return("empty")
      paste0("rows", nrow(d), "_maxSum", suppressWarnings(max(rowSums(d[, paste0("ball_", 1:6)], na.rm = TRUE))))
      
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
    build_hist <- function(s) {
      sums <- s$sums
      # Histogram meta
      hist_meta <- graphics::hist(sums, breaks = seq(min(sums), max(sums) + 5, by = 5), plot = FALSE)
      max_count <- max(hist_meta$counts)
      dens <- stats::density(sums, adjust = 1.2)
      bin_width <- 5
      dens_scaled <- dens$y * length(sums) * bin_width
      
      plotly::plot_ly() %>%
        plotly::add_histogram(
          x = ~sums,
          xbins = list(size = bin_width),
          marker = list(color = "#8b5cf6", line = list(color = "rgba(255,255,255,0.3)", width = 1.5)),
          name = tr()("sums_label_frequency"),
          hovertemplate = paste0(
            tr()("sums_label_sum_value"), ": %{x}<br>",
            tr()("sums_label_frequency"), ": %{y}<br><extra></extra>"
          )
        ) %>%
        plotly::add_trace(
          x = dens$x,
          y = dens_scaled,
          type = "scatter",
          mode = "lines",
          line = list(color = "#DC143C", width = 3, shape = "spline"),
          name = tr()("sums_density_curve") %||% "Density Curve",
          fill = "tozeroy",
          fillcolor = "rgba(251, 191, 36, 0.15)",
          hovertemplate = paste0(
            tr()("sums_label_sum_value"), ": %{x:.1f}<br>",
            tr()("sums_label_density") %||% "Density", ": %{y:.1f}<extra></extra>"
          )
        ) %>%
        plotly::add_trace(
          x = rep(s$mean, 2),
          y = c(0, max_count * 1.1),
          type = "scatter", mode = "lines",
          line = list(color = "#10b981", width = 3, dash = "dash"),
          name = tr()("sums_metric_average"),
          hovertemplate = paste0(tr()("sums_metric_average"), ": ", round(s$mean, 1), "<extra></extra>")
        ) %>%
        plotly::add_trace(
          x = rep(s$median, 2),
          y = c(0, max_count * 1.1),
          type = "scatter", mode = "lines",
          line = list(color = "#ec4899", width = 3, dash = "dot"),
          name = tr()("sums_metric_median"),
          hovertemplate = paste0(tr()("sums_metric_median"), ": ", s$median, "<extra></extra>")
        ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("sums_label_sum_value"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("sums_label_frequency"), gridcolor = "rgba(255,255,255,0.1)"),
          bargap = 0.1,
          hovermode = "x unified"
        )
    }
    
    build_trend <- function(s) {
      sums <- s$sums
      df <- data.frame(draw = seq_along(sums), sum = sums)
      
      plotly::plot_ly(df, x = ~draw, y = ~sum, type = "scatter", mode = "lines+markers",
                      line = list(color = "#8b5cf6", width = 2),
                      marker = list(color = "#ec4899", size = 6, line = list(color = "rgba(255,255,255,0.5)", width = 1)),
                      hovertemplate = paste0(tr()("sums_label_draw"), " #%{x}<br>", tr()("sums_label_sum_value"), ": %{y}<extra></extra>")
      ) %>%
        plotly::add_trace(x = range(df$draw), y = rep(s$mean, 2),
                          type = "scatter", mode = "lines",
                          line = list(color = "#10b981", width = 2, dash = "dash"),
                          name = tr()("sums_metric_average"),
                          hoverinfo = "skip") %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("sums_label_draw_number"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("sums_label_sum_value"),   gridcolor = "rgba(255,255,255,0.1)"),
          hovermode = "x unified"
        )
    }
    
    build_range <- function(s) {
      sums <- s$sums
      br_from <- floor(min(sums)/10)*10
      br_to   <- ceiling(max(sums)/10)*10 + 10
      ranges  <- cut(sums, breaks = seq(br_from, br_to, by = 10), include.lowest = TRUE)
      ct      <- table(ranges)
      
      df <- data.frame(range = names(ct), count = as.numeric(ct))
      df$percentage <- round(df$count / sum(df$count) * 100, 1)
      
      colors <- grDevices::colorRampPalette(c("#4169E1", "#8b5cf6", "#ec4899", "#DC143C"))(nrow(df))
      
      plotly::plot_ly(df, x = ~range, y = ~count, type = "bar",
                      marker = list(color = colors, line = list(color = "rgba(255,255,255,0.3)", width = 2)),
                      text = ~paste0(count, " (", percentage, "%)"),
                      textposition = "outside",
                      textfont = list(color = "#e8eaed", size = 12),
                      hovertemplate = paste0(
                        "<b>%{x}</b><br>",
                        tr()("sums_hover_count"), ": %{y}<br>",
                        tr()("sums_hover_percentage"), ": %{text}<extra></extra>"
                      )
      ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("sums_label_sum_range"), gridcolor = "rgba(255,255,255,0.1)", tickangle = -45),
          yaxis = list(title = tr()("sums_label_frequency"), gridcolor = "rgba(255,255,255,0.1)"),
          margin = list(b = 100)
        )
    }
    
    build_box <- function(s) {
      plotly::plot_ly(y = ~s$sums, type = "box",
                      marker = list(color = "#8b5cf6"),
                      line   = list(color = "#ec4899", width = 2),
                      fillcolor = "rgba(139, 92, 246, 0.3)",
                      name = tr()("sums_label_sum_value"),
                      boxmean = "sd",
                      hovertemplate = paste0(tr()("sums_hover_value"), ": %{y}<extra></extra>")
      ) %>%
        chart_theme() %>%
        plotly::layout(
          yaxis = list(title = tr()("sums_label_sum_value"), gridcolor = "rgba(255,255,255,0.1)"),
          xaxis = list(title = "", showticklabels = FALSE)
        )
    }
    
    build_moving <- function(s) {
      sums <- s$sums
      df <- data.frame(draw = seq_along(sums), sum = sums)
      win <- min(20, length(sums))
      if (length(sums) >= win) {
        df$ma    <- zoo::rollmean(df$sum, k = win, fill = NA, align = "right")
        df$sd    <- zoo::rollapply(df$sum, width = win, FUN = stats::sd, fill = NA, align = "right")
        df$upper <- df$ma + df$sd
        df$lower <- df$ma - df$sd
      }
      
      p <- plotly::plot_ly(df, x = ~draw)
      p <- plotly::add_trace(p, y = ~sum,  name = tr()("sums_label_sum_value"), type = "scatter", mode = "lines",
                             line = list(color = "rgba(139, 92, 246, 0.4)", width = 1),
                             hovertemplate = paste0(tr()("sums_label_draw"), ": %{x}<br>", tr()("sums_label_sum_value"), ": %{y}<extra></extra>"))
      if ("upper" %in% names(df))
        p <- plotly::add_trace(p, y = ~upper, name = "Upper Band", type = "scatter", mode = "lines",
                               line = list(color = "rgba(236, 72, 153, 0.3)", width = 1, dash = "dot"),
                               hovertemplate = "Upper: %{y:.1f}<extra></extra>")
      if ("lower" %in% names(df))
        p <- plotly::add_trace(p, y = ~lower, name = "Lower Band", type = "scatter", mode = "lines",
                               line = list(color = "rgba(79, 172, 254, 0.3)", width = 1, dash = "dot"),
                               fill = "tonexty", fillcolor = "rgba(139, 92, 246, 0.1)",
                               hovertemplate = "Lower: %{y:.1f}<extra></extra>")
      if ("ma" %in% names(df))
        p <- plotly::add_trace(p, y = ~ma, name = tr()("sums_metric_average"), type = "scatter", mode = "lines",
                               line = list(color = "#10b981", width = 3),
                               hovertemplate = "MA: %{y:.1f}<extra></extra>")
      
      p %>% chart_theme() %>% plotly::layout(
        xaxis = list(title = tr()("sums_label_draw_number"), gridcolor = "rgba(255,255,255,0.1)"),
        yaxis = list(title = tr()("sums_label_sum_value"),   gridcolor = "rgba(255,255,255,0.1)"),
        hovermode = "x unified"
      )
    }
    
    # ---------------------------
    # Cached render helper
    # ---------------------------
    render_cached_plot <- function(output_id, chart_id, builder) {
      output[[output_id]] <- plotly::renderPlotly({
        req(has_data())
        key <- cache_key(chart_id)
        cached <- .sums_cache_get(key)
        if (!is.null(cached)) return(cached)
        s <- sum_stats()
        plt <- builder(s)
        .sums_cache_set(key, plt)
        plt
      })
    }
    
    render_cached_plot("hist",       "hist",    build_hist)
    render_cached_plot("trend",      "trend",   build_trend)
    render_cached_plot("rangeChart", "range",   build_range)
    render_cached_plot("boxPlot",    "box",     build_box)
    render_cached_plot("movingAvg",  "moving",  build_moving)
    
  })
}
