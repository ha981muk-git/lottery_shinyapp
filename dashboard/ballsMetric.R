# =============================================================================
# balls Metrics UI with Translation Support — Performance Edition (Version C)
# Global chart caching + empty-state handling + deduped layout code
# =============================================================================

# Public helper: return total row count of your base dataset
get_row_count <- function() {
  nrow(data_loader$load())
}

# -----------------------------------------------------------------------------
# OPTIONAL: Global cache invalidation helper for the hosting app.
# Call this after your data source refreshes (e.g., a nightly ETL).
# You can also update options(metrics_data_version = "<new-version>")
# and the module will segregate caches automatically by version.
# -----------------------------------------------------------------------------
balls_clear_cache <- function() {
  if (exists(".balls_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".balls_plot_cache", envir = .GlobalEnv, inherits = FALSE)
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Global shared cache accessors (persist across sessions)
# Keyed by (version, weeks, range, lang, chart_id)
# -----------------------------------------------------------------------------
.balls_get_cache_env <- function() {
  if (!exists(".balls_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".balls_plot_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".balls_plot_cache", envir = .GlobalEnv, inherits = FALSE)
}

.balls_cache_get <- function(key) {
  env <- .balls_get_cache_env()
  if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL
}

.balls_cache_set <- function(key, value) {
  env <- .balls_get_cache_env()
  assign(key, value, envir = env)
  invisible(value)
}

# -----------------------------------------------------------------------------
# UI
# - Uses conditionalPanel on output$hasData to show either charts or empty state
# -----------------------------------------------------------------------------
ballsMetricUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "padding: 20px;",
      
      # Header (title + subtitle)
      div(style = "margin-bottom: 32px;", uiOutput(ns("header"))),
      
      # WHEN DATA EXISTS -------------------------------------------------------
      conditionalPanel(
        condition = sprintf("output['%s']", ns("hasData")),
        layout_column_wrap(
          width = 1/4,
          heights_equal = "row",
          uiOutput(ns("metricCard1")),
          uiOutput(ns("metricCard2")),
          uiOutput(ns("metricCard3")),
          uiOutput(ns("metricCard4"))
        ),
        layout_column_wrap(
          width = 1/2,
          heights_equal = "row",
          div(
            class = "chart-card",
            uiOutput(ns("trendChartTitle")),
            plotlyOutput(ns("trendChart"), height = "350px")
          ),
          div(
            class = "chart-card",
            uiOutput(ns("distributionChartTitle")),
            plotlyOutput(ns("distributionChart"), height = "350px")
          )
        ),
        div(
          class = "chart-card",
          style = "margin-top: 20px;",
          uiOutput(ns("densityChartTitle")),
          plotlyOutput(ns("densityChart"), height = "400px")
        ),
        div(
          class = "chart-card",
          style = "margin-top: 20px;",
          uiOutput(ns("overviewChartTitle")),
          plotlyOutput(ns("overviewChart"), height = "400px")
        ),
        div(
          class = "chart-card",
          style = "margin-top: 20px;",
          uiOutput(ns("lineChartTitle")),
          plotlyOutput(ns("lineChart"), height = "400px")
        )
      ),
      
      # WHEN NO DATA -----------------------------------------------------------
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
# - Shared chart theme / helpers
# - Global shared plot cache (keys include a data version)
# - Empty state flow (hide charts, show friendly message)
# -----------------------------------------------------------------------------
ballsMetricServer <- function(id, filtered_data, input_controls) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------
    # i18n
    # ---------------------------
    get_lang <- reactive({
      query <- parseQueryString(isolate(session$clientData$url_search))
      query$lang %||% "de"
    })
    
    # Translation shim for convenience
    tr <- reactive({
      lang <- get_lang()
      function(key) t(key, lang)
    })
    
    # ---------------------------
    # Colors (stable mapping)
    # ---------------------------
    ball_colors <- c(
      "Ball 1" = "#4169E1",  # royal blue
      "Ball 2" = "#DC143C",  # crimson red
      "Ball 3" = "#32CD32",  # lime green
      "Ball 4" = "#FFD700",  # gold/yellow
      "Ball 5" = "#9370DB",  # medium purple
      "Ball 6" = "#00CED1"   # dark cyan
    )
    
    # ---------------------------
    # Shared chart theme helper
    # ---------------------------
    chart_theme <- function(p, title_txt) {
      p %>%
        plotly::layout(
          title = title_txt,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font = list(color = "rgba(255,255,255,0.6)")
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }
    
    # Title render helper
    render_title <- function(id, key) {
      output[[id]] <- renderUI({
        div(class = "chart-title", tr()(key))
      })
    }
    
    # ---------------------------
    # Header + Empty-state texts
    # ---------------------------
    output$header <- renderUI({
      tagList(
        h1(class = "header-title",   tr()("balls_title")),
        p (class = "header-subtitle", tr()("balls_subtitle"))
      )
    })
    
    output$emptyTitle <- renderUI({
      # Use a dedicated string if available; fallback to inline
      HTML(tr()("no_data_title") %||% "No data for selected filters")
    })
    
    output$emptySubtitle <- renderUI({
      HTML(tr()("no_data_subtitle") %||% "Try expanding your number range or time window.")
    })
    
    # Render chart titles
    render_title("trendChartTitle",         "balls_trend_title")
    render_title("distributionChartTitle",  "balls_distribution_title")
    render_title("densityChartTitle",       "balls_chart_density")
    render_title("overviewChartTitle",      "balls_overview_title")
    render_title("lineChartTitle",          "balls_line_chart_title")
    
    # ---------------------------
    # Has data? (export to UI)
    # ---------------------------
    has_data <- reactive({
      d <- filtered_data()
      !is.null(d) && nrow(d) > 0
    })
    
    output$hasData <- reactive({ has_data() })
    # IMPORTANT for conditionalPanel to react
    outputOptions(output, "hasData", suspendWhenHidden = FALSE)
    
    # ---------------------------
    # Metric Cards (only compute if data exists)
    # ---------------------------
    create_metric_card <- function(title, value, suffix = "") {
      div(
        class = "metric-card",
        div(class = "metric-label", title),
        div(class = "metric-value", paste0(value, suffix))
      )
    }
    
    output$metricCard1 <- renderUI({
      req(has_data())
      d <- filtered_data()
      sel <- nrow(d)
      total <- get_row_count()
      cov  <- if (isTRUE(total > 0)) (sel / total) * 100 else 0
      create_metric_card(
        tr()("balls_metric_coverage"),
        format(round(cov), big.mark = ","),
        "%"
      )
    })
    
    output$metricCard2 <- renderUI({
      req(has_data())
      d <- filtered_data()
      create_metric_card(
        tr()("balls_metric_occurrence"),
        format(round(nrow(d)), big.mark = ",")
      )
    })
    
    output$metricCard3 <- renderUI({
      req(has_data())
      d <- filtered_data()
      # Constant: combinations of 6 from 49 = 13,983,816
      N_Tickets <- 13983816
      chance <- (nrow(d) / N_Tickets) * 100
      create_metric_card(
        tr()("balls_metric_chance"),
        format(round(chance, 2), nsmall = 2),
        "%"
      )
    })
    
    output$metricCard4 <- renderUI({
      req(has_data())
      rng <- input_controls()$range
      num_from <- as.numeric(rng[1])
      num_to   <- as.numeric(rng[2])
      create_metric_card(
        tr()("balls_metric_range"),
        (num_to - num_from + 1)
      )
    })
    
    # ---------------------------
    # Cache key helpers
    # data_version: use options(metrics_data_version) if provided by host app.
    # This lets you invalidate globally when data updates without digesting data.
    # Fallback version uses row count & max of numeric columns as a light fingerprint.
    # ---------------------------
    compute_light_version <- function(d) {
      if (is.null(d) || !nrow(d)) return("empty")
      nc <- ncol(d)
      max_num <- suppressWarnings({
        suppressWarnings(max(unlist(lapply(d, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_)), na.rm = TRUE))
      })
      max_date <- if ("datum" %in% names(d) && inherits(d$datum, "Date")) {
        as.character(max(d$datum, na.rm = TRUE))
      } else ""
      paste0("rows", nrow(d), "_cols", nc, "_max", max_num, "_date", max_date)
    }
    
    cache_key <- function(chart_id) {
      rng   <- input_controls()$range %||% c(1, 49)
      weeks <- as.numeric(input_controls()$timeRange %||% 7)
      lang  <- get_lang()
      data_version <- getOption("metrics_data_version", NULL)
      
      # We want a version that changes only when the *underlying* dataset changes.
      # If host app didn't set a global version, fallback to a lightweight signature
      # based on current filtered data shape. (Okay trade-off in practice.)
      if (is.null(data_version)) {
        d <- filtered_data()
        data_version <- compute_light_version(d)
      }
      
      sprintf("v=%s|w=%s|r=%s-%s|lang=%s|chart=%s",
              data_version, weeks, rng[1], rng[2], lang, chart_id)
    }
    
    # ---------------------------
    # Plot builders (return plotly objects)
    # ---------------------------
    build_box_plot <- function(d) {
      p <- plotly::plot_ly()
      for (i in 1:6) {
        ball_name  <- paste0("Ball ", i)
        ball_label <- tr()(paste0("ball_", i))
        p <- plotly::add_trace(
          p,
          x = ball_label,
          y = d[[paste0("ball_", i)]],
          type = "box",
          name = ball_label,
          fillcolor = ball_colors[ball_name],
          marker    = list(color = ball_colors[ball_name]),
          line      = list(color = ball_colors[ball_name])
        )
      }
      chart_theme(
        p %>% plotly::layout(
          xaxis = list(title = tr()("ball_label"),  color = "rgba(255,255,255,0.6)"),
          yaxis = list(title = tr()("value_label"), color = "rgba(255,255,255,0.6)"),
          showlegend = TRUE
        ),
        title_txt = tr()("balls_boxplot_title")
      )
    }
    
    build_violin_plot <- function(d) {
      p <- plotly::plot_ly()
      for (i in 1:6) {
        ball_name  <- paste0("Ball ", i)
        ball_label <- tr()(paste0("ball_", i))
        p <- plotly::add_trace(
          p,
          x = ball_label,
          y = d[[paste0("ball_", i)]],
          type = "violin",
          name = ball_label,
          side = "both",
          box = list(
            visible   = TRUE,
            fillcolor = plotly::toRGB(ball_colors[ball_name], alpha = 0.3),
            line      = list(color = ball_colors[ball_name], width = 2)
          ),
          meanline = list(visible = TRUE),
          fillcolor = plotly::toRGB(ball_colors[ball_name], alpha = 0.6),
          line      = list(color = ball_colors[ball_name]),
          opacity   = 0.6
        )
      }
      chart_theme(
        p %>% plotly::layout(
          xaxis = list(title = tr()("ball_label"),  color = "rgba(255,255,255,0.6)"),
          yaxis = list(title = tr()("value_label"), color = "rgba(255,255,255,0.6)"),
          showlegend = TRUE
        ),
        title_txt = tr()("balls_violin_title")
      )
    }
    
    build_density_plot <- function(d) {
      df_long <- d %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("ball_"),
          names_to = "ball",
          values_to = "value"
        ) %>%
        dplyr::mutate(ball = stringr::str_replace(.data$ball, "ball_", "Ball "))
      
      p <- plotly::plot_ly()
      for (ball_name in unique(df_long$ball)) {
        vals <- df_long$value[df_long$ball == ball_name]
        if (!length(vals)) next
        dens <- stats::density(vals)
        p <- plotly::add_trace(
          p,
          x = dens$x,
          y = dens$y,
          type = "scatter",
          mode = "lines",
          name = paste(ball_name, "(Smooth)"),
          line = list(color = ball_colors[ball_name], width = 2)
        )
      }
      chart_theme(
        p %>% plotly::layout(
          xaxis = list(title = tr()("value_label"), color = "rgba(255,255,255,0.6)"),
          yaxis = list(title = tr()("ball_label"),  color = "rgba(255,255,255,0.6)"),
          legend = list(orientation = "h", y = -0.2)
        ),
        title_txt = tr()("balls_chart_density_title")
      )
    }
    
    build_raincloud_plot <- function(d) {
      p <- plotly::plot_ly()
      ball_labels <- vapply(1:6, function(i) tr()(paste0("ball_", i)), character(1))
      for (i in 1:6) {
        ball_name   <- paste0("Ball ", i)
        ball_label  <- ball_labels[i]
        ball_values <- d[[paste0("ball_", i)]]
        color <- ball_colors[ball_name]
        
        # Half violin
        p <- plotly::add_trace(
          p,
          x = rep(i, length(ball_values)),
          y = ball_values,
          type = "violin",
          side = "positive",
          width = 0.6,
          fillcolor = plotly::toRGB(color, alpha = 0.5),
          line = list(color = color),
          opacity = 0.6,
          points = FALSE,
          showlegend = FALSE,
          name = ball_label
        )
        
        # Centered box
        p <- plotly::add_trace(
          p,
          x = rep(i, length(ball_values)),
          y = ball_values,
          type = "box",
          fillcolor = plotly::toRGB(color, alpha = 0.8),
          line = list(color = color, width = 2),
          boxpoints = FALSE,
          width = 0.3,
          name = ball_label,
          showlegend = FALSE
        )
        
        # Jittered points (left)
        set.seed(42 + i)
        jitter_amount <- stats::runif(length(ball_values), -0.35, -0.05)
        x_jittered <- rep(i, length(ball_values)) + jitter_amount
        
        p <- plotly::add_trace(
          p,
          x = x_jittered,
          y = ball_values,
          type = "scatter",
          mode = "markers",
          marker = list(color = color, size = 4, opacity = 0.6),
          showlegend = FALSE,
          hoverinfo = "y",
          name = ball_label
        )
      }
      
      chart_theme(
        p %>% plotly::layout(
          yaxis = list(title = tr()("value_label"), color = "rgba(255,255,255,0.6)"),
          xaxis = list(
            title = tr()("ball_label"),
            color = "rgba(255,255,255,0.6)",
            tickmode = "array",
            tickvals = 1:6,
            ticktext = ball_labels,
            range = c(0.5, 6.5)
          ),
          showlegend = FALSE
        ),
        title_txt = tr()("balls_raincloud_title")
      )
    }
    
    build_line_plot <- function(d) {
      p <- plotly::plot_ly()
      x_positions <- 1:6
      ball_labels <- vapply(1:6, function(i) tr()(paste0("ball_", i)), character(1))
      
      # NOTE: For large n, consider sampling recent rows to avoid heavy DOM.
      nr <- nrow(d)
      for (row in 1:nr) {
        y_values <- as.numeric(d[row, paste0("ball_", 1:6), drop = TRUE])
        p <- plotly::add_trace(
          p,
          x = x_positions,
          y = y_values,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "rgba(255,255,255,0.30)", width = 1),
          marker = list(size = 4, color = "rgba(255,255,255,0.4)"),
          hoverinfo = "y+x",
          showlegend = FALSE,
          name = paste("Row", row)
        )
      }
      
      chart_theme(
        p %>% plotly::layout(
          xaxis = list(
            title = tr()("ball_label"),
            color = "rgba(255,255,255,0.6)",
            tickmode = "array",
            tickvals = x_positions,
            ticktext = ball_labels,
            range = c(0.5, 6.5)
          ),
          yaxis = list(
            title = tr()("value_label"),
            color = "rgba(255,255,255,0.6)"
          ),
          hovermode = "closest"
        ),
        title_txt = tr()("balls_line_chart")
      )
    }
    
    # ---------------------------
    # Renderers with GLOBAL CACHE
    # ---------------------------
    render_cached_plot <- function(output_id, chart_id, builder_fn) {
      output[[output_id]] <- plotly::renderPlotly({
        req(has_data())
        key <- cache_key(chart_id)
        cached <- .balls_cache_get(key)
        if (!is.null(cached)) return(cached)
        d <- filtered_data()
        req(d, nrow(d) > 0)
        plt <- builder_fn(d)
        .balls_cache_set(key, plt)
        plt
      })
    }
    
    render_cached_plot("trendChart",        "box",       build_box_plot)
    render_cached_plot("distributionChart", "violin",    build_violin_plot)
    render_cached_plot("densityChart",      "density",   build_density_plot)
    render_cached_plot("overviewChart",     "raincloud", build_raincloud_plot)
    render_cached_plot("lineChart",         "lines",     build_line_plot)
    
    # ---------------------------
    # Invalidate charts on refresh for current metric only (if host uses it)
    # Here we conservatively clear all balls* charts for current filter key.
    # Host app can also call balls_clear_cache() after data updates.
    # ---------------------------
    observeEvent(input_controls()$refresh, ignoreInit = TRUE, {
      # Rebuild cache keys for current state and clear them
      ids <- c("box", "violin", "density", "raincloud", "lines")
      keys <- vapply(ids, function(cid) cache_key(cid), character(1))
      env <- .balls_get_cache_env()
      rm(list = intersect(ls(envir = env, all.names = TRUE), keys), envir = env)
      invisible(TRUE)
    })
    
  })
}
