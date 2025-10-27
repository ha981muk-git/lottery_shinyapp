# =============================================================================
# Odd/Even Metric Module — Performance Edition (Global Cache + Empty State)
# =============================================================================

# -----------------------------------------------------------------------------
# OPTIONAL: Global cache invalidation helper for the hosting app.
# Call this after your data source refreshes (e.g., nightly ETL).
# Or set options(metrics_data_version = "<new-version>") to rotate namespaces.
# -----------------------------------------------------------------------------
odds_evens_clear_cache <- function() {
  if (exists(".odds_evens_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".odds_evens_plot_cache", envir = .GlobalEnv, inherits = FALSE)
  }
  invisible(TRUE)
}

# Global cache env (shared between users)
.odds_evens_get_cache_env <- function() {
  if (!exists(".odds_evens_plot_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".odds_evens_plot_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".odds_evens_plot_cache", envir = .GlobalEnv, inherits = FALSE)
}

.odds_evens_cache_get <- function(key) {
  env <- .odds_evens_get_cache_env()
  if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else NULL
}

.odds_evens_cache_set <- function(key, value) {
  env <- .odds_evens_get_cache_env()
  assign(key, value, envir = env)
  invisible(value)
}

# -----------------------------------------------------------------------------
# UI — uses conditionalPanel via output$hasData to show charts or empty state
# -----------------------------------------------------------------------------
oddsEvensMetricUI <- function(id) {
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
oddsEvensMetricServer <- function(id, filtered_data) {
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
        h1(class = "header-title",    tr()("odds_evens_title")),
        p (class = "header-subtitle", tr()("odds_evens_subtitle"))
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
    render_title("chartTitle1", "📊", "odds_evens_chart_pascal")
    render_desc ("chartDesc1",  "odds_evens_chart_pascal_desc")
    render_title("chartTitle2", "🥧", "odds_evens_chart_pie")
    render_title("chartTitle3", "📈", "odds_evens_chart_trend")
    render_title("chartTitle4", "📊", "odds_evens_chart_stacked")
    
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
    # Stats (guarded + defensive)
    # ---------------------------
    odds_evens_stats <- reactive({
      req(has_data())
      d <- filtered_data()
      cols <- paste0("ball_", 1:6)
      req(all(cols %in% names(d)))
      
      # Defensive subsetting
      balls <- d[, cols, drop = FALSE]
      # Compute odds/evens per draw
      odds  <- rowSums(balls %% 2 == 1, na.rm = TRUE)
      evens <- 6 - odds
      
      combinations <- paste0(odds, " ", tr()("odds_evens_label_odds"), " / ", evens, " ", tr()("odds_evens_label_evens"))
      tab <- table(combinations)
      most_common <- names(tab)[which.max(tab)]
      
      list(
        odds = odds,
        evens = evens,
        combinations = combinations,
        total_odds = sum(odds,  na.rm = TRUE),
        total_evens = sum(evens, na.rm = TRUE),
        avg_odds = mean(odds,   na.rm = TRUE),
        most_common = most_common
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
    
    output$metricCard1 <- renderUI({
      req(has_data()); s <- odds_evens_stats()
      card("🎲", round(s$avg_odds, 2), tr()("odds_evens_metric_avg_odds"))
    })
    output$metricCard2 <- renderUI({
      req(has_data()); s <- odds_evens_stats()
      card("⚖️", round(6 - s$avg_odds, 2), tr()("odds_evens_metric_avg_evens"))
    })
    output$metricCard3 <- renderUI({
      req(has_data()); s <- odds_evens_stats()
      div(
        class = "value-box-custom",
        div(class = "value-box-icon", "⭐"),
        div(class = "value-box-value", style = "font-size: 20px;", s$most_common),
        div(class = "value-box-label", tr()("odds_evens_metric_most_common"))
      )
    })
    
    # ---------------------------
    # Cache-key helpers
    # Use options(metrics_data_version) if provided; otherwise fallback
    # to a lightweight signature from current filtered data.
    # ---------------------------
    compute_light_version <- function(d) {
      if (is.null(d) || !nrow(d)) return("empty")
      cols <- paste0("ball_", 1:6)
      cols <- cols[cols %in% names(d)]
      if (!length(cols)) return(paste0("rows", nrow(d), "_meanOdds0"))
      odds_cnt <- rowSums(d[, cols, drop = FALSE] %% 2 == 1, na.rm = TRUE)
      paste0("rows", nrow(d), "_meanOdds", sprintf("%.3f", suppressWarnings(mean(odds_cnt, na.rm = TRUE))))
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
    build_pascal <- function(s) {
      # Count frequency of each combination
      combo_counts <- table(s$combinations)
      combo_df <- data.frame(
        combination = names(combo_counts),
        count = as.numeric(combo_counts),
        stringsAsFactors = FALSE
      )
      
      # Try to extract the numeric "odds" at the start (robust to translation)
      # Pattern: leading integer
      odds_num <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", combo_df$combination)))
      combo_df$odds_num <- odds_num
      combo_df <- combo_df[order(combo_df$odds_num), ]
      
      # Gradient palette across 7 possible odds counts (0..6)
      colors <- c("#00CED1", "#4169E1", "#9370DB", "#8b5cf6", "#ec4899", "#DC143C", "#FFD700")
      
      combo_df$percentage <- round(combo_df$count / sum(combo_df$count) * 100, 1)
      
      plotly::plot_ly(
        combo_df,
        x = ~reorder(combination, odds_num),
        y = ~count,
        type = "bar",
        marker = list(
          color = colors[pmin(7, pmax(1, odds_num + 1))],
          line = list(color = "rgba(255, 255, 255, 0.3)", width = 2)
        ),
        text = ~paste0(count, " ", tr()("odds_evens_hover_draws"), "<br>", percentage, "%"),
        textposition = "outside",
        textfont = list(color = "#e8eaed", size = 14, family = "Inter"),
        hovertemplate = paste0(
          "<b>%{x}</b><br>",
          tr()("odds_evens_label_frequency"), ": %{y} ", tr()("odds_evens_hover_draws"), "<br>",
          tr()("odds_evens_hover_percentage"), ": %{text}<br>",
          "<extra></extra>"
        )
      ) %>%
        chart_theme() %>%
        plotly::layout(
          xaxis = list(
            title = tr()("odds_evens_label_combination"),
            gridcolor = "rgba(255, 255, 255, 0.1)",
            tickangle = -45,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = tr()("odds_evens_label_frequency"),
            gridcolor = "rgba(255, 255, 255, 0.1)"
          ),
          margin = list(b = 100, t = 40)
        )
    }
    
    build_pie <- function(s) {
      df <- data.frame(
        category = c(tr()("odds_evens_label_odds"), tr()("odds_evens_label_evens")),
        count = c(s$total_odds, s$total_evens)
      )
      plotly::plot_ly(
        df,
        labels = ~category,
        values = ~count,
        type = "pie",
        marker = list(colors = c("#ec4899", "#4169E1"),
                      line = list(color = "#FFFFFF", width = 2)),
        textinfo = "label+percent",
        textfont = list(size = 16, color = "#FFFFFF", family = "Inter"),
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          tr()("odds_evens_label_count"), ": %{value}<br>",
          tr()("odds_evens_hover_percentage"), ": %{percent}<br>",
          "<extra></extra>"
        )
      ) %>%
        chart_theme() %>%
        plotly::layout(
          showlegend = TRUE,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
        )
    }
    
    build_trend <- function(s) {
      df <- data.frame(
        draw  = seq_along(s$odds),
        odds  = s$odds,
        evens = s$evens
      )
      
      # Moving average window
      window_size <- min(20, nrow(df))
      if (nrow(df) >= window_size) {
        df$ma_odds <- zoo::rollmean(df$odds, k = window_size, fill = NA, align = "right")
      }
      
      p <- plotly::plot_ly(df, x = ~draw) %>%
        plotly::add_trace(
          y = ~odds, name = tr()("odds_evens_label_odds"), type = "scatter", mode = "lines",
          line = list(color = "#ec4899", width = 2),
          hovertemplate = paste0(tr()("odds_evens_label_draw_number"), ": %{x}<br>", tr()("odds_evens_label_odds"), ": %{y}<extra></extra>")
        ) %>%
        plotly::add_trace(
          y = ~evens, name = tr()("odds_evens_label_evens"), type = "scatter", mode = "lines",
          line = list(color = "#4169E1", width = 2),
          hovertemplate = paste0(tr()("odds_evens_label_draw_number"), ": %{x}<br>", tr()("odds_evens_label_evens"), ": %{y}<extra></extra>")
        )
      
      if ("ma_odds" %in% names(df)) {
        p <- plotly::add_trace(
          p, y = ~ma_odds, name = paste0("MA (", tr()("odds_evens_label_odds"), ")"),
          type = "scatter", mode = "lines",
          line = list(color = "#8b5cf6", width = 3, dash = "dash"),
          hovertemplate = paste0(tr()("odds_evens_label_draw_number"), ": %{x}<br>MA: %{y:.2f}<extra></extra>")
        )
      }
      
      p %>% chart_theme() %>%
        plotly::layout(
          xaxis = list(title = tr()("odds_evens_label_draw_number"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("odds_evens_label_count"),       gridcolor = "rgba(255,255,255,0.1)"),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
          yaxis2 = list(range = c(0, 6)) # visual hint; main axis still used
        )
    }
    
    build_stacked <- function(s) {
      n <- length(s$odds)
      n_draws <- min(50, n)
      start_idx <- max(1, n - n_draws + 1)
      
      df <- data.frame(
        draw  = start_idx:n,
        odds  = s$odds[start_idx:n],
        evens = s$evens[start_idx:n]
      )
      
      plotly::plot_ly(df, x = ~draw) %>%
        plotly::add_trace(
          y = ~odds, name = tr()("odds_evens_label_odds"), type = "bar",
          marker = list(color = "#ec4899"),
          hovertemplate = paste0(tr()("odds_evens_label_draw_number"), ": %{x}<br>", tr()("odds_evens_label_odds"), ": %{y}<extra></extra>")
        ) %>%
        plotly::add_trace(
          y = ~evens, name = tr()("odds_evens_label_evens"), type = "bar",
          marker = list(color = "#4169E1"),
          hovertemplate = paste0(tr()("odds_evens_label_draw_number"), ": %{x}<br>", tr()("odds_evens_label_evens"), ": %{y}<extra></extra>")
        ) %>%
        chart_theme() %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = tr()("odds_evens_label_draw_number"), gridcolor = "rgba(255,255,255,0.1)"),
          yaxis = list(title = tr()("odds_evens_label_count"),       gridcolor = "rgba(255,255,255,0.1)", range = c(0, 6)),
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
        cached <- .odds_evens_cache_get(key)
        if (!is.null(cached)) return(cached)
        s <- odds_evens_stats()
        plt <- builder(s)
        .odds_evens_cache_set(key, plt)
        plt
      })
    }
    
    # Wire up cached plots
    render_cached_plot("pascalChart", "pascal", build_pascal)
    render_cached_plot("pie",         "pie",     build_pie)
    render_cached_plot("trendLine",   "trend",   build_trend)
    render_cached_plot("stacked",     "stacked", build_stacked)
    
  })
}
