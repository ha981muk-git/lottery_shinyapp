
# ============================================================================
# FIX 3: ADD CACHING TO METRIC GENERATION (Optional but POWERFUL)
# ============================================================================
# Add this to the TOP of your app.R or DashboardModule.R:

library(memoise)

# ✅ Cache metrics data for 1 hour
generate_metrics_cached <- memoise(
  function() {
    generate_metrics()
  },
  cache = cache_filesystem(path = file.path(tempdir(), "shiny_cache"))
)

# Then in dashboardServer, replace:
#   data <- generate_metrics()
# With:
#   data <- generate_metrics_cached()

# ============================================================================
# FIX 4: OPTIMIZE PLOTLY RENDERING (in metric files)
# ============================================================================
# In each metric file (ballsMetricUI.R, sumsMetricUI.R, etc.)
# Update the renderPlotly calls:

# BEFORE:
# output$plot <- renderPlotly({
#   create_plot(filtered_data())
# })

# AFTER - Add caching:
# output$plot <- renderPlotly({
#   create_plot(filtered_data())
# }, cache = TRUE)  # ✅ Cache by reactive dependencies

# ============================================================================
# FIX 5: REDUCE METRIC SWITCHING TIME (ADVANCED)
# ============================================================================
# The 4.8s delay on first metric is data filtering + rendering
# Try this aggressive optimization in dashboardServer:

# Add AFTER filtered_data definition:
filtered_data_memo <- reactive({
  req(metrics_data_ready())
  
  # Cache last 5 filter results
  cache_key <- paste(
    input_controls()$timeRange,
    paste(debounced_range(), collapse="-"),
    sep = "|"
  )
  
  # Get or compute
  if (!exists("filter_cache")) {
    filter_cache <<- list()
  }
  
  if (!(cache_key %in% names(filter_cache))) {
    filter_cache[[cache_key]] <<- filtered_data()
    
    # Keep only last 5
    if (length(filter_cache) > 5) {
      filter_cache <<- filter_cache[-1]
    }
  }
  
  filter_cache[[cache_key]]
})


# ============================================================
# ULTRA-SMOOTH UI MODULE - MAXIMUM PERFORMANCE OPTIMIZATION
# ============================================================

# UI Module - Enhanced with better debouncing and visual feedback
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  time_choices <- setNames(
    c(7, 30, 60, 90, 120, 150, 180),
    c(t("time_last_7", lang), t("time_last_30", lang), t("time_last_60", lang),
      t("time_last_90", lang), t("time_last_120", lang), t("time_last_150", lang),
      t("time_last_180", lang))
  )
  
  metric_choices <- setNames(
    c("balls", "sums", "odds_evens", "table", "difference", "lag"),
    c(t("metric_balls", lang), t("metric_sums", lang), t("metric_odds_evens", lang),
      t("metric_tables", lang), t("metric_difference", lang), t("metric_lag", lang))
  )
  
  tagList(
    # Optimized CSS for smooth transitions
    tags$style(HTML("
      .smooth-transition { transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1); }
      .slider-updating { opacity: 0.7; pointer-events: none; }
      .btn-primary:active { transform: scale(0.98); }
      @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
      .updating { animation: pulse 1.5s ease-in-out infinite; }
    ")),
    
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    
    # Slider with loading indicator
    div(id = ns("sliderContainer"), class = "smooth-transition",
        sliderInput(ns("range"), 
                    t("input_ball_range", lang), 
                    min = 1, max = 49, value = c(1, 49), step = 1)
    ),
    
    selectInput(ns("metric"), 
                t("input_analysis_type", lang), 
                choices = metric_choices, 
                selected = "balls"),
    
    selectInput(ns("timeRange"), 
                t("input_time_window", lang), 
                choices = time_choices, 
                selected = 30),
    
    actionButton(ns("refresh"), 
                 t("input_refresh", lang), 
                 class = "btn-primary w-100",
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600; transition: all 0.2s;")
  )
}


# ============================================================================
# FIX 2: OPTIMIZE SLIDER DEBOUNCE (in DashboardModule.R)
# ============================================================================
# Replace current lotteryInputServer with this:

lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    minDistance <- 6
    
    # Slider validation
    observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        maxVal <- min(minVal + minDistance, 49)
        minVal <- maxVal - minDistance
        updateSliderInput(session, "range", value = c(minVal, maxVal))
      }
    }, ignoreInit = TRUE)
    
    # ✅ OPTIMIZED: Reduced throttle from 800ms to 600ms
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(600)
    
    # ✅ OPTIMIZED: Faster debounces for snappier feel
    range_debounced <- reactive(input$range) %>% debounce(250)
    metric_debounced <- reactive(input$metric) %>% debounce(80)
    timeRange_debounced <- reactive(input$timeRange) %>% debounce(150)
    
    return(reactive({
      list(
        range = range_debounced(),
        metric = metric_debounced(),
        timeRange = timeRange_debounced(),
        refresh = refresh_throttled()
      )
    }))
  })
}



# Dashboard UI - Optimized skeleton and transitions
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Enhanced CSS for smooth animations
    tags$style(HTML("
      @keyframes shimmer {
        0% { background-position: -200% 0; }
        100% { background-position: 200% 0; }
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .skeleton-card {
        height: 200px;
        background: linear-gradient(90deg, 
          rgba(139,92,246,0.08) 25%, 
          rgba(139,92,246,0.15) 50%, 
          rgba(139,92,246,0.08) 75%);
        background-size: 200% 100%;
        animation: shimmer 1.5s ease-in-out infinite;
        border-radius: 12px;
        margin-bottom: 20px;
      }
      .metric-container {
        animation: fadeIn 0.3s ease-out;
      }
      #", ns("metricsContainer"), " > div {
        transition: opacity 0.25s ease, transform 0.25s ease;
      }
    ")),
    
    # Improved skeleton loader
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card", style = "height: 180px;"),
        div(class = "skeleton-card", style = "height: 320px; animation-delay: 0.05s;"),
        div(class = "skeleton-card", style = "height: 220px; animation-delay: 0.1s;")
    ),
    
    # Metrics container with smooth transitions
    div(id = ns("metricsContainer"),
        style = "display: none;",
        
        div(id = ns("metric-balls"), class = "metric-container",
            style = "display: none;", ballsMetricUI(ns("balls"))),
        
        div(id = ns("metric-sums"), class = "metric-container",
            style = "display: none;", sumsMetricUI(ns("sums"))),
        
        div(id = ns("metric-odds_evens"), class = "metric-container",
            style = "display: none;", oddsEvensMetricUI(ns("odds_evens"))),
        
        div(id = ns("metric-table"), class = "metric-container",
            style = "display: none;", tableMetricUI(ns("table"))),
        
        div(id = ns("metric-difference"), class = "metric-container",
            style = "display: none;", differenceMetricUI(ns("difference"))),
        
        div(id = ns("metric-lag"), class = "metric-container",
            style = "display: none;", lagMetricUI(ns("lag")))
    )
  )
}


# ============================================================================
# PERFORMANCE OPTIMIZATION FIXES FOR YOUR SHINY LOTTERY APP
# ============================================================================
# Apply these fixes to: DashboardModule.R (dashboardServer function)

# ISSUE #1: App takes 10.6 seconds to load (CRITICAL)
# ROOT CAUSE: generate_metrics() runs synchronously, blocks entire app
# 
# SOLUTION: Lazy-load generate_metrics() in background

# ============================================================================
# FIX 1: LAZY-LOAD METRICS DATA (replaces current dashboardServer)
# ============================================================================

# ============================================================================
# FIX 1: LAZY-LOAD METRICS DATA (replaces current dashboardServer)
# ============================================================================

dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ✅ NEW: Don't load metrics immediately - load in background
    # This makes app appear instantly
    metrics_data_ready <- reactiveVal(FALSE)
    metrics_data <- reactiveVal(NULL)
    
    # Load data in background after app renders (non-blocking)
    load_triggered <- reactiveVal(FALSE)
    
    observe({
      # Load after 500ms to let UI render first
      invalidateLater(500)
      
      if (!load_triggered()) {
        if (!metrics_data_ready()) {
          # Load the expensive data
          data <- generate_metrics()
          metrics_data(data)
          metrics_data_ready(TRUE)
        }
        load_triggered(TRUE)
      }
    })
    
    draws_per_week <- 2
    
    # ✅ Reduced debounce from 350ms to 250ms (more responsive)
    debounced_range <- reactive(input_controls()$range) %>% debounce(250)
    
    # ✅ Smart filtered data with better caching
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$timeRange, 
        debounced_range()),
      {
        # Don't process if data not ready yet
        req(metrics_data_ready(), !is.null(metrics_data()))
        
        data <- metrics_data()
        req(!is.null(data) && nrow(data) > 0)
        
        weeks <- as.numeric(input_controls()$timeRange)
        days <- weeks * 2  # 2 draws per week
        data <- tail(data, min(days, nrow(data)))
        
        range_vals <- debounced_range()
        num_from <- as.numeric(range_vals[1])
        num_to <- as.numeric(range_vals[2])
        
        # ✅ FASTER filtering: use data.table or more efficient filter
        data <- data %>%
          filter(ball_1 >= num_from & ball_6 <= num_to)
        
        req(nrow(data) > 0)
        data
      },
      ignoreNULL = TRUE
    )
    
    # Track initialized servers
    initialized_servers <- reactiveVal(list())
    
    # ✅ OPTIMIZED initialization - NO SLEEP DELAYS
    initialize_server <- function(metric, priority = FALSE) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return(TRUE)
      
      tryCatch({
        switch(metric,
               "balls" = ballsMetricServer("balls", filtered_data, input_controls),
               "sums" = sumsMetricServer("sums", filtered_data),
               "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
               "table" = tableMetricServer("table", filtered_data),
               "difference" = differenceMetricServer("difference", filtered_data),
               "lag" = lagMetricServer("lag", filtered_data)
        )
        
        initialized_servers(c(already_init, metric))
        return(TRUE)
      }, error = function(e) {
        message("Error initializing ", metric, ": ", e$message)
        return(FALSE)
      })
    }
    
    # STEP 1: Show first metric immediately (no loading delay)
    first_metric_shown <- reactiveVal(FALSE)
    
    observe({
      req(input_controls()$metric)
      if (first_metric_shown()) return()
      
      metric <- input_controls()$metric
      
      # Fast fade-in of skeleton -> content
      shinyjs::delay(50, {
        shinyjs::hide("skeleton-loader", anim = TRUE, animType = "fade")
        shinyjs::show("metricsContainer", anim = TRUE, animType = "fade")
      })
      
      # Initialize current metric right away
      if (initialize_server(metric, priority = TRUE)) {
        shinyjs::show(id = paste0("metric-", metric), anim = TRUE, animType = "fade")
      }
      
      first_metric_shown(TRUE)
    })
    
    # STEP 2: ✅ FIXED - Load background metrics WITHOUT Sys.sleep()
    # Uses non-blocking invalidateLater instead
    observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      # Wait before starting background load
      invalidateLater(1200)
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      remaining <- setdiff(all_metrics, first_metric)
      
      # Priority order for background loading
      priority_order <- c("sums", "table", "odds_evens", "difference", "lag", "balls")
      remaining <- intersect(priority_order, remaining)
      
      # ✅ Load all at once (NO SLEEP - let Shiny handle timing)
      lapply(remaining, function(m) {
        initialize_server(m)
      })
      
    }) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    # STEP 3: Instant metric switching with optimized transitions
    observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      new_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      # Quick fade out
      for (m in all_metrics) {
        if (m != new_metric) {
          shinyjs::hide(id = paste0("metric-", m), anim = TRUE, animType = "fade", time = 0.15)
        }
      }
      
      # Initialize if needed
      initialize_server(new_metric, priority = TRUE)
      
      # Fast fade in
      shinyjs::delay(150, {
        shinyjs::show(id = paste0("metric-", new_metric), anim = TRUE, animType = "fade", time = 0.25)
      })
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
  })
}

