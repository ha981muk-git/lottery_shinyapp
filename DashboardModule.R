# UI Module
lotteryInputUI <- function(id, lang = "de") {
  ns <- NS(id)
  
  # Dynamic choices based on language
  time_choices <- setNames(
    c(7, 30, 60, 90, 120, 150, 180),
    c(t("time_last_7", lang),
      t("time_last_30", lang),
      t("time_last_60", lang),
      t("time_last_90", lang),
      t("time_last_120", lang),
      t("time_last_150", lang),
      t("time_last_180", lang)
    ))
  
  metric_choices <- setNames(
    c("balls", "sums", "odds_evens", "table", "difference", "lag"),
    c(
      t("metric_balls", lang),
      t("metric_sums", lang),
      t("metric_odds_evens", lang),
      t("metric_tables", lang),
      t("metric_difference", lang),
      t("metric_lag", lang)
    )
  )
  
  tagList(
    div(style = "margin-bottom: 24px;",
        h4(style = "color: #e8eaed; margin-bottom: 8px;",
           span(class = "status-dot"), t("input_live_dashboard", lang)),
        p(style = "color: rgba(255, 255, 255, 0.5); font-size: 0.875rem;", 
          t("input_realtime", lang))
    ),
    sliderInput(ns("range"), 
                t("input_ball_range", lang), 
                min = 1, max = 49, value = c(1,49), step = 1),
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
                 style = "margin-top: 20px; border-radius: 10px; padding: 10px; font-weight: 600;")
  )
}

# Module Server
lotteryInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    minDistance <- 6
    
    # Regular list for handles
    handles <- list()
    
    handle1 <- observeEvent(input$range, {
      minVal <- input$range[1]
      maxVal <- input$range[2]
      
      if ((maxVal - minVal) < minDistance) {
        newRange <- c(max(1, maxVal - minDistance), min(49, maxVal))
        updateSliderInput(session, "range", value = newRange)
      }
    }, ignoreInit = TRUE)
    
    handles[[length(handles) + 1]] <- handle1
    
    range_debounced <- reactive({
      input$range
    }) %>% debounce(500)
    
    refresh_throttled <- reactive({
      input$refresh
    }) %>% throttle(300)
    
    session$onSessionEnded(function() {
      lapply(handles, function(h) {
        if (!is.null(h)) h$destroy()
      })
    })
    
    return(reactive({
      list(
        range = range_debounced(),
        metric = input$metric,
        timeRange = input$timeRange,
        refresh = refresh_throttled()
      )
    }))
  })
}

# Dashboard UI
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(id = ns("skeleton-loader"),
        style = "padding: 20px;",
        div(class = "skeleton-card",
            style = "height: 200px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px; margin-bottom: 20px;"),
        div(class = "skeleton-card",
            style = "height: 300px; background: linear-gradient(90deg, rgba(139,92,246,0.1) 25%, rgba(139,92,246,0.2) 50%, rgba(139,92,246,0.1) 75%); background-size: 200% 100%; animation: shimmer 1.2s infinite; border-radius: 12px;")
    ),
    
    div(id = ns("metricsContainer"),
        style = "display: none;",
        
        div(id = ns("metric-balls"), 
            style = "display: none;",
            ballsMetricUI(ns("balls"))),
        
        div(id = ns("metric-sums"), 
            style = "display: none;",
            sumsMetricUI(ns("sums"))),
        
        div(id = ns("metric-odds_evens"), 
            style = "display: none;",
            oddsEvensMetricUI(ns("odds_evens"))),
        
        div(id = ns("metric-table"), 
            style = "display: none;",
            tableMetricUI(ns("table"))),
        
        div(id = ns("metric-difference"), 
            style = "display: none;",
            differenceMetricUI(ns("difference"))),
        
        div(id = ns("metric-lag"), 
            style = "display: none;",
            lagMetricUI(ns("lag")))
    )
  )
}

# Server Module
dashboardServer <- function(id, input_controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metrics_data <- generate_metrics()
    draws_per_week <- 2
    
    # Regular list for handles
    handles <- list()
    
    cache_env <- new.env()
    
    get_filtered_data <- function(weeks, range_vals) {
      cache_key <- paste(weeks, range_vals[1], range_vals[2], sep = "_")
      
      if (length(ls(envir = cache_env)) > 20) {
        rm(list = ls(envir = cache_env)[1:5], envir = cache_env)
      }
      
      if (exists(cache_key, envir = cache_env)) {
        return(get(cache_key, envir = cache_env))
      }
      
      data <- metrics_data
      req(!is.null(data) && nrow(data) > 0)
      
      days <- weeks * draws_per_week
      data <- tail(data, min(days, nrow(data)))
      
      num_from <- as.numeric(range_vals[1])
      num_to <- as.numeric(range_vals[2])
      
      data <- data %>%
        filter(ball_1 >= num_from & ball_6 <= num_to)
      
      req(nrow(data) > 0)
      
      assign(cache_key, data, envir = cache_env)
      data
    }
    
    filtered_data <- eventReactive(
      c(input_controls()$refresh, 
        input_controls()$timeRange, 
        input_controls()$range),
      {
        weeks <- as.numeric(input_controls()$timeRange)
        range_vals <- input_controls()$range
        get_filtered_data(weeks, range_vals)
      },
      ignoreNULL = TRUE
    )
    
    initialized_servers <- reactiveVal(list())
    
    initialize_server <- function(metric) {
      already_init <- initialized_servers()
      if (metric %in% already_init) return()
      
      switch(metric,
             "balls" = ballsMetricServer("balls", filtered_data, input_controls),
             "sums" = sumsMetricServer("sums", filtered_data),
             "odds_evens" = oddsEvensMetricServer("odds_evens", filtered_data),
             "table" = tableMetricServer("table", filtered_data),
             "difference" = differenceMetricServer("difference", filtered_data),
             "lag" = lagMetricServer("lag", filtered_data)
      )
      
      initialized_servers(c(already_init, metric))
    }
    
    handle1 <- observe({
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      shinyjs::hide("skeleton-loader")
      shinyjs::show("metricsContainer")
      
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, priority = 100) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    handles[[length(handles) + 1]] <- handle1
    
    handle2 <- observe({
      req(input_controls()$metric)
      first_metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      other_metrics <- setdiff(all_metrics, first_metric)
      
      shinyjs::delay(500, {
        lapply(other_metrics, function(m) {
          initialize_server(m)
        })
      })
      
    }, priority = 10) %>% bindEvent(input_controls()$metric, once = TRUE)
    
    handles[[length(handles) + 1]] <- handle2
    
    handle3 <- observeEvent(input_controls()$metric, {
      req(input_controls()$metric)
      metric <- input_controls()$metric
      
      all_metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
      
      lapply(all_metrics, function(m) {
        if (m != metric) {
          shinyjs::hide(id = paste0("metric-", m))
        }
      })
      
      initialize_server(metric)
      shinyjs::show(id = paste0("metric-", metric))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 50)
    
    handles[[length(handles) + 1]] <- handle3
    
    session$onSessionEnded(function() {
      lapply(handles, function(h) {
        if (!is.null(h)) h$destroy()
      })
      
      rm(list = ls(envir = cache_env), envir = cache_env)
      initialized_servers(list())
    })
    
  })
}