# ---- Source external base file (adjust path if needed) ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
  script_folder <- dirname(current_script_path)
  
  files_to_source <- c(
    "Base.R",
    "PrepareData.R",
    "DashboardModule.R",
    "GeneratorModule.R",
    "StatsModule.R",
    "HotcoldModule.R"
  )
  
  for (file_name in files_to_source) {
    file_path <- file.path(script_folder, file_name)
    if (file.exists(file_path)) {
      source(file_path)
    } else {
      warning(paste("File not found:", file_name))
    }
  }
}

# ✅ PERFORMANCE TESTING FLAG - Set to TRUE to enable
ENABLE_PERFORMANCE_LOGS <- TRUE

# -------------------------
# Top-level UI
# -------------------------
ui <- page_navbar(
  title = div(
    style = "display: flex; width: 100%; align-items: center; gap: 25px;",
    span(""),
    span("Analysis of Lotto 6aus49")
  ),
  theme = app_theme,
  fillable = TRUE,
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "ShinyLottery.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Dashboard.css"),
    useShinyjs(),
    use_waiter(),
    # Performance monitor badge (only shows if enabled)
    if(ENABLE_PERFORMANCE_LOGS) {
      tags$div(
        id = "perf-badge",
        style = "position: fixed; bottom: 20px; right: 20px; background: rgba(0,255,0,0.2); 
                 border: 2px solid #0f0; padding: 10px 15px; border-radius: 8px; 
                 color: #0f0; font-family: monospace; font-size: 12px; z-index: 9999;
                 box-shadow: 0 4px 6px rgba(0,0,0,0.3);",
        div(style = "font-weight: bold; margin-bottom: 5px;", "⚡ PERFORMANCE MONITOR"),
        div(id = "perf-info", "Initializing...")
      )
    }
  ),
  
  nav_panel("🎲 Lotto Analyzer",
            layout_sidebar(
              sidebar = sidebar(
                width = 280,
                class = "control-panel",
                lotteryInputUI("inputs1"),
                # Add performance stats in sidebar (if enabled)
                if(ENABLE_PERFORMANCE_LOGS) {
                  div(
                    style = "margin-top: 30px; padding: 15px; background: rgba(0,255,0,0.1); 
                             border-radius: 8px; border: 1px solid rgba(0,255,0,0.3);",
                    h5(style = "color: #0f0; margin-bottom: 10px;", "📊 Performance Stats"),
                    verbatimTextOutput("perfStats", placeholder = TRUE)
                  )
                }
              ),
              uiOutput("dashboard1-metricContent"),
              fillable = FALSE
            )
  )
)

# -------------------------
# Top-level Server
# -------------------------
server <- function(input, output, session) {
  
  # Performance tracking reactiveValues
  perf <- reactiveValues(
    data_load_time = 0,
    filter_time = 0,
    render_time = 0,
    total_updates = 0
  )
  
  # Call input module
  input_controls <- inputModuleServer("inputs1")
  
  # Wrap dashboardServer with performance monitoring
  if(ENABLE_PERFORMANCE_LOGS) {
    
    # Track when inputs change (FIXED: using isolate to prevent infinite loop)
    observeEvent(input_controls(), {
      perf$total_updates <- isolate(perf$total_updates) + 1
      
      cat("\n", rep("=", 60), "\n", sep = "")
      cat("🔄 UPDATE #", perf$total_updates, "\n")
      cat(rep("=", 60), "\n")
      cat("Time Range:", input_controls()$timeRange, "weeks\n")
      cat("Ball Range:", input_controls()$range[1], "-", input_controls()$range[2], "\n")
      cat("Metric:    ", input_controls()$metric, "\n")
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
  }
  
  # Call dashboard server
  dashboardServer("dashboard1", input_controls = input_controls)
  
  # Commented out modules
  # gen_out <- generatorServer("gen1")
  # statsServer("stats1")
  # hotcoldServer("hc1")
  
  # Display performance stats in sidebar
  if(ENABLE_PERFORMANCE_LOGS) {
    output$perfStats <- renderText({
      # FIXED: Only trigger on specific inputs, not continuous
      req(input_controls())
      
      paste0(
        "Updates: ", isolate(perf$total_updates), "\n",
        "Load: ", sprintf("%.1f ms", isolate(perf$data_load_time)), "\n",
        "Filter: ", sprintf("%.1f ms", isolate(perf$filter_time)), "\n",
        "Total: ", sprintf("%.1f ms", isolate(perf$data_load_time + perf$filter_time))
      )
    })
  }
  
  # Initial performance report
  if(ENABLE_PERFORMANCE_LOGS) {
    observe({
      # Run once on startup
      isolate({
        cat("\n")
        cat("╔", rep("═", 58), "╗\n", sep = "")
        cat("║  🚀 PERFORMANCE MONITORING ENABLED                        ║\n")
        cat("╠", rep("═", 58), "╣\n", sep = "")
        
        # Test data loading
        test_start <- Sys.time()
        test_data <- generate_metrics()
        test_time <- as.numeric(Sys.time() - test_start, units = "secs") * 1000
        
        cat(sprintf("║  Data Size: %d rows × %d cols                           ║\n", 
                    nrow(test_data), ncol(test_data)))
        cat(sprintf("║  Load Time: %.2f ms                                     ║\n", test_time))
        cat(sprintf("║  Memory:    %.2f KB                                     ║\n", 
                    object.size(test_data) / 1024))
        cat("╠", rep("═", 58), "╣\n", sep = "")
        
        if(test_time < 10) {
          cat("║  ✅ EXCELLENT: Data loading is instant!                   ║\n")
        } else if(test_time < 50) {
          cat("║  ✅ GOOD: Performance is smooth                           ║\n")
        } else {
          cat("║  ⚠️  SLOW: Consider optimization                          ║\n")
        }
        
        cat("╠", rep("═", 58), "╣\n", sep = "")
        cat("║  💡 Check RStudio Console for detailed timing logs       ║\n")
        cat("║  💡 Set ENABLE_PERFORMANCE_LOGS = FALSE to disable       ║\n")
        cat("╚", rep("═", 58), "╝\n", sep = "")
        cat("\n")
      })
    })
  }
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server)