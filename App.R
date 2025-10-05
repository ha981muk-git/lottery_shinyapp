# ---- Source external base file (adjust path if needed) ----
# Only works if you are running inside RStudio as before
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  
  # Get the folder of the currently active script
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
  script_folder <- dirname(current_script_path)
  
  # List of R files to source
  files_to_source <- c(
    "Base.R",
    "PrepareData.R",
    "DashboardModule.R",
    "GeneratorModule.R",
    "StatsModule.R",
    "HotcoldModule.R"
  )
  
  # Source each file if it exists
  for (file_name in files_to_source) {
    file_path <- file.path(script_folder, file_name)
    if (file.exists(file_path)) {
      source(file_path)
    } else {
      warning(paste("File not found:", file_name))
    }
  }
}

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
    use_waiter()
  ),
  
  nav_panel("🎲 Lotto Analyzer",
            layout_sidebar(
              sidebar = sidebar(
                width = 280,
                class = "control-panel",
                lotteryInputUI("inputs1")
            ),
              # main content (module)
              uiOutput("dashboard1-metricContent"),
              #sidebar_collapsible = FALSE, # sidebar is open on load
              fillable = FALSE
            )
  ),
  
  # nav_panel(title = "🎲 Analysis", div(style = "max-width: 1400px; margin: 0 auto; padding: 20px;",
  #                                      generatorUI("gen1")
  # )),
  # 
  # nav_panel(title = "📊 Statistics", div(style = "max-width: 1400px; margin: 0 auto; padding: 20px;",
  #                                        statsUI("stats1")
  # )),
  # 
  # nav_panel(title = "🔥 Hot & Cold", div(style = "max-width: 1400px; margin: 0 auto; padding: 20px;",
  #                                        hotcoldUI("hc1")
  # ))
)

# -------------------------
# Top-level Server
# -------------------------
server <- function(input, output, session) {
  # Call input module
  input_controls <- inputModuleServer("inputs1")
  
  # call modules
  dashboardServer("dashboard1", input_controls = input_controls)
  #ballsMetricServer("dashboard1", input_controls = input_controls)
  gen_out <- generatorServer("gen1")  # returns list with selected_numbers reactive if needed
  statsServer("stats1")
  hotcoldServer("hc1")
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server)