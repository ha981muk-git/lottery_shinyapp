# 1. Check RStudio and get script folder
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
  script_folder <- dirname(current_script_path)
  
  # 2. Source main scripts first
  main_files <- c(
    "Base.R",
    "PrepareData.R",
    "DashboardModule.R",
    "ui.R",
    "server.R"
    # "GeneratorModule.R",
    # "StatsModule.R",
    # "HotcoldModule.R"
  )
  
  lapply(main_files, function(f) {
    file_path <- file.path(script_folder, f)
    if (file.exists(file_path)) {
      tryCatch(source(file_path), error = function(e) {
        warning(paste("Failed to source:", f, "-", e$message))
      })
    } else {
      warning(paste("File not found:", f))
    }
  })
  
  # 3. Automatically source all metric modules in the 'dashboard' folder
  metric_files <- list.files(
    path = file.path(script_folder, "dashboard"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  
  lapply(metric_files, function(f) {
    tryCatch(source(f), error = function(e) {
      warning(paste("Failed to source metric file:", f, "-", e$message))
    })
  })
}



# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server)