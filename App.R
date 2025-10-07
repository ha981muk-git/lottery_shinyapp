# 1. Check RStudio and get script folder
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
  script_folder <- dirname(current_script_path)
  
  # 2. Source main scripts first
  main_files <- c(
    "Base.R",
    "PrepareData.R",
    "DashboardModule.R"
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
# Top-level UI: Lotto 6aus49 Dashboard
# -------------------------
# -------------------------
# Single Page UI: Lotto 6aus49 Professional Website
# -------------------------
ui <- page_fluid(
  theme = app_theme,
  
  # Include CSS, JS libraries
  tags$head(
    # ✅ Keep Dashboard.css (DARK theme)
    tags$link(rel = "stylesheet", type = "text/css", href = "Dashboard.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
    
    useShinyjs(),
    use_waiter(),
  ),
  
  # Professional Header
  div(class = "professional-header",
      div(class = "header-content",
          div(class = "logo-section",
              span("🎲", class = "logo-icon"),
              div(class = "logo-text",
                  h1("Lotto 6aus49 Analyzer"),
                  p("Intelligente Analyse für Ihre Gewinnchancen")
              )
          ),
          div(class = "header-nav",
              a(href = "#", "Startseite"),
              a(href = "#analyzer", "Analyzer"),
              a(href = "#about", "Über uns"),
              a(href = "#contact", "Kontakt")
          )
      )
  ),
  
  # Main Content
  div(class = "main-content",
      
      # Welcome Section
      div(class = "welcome-section",
          h2("Willkommen beim Lotto 6aus49 Analyzer"),
          p("Entdecken Sie die Macht der Datenanalyse für Ihre Lottozahlen. Unser professionelles Tool bietet Ihnen tiefe Einblicke in historische Ziehungen, Häufigkeitsanalysen und intelligente Mustererkennungen."),
          p("Nutzen Sie modernste statistische Methoden, um Ihre Zahlenauswahl zu optimieren und fundierte Entscheidungen zu treffen.")
      ),
      
      # Main Analyzer Section
      div(id = "analyzer",
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              class = "control-panel",
              h3("Einstellungen", style = "margin-top: 0; color: #e8eaed;"),
              lotteryInputUI("inputs1")
            ),
            div(
              style = "padding: 20px;",
              uiOutput("dashboard1-metricContent")
            ),
            fillable = FALSE
          )
      )
  ),
  
  # Professional Footer
  div(class = "professional-footer",
      div(class = "footer-content",
          div(class = "footer-sections",
              # About Section
              div(class = "footer-section",
                  h3("Über Lotto Analyzer"),
                  p("Professionelle Analyse-Tools für Lotto 6aus49. Basierend auf historischen Daten und modernen statistischen Methoden.")
              ),
              # Quick Links
              div(class = "footer-section",
                  h3("Schnellzugriff"),
                  tags$ul(
                    tags$li(a(href = "#", "Startseite")),
                    tags$li(a(href = "#analyzer", "Analyzer")),
                    tags$li(a(href = "#", "Statistiken")),
                    tags$li(a(href = "#", "FAQ"))
                  )
              ),
              # Legal
              div(class = "footer-section",
                  h3("Rechtliches"),
                  tags$ul(
                    tags$li(a(href = "#", "Impressum")),
                    tags$li(a(href = "#", "Datenschutz")),
                    tags$li(a(href = "#", "AGB")),
                    tags$li(a(href = "#", "Haftungsausschluss"))
                  )
              ),
              # Contact
              div(class = "footer-section",
                  h3("Kontakt"),
                  p("E-Mail: info@lotto-analyzer.de"),
                  p("Hinweis: Dies ist ein Analyse-Tool. Keine Gewinngarantie.")
              )
          ),
          div(class = "footer-bottom",
              p(paste0("© ", format(Sys.Date(), "%Y"), " Lotto 6aus49 Analyzer. Alle Rechte vorbehalten. | Verantwortungsvolles Spielen"))
          )
      )
  )
)

# -------------------------
# Top-level Server
# -------------------------
server <- function(input, output, session) {
  # Call input module
  input_controls <- lotteryInputServer("inputs1")
  
  # call modules
  dashboardServer("dashboard1", input_controls = input_controls)
#  ballsMetricServer("dashboard1", input_controls = input_controls)
#  gen_out <- generatorServer("gen1")  # returns list with selected_numbers reactive if needed
#  statsServer("stats1")
#  hotcoldServer("hc1")
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server)