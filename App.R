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
  
  # Include CSS, JS libraries and custom styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "ShinyLottery.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Dashboard.css"),
    useShinyjs(),
    use_waiter(),
    tags$style(HTML("
      /* Professional German Website Styling */
      body {
        font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        color: #2c3e50;
        background-color: #f8f9fa;
        margin: 0;
        padding: 0;
      }
      
      /* Professional Header */
      .professional-header {
        background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
        color: white;
        padding: 30px 0;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        position: sticky;
        top: 0;
        z-index: 1000;
      }
      
      .header-content {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 30px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .logo-section {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      .logo-icon {
        font-size: 2.5em;
        filter: drop-shadow(2px 2px 4px rgba(0,0,0,0.2));
      }
      
      .logo-text h1 {
        margin: 0;
        font-size: 1.8em;
        font-weight: 700;
        letter-spacing: -0.5px;
      }
      
      .logo-text p {
        margin: 5px 0 0 0;
        font-size: 0.9em;
        opacity: 0.9;
        font-weight: 300;
      }
      
      .header-nav {
        display: flex;
        gap: 25px;
        align-items: center;
      }
      
      .header-nav a {
        color: white;
        text-decoration: none;
        font-weight: 500;
        font-size: 0.95em;
        transition: opacity 0.3s ease;
      }
      
      .header-nav a:hover {
        opacity: 0.8;
      }
      
      /* Main Content Area */
      .main-content {
        max-width: 1400px;
        margin: 40px auto;
        padding: 0 30px;
        min-height: calc(100vh - 400px);
      }
      
      .welcome-section {
        background: white;
        border-radius: 12px;
        padding: 40px;
        margin-bottom: 30px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
      }
      
      .welcome-section h2 {
        color: #1e3c72;
        font-size: 2em;
        margin-bottom: 15px;
        font-weight: 600;
      }
      
      .welcome-section p {
        color: #5a6c7d;
        font-size: 1.1em;
        line-height: 1.6;
        margin-bottom: 10px;
      }
      
      /* Control Panel Styling */
      .control-panel {
        background: white;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
        padding: 25px;
      }
      
      .bslib-sidebar-layout {
        gap: 25px;
      }
      
      /* Card styling */
      .card {
        border-radius: 12px;
        border: none;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        background: white;
      }
      
      .card:hover {
        transform: translateY(-4px);
        box-shadow: 0 8px 24px rgba(30, 60, 114, 0.15);
      }
      
      /* Professional Footer */
      .professional-footer {
        background: #2c3e50;
        color: white;
        margin-top: 60px;
        padding: 40px 0 20px 0;
        border-top: 4px solid #1e3c72;
      }
      
      .footer-content {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 30px;
      }
      
      .footer-sections {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 40px;
        margin-bottom: 30px;
      }
      
      .footer-section h3 {
        font-size: 1.2em;
        margin-bottom: 15px;
        font-weight: 600;
        color: #ecf0f1;
      }
      
      .footer-section p,
      .footer-section ul {
        font-size: 0.9em;
        line-height: 1.8;
        color: #bdc3c7;
      }
      
      .footer-section ul {
        list-style: none;
        padding: 0;
        margin: 0;
      }
      
      .footer-section ul li {
        margin-bottom: 8px;
      }
      
      .footer-section a {
        color: #bdc3c7;
        text-decoration: none;
        transition: color 0.3s ease;
      }
      
      .footer-section a:hover {
        color: #ecf0f1;
      }
      
      .footer-bottom {
        border-top: 1px solid #34495e;
        padding-top: 20px;
        text-align: center;
        color: #95a5a6;
        font-size: 0.85em;
      }
      
      /* Smooth transitions */
      * {
        transition: all 0.2s ease;
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .header-content {
          flex-direction: column;
          gap: 20px;
        }
        
        .header-nav {
          flex-wrap: wrap;
          justify-content: center;
        }
        
        .welcome-section {
          padding: 25px;
        }
        
        .footer-sections {
          grid-template-columns: 1fr;
        }
      }
    "))
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
              h3("Einstellungen", style = "margin-top: 0; color: #1e3c72;"),
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

# Keep commented sections for future use:
# nav_panel(
#   title = span(icon("dice"), " Analysis"), 
#   div(
#     style = "max-width: 1400px; margin: 0 auto; padding: 30px;",
#     generatorUI("gen1")
#   )
# ),
# 
# nav_panel(
#   title = span(icon("chart-bar"), " Statistics"), 
#   div(
#     style = "max-width: 1400px; margin: 0 auto; padding: 30px;",
#     statsUI("stats1")
#   )
# ),
# 
# nav_panel(
#   title = span(icon("fire"), " Hot & Cold"), 
#   div(
#     style = "max-width: 1400px; margin: 0 auto; padding: 30px;",
#     hotcoldUI("hc1")
#   )
# )

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