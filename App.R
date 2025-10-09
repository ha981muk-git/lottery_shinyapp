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
# -------------------------
# Single Page UI: Lotto 6aus49 Educational/Testing Platform
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
    
    # Additional inline styles for disclaimer banner
    tags$style(HTML("
      .disclaimer-banner {
        background: linear-gradient(135deg, #ff6b6b 0%, #ee5a6f 100%);
        color: white;
        padding: 15px 0;
        text-align: center;
        font-weight: 600;
        font-size: 0.95em;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        position: sticky;
        top: 0;
        z-index: 1001;
        animation: pulse 2s ease-in-out infinite;
      }
      
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.9; }
      }
      
      .disclaimer-banner strong {
        font-size: 1.1em;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .educational-notice {
        background: rgba(255, 193, 7, 0.15);
        border: 2px solid #ffc107;
        border-radius: 12px;
        padding: 25px;
        margin: 20px 0;
        color: #ffc107;
      }
      
      .educational-notice h3 {
        color: #ffc107;
        margin-top: 0;
        font-size: 1.4em;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .educational-notice ul {
        color: rgba(255, 255, 255, 0.85);
        line-height: 1.8;
      }
      
      .testing-badge {
        display: inline-block;
        background: #ff9800;
        color: white;
        padding: 5px 12px;
        border-radius: 20px;
        font-size: 0.8em;
        font-weight: 700;
        margin-left: 10px;
        animation: blink 1.5s ease-in-out infinite;
      }
      
      @keyframes blink {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.7; }
      }
    "))
  ),
  
  # ⚠️ DISCLAIMER BANNER (Top Priority)
  div(class = "disclaimer-banner",
      "⚠️ ",
      strong("EDUCATIONAL & TESTING PURPOSE ONLY"),
      " | This website is under construction and for statistical analysis demonstration purposes only | No real gambling services provided"
  ),
  
  # Professional Header
  div(class = "professional-header",
      div(class = "header-content",
          div(class = "logo-section",
              span("🎲", class = "logo-icon"),
              div(class = "logo-text",
                  h1("6/49 Statistical Visualization", 
                     span(class = "testing-badge", "TESTING")),
                  p("Statistical Analysis & Educational Platform")
              )
          ),
          div(class = "header-nav",
              a(href = "#", "Home"),
              a(href = "#analyzer", "Analyzer"),
              a(href = "#educational", "Educational Info"),
              a(href = "#disclaimer", "Disclaimer")
          )
      )
  ),
  
  # Main Content
  div(class = "main-content",
      # Welcome Section
      div(class = "welcome-section",
          h2("Welcome to 6/49 Statistical Visualization"),
          p(strong("Educational Platform:"), " Discover the power of data analysis for understanding lottery number patterns. This professional tool provides insights into historical draws, frequency analysis, and pattern recognition using statistical methods."),
      ),
      
      # Main Analyzer Section
      div(id = "analyzer",
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              class = "control-panel",
              h3("Analysis Settings", style = "margin-top: 0; color: #e8eaed;"),
              lotteryInputUI("inputs1")
            ),
            div(
              style = "padding: 20px;",
              uiOutput("dashboard1-metricContent")
            ),
            fillable = FALSE
          )
      ),
      
      # Educational Notice (Prominent)
      div(class = "educational-notice",
          h3(
            "⚠️ Important Notice - Educational Purpose Only"
          ),
          tags$ul(
            tags$li(strong("This is a TESTING and EDUCATIONAL platform"), " for demonstrating statistical analysis methods"),
            tags$li("This website is ", strong("under construction"), " and not intended for commercial use"),
            tags$li("No real lottery services, betting, or gambling features are provided"),
            tags$li("All data analysis is for ", strong("educational and research purposes"), " only"),
            tags$li("This tool demonstrates probability theory, data visualization, and statistical methods"),
            tags$li(strong("Warning:"), " Gambling can be addictive. Please play responsibly. This site does NOT encourage gambling")
          ),
          p(style = "margin-top: 15px; font-style: italic; color: rgba(255,255,255,0.7);",
            "🔬 Purpose: Academic demonstration of statistical computing")
      ),
      
      # Additional Educational Section
      div(id = "educational",
          style = "margin-top: 40px; padding: 30px; background: rgba(255,255,255,0.03); border-radius: 12px;",
          h2("About This Educational Project", style = "color: #e8eaed;"),
          p(style = "color: rgba(255,255,255,0.7); line-height: 1.8;",
            "This application demonstrates advanced statistical analysis techniques using publicly available lottery data. ",
            "It serves as an educational resource for understanding probability distributions, frequency analysis, ",
            "and data visualization methods. The platform is designed for students, researchers, and data science enthusiasts ",
            "interested in learning about statistical computing and interactive web applications."
          ),
          h3("Learning Objectives:", style = "color: #e8eaed; margin-top: 20px;"),
          tags$ul(
            style = "color: rgba(255,255,255,0.7); line-height: 1.8;",
            tags$li("Understanding probability theory and statistical distributions"),
            tags$li("Interactive web application development"),
            tags$li("Time series analysis and pattern recognition"),
            tags$li("Responsible interpretation of statistical results")
          )
      )
  ),
  
  # Professional Footer
  div(class = "professional-footer",
      div(class = "footer-content",
          div(class = "footer-sections",
              # About Section
              div(class = "footer-section",
                  h3("About This Project"),
                  p(strong("Educational & Testing Only")),
                  p("Professional analysis tools for demonstrating statistical methods with Lotto 6aus49 data. Based on historical data and modern statistical approaches."),
                  p(style = "color: #ffc107; font-weight: 600;", 
                    "⚠️ Under Construction - Testing Phase")
              ),
              # Quick Links
              div(class = "footer-section",
                  h3("Quick Links"),
                  tags$ul(
                    tags$li(a(href = "#", "Home")),
                    tags$li(a(href = "#analyzer", "Analyzer")),
                    tags$li(a(href = "#educational", "Educational Info")),
                    tags$li(a(href = "#disclaimer", "Disclaimer"))
                  )
              ),
              # Legal & Disclaimer
              div(class = "footer-section",
                  h3("Legal & Disclaimer"),
                  tags$ul(
                    tags$li(a(href = "#", "Full Disclaimer")),
                    tags$li(a(href = "#", "Privacy Policy")),
                    tags$li(a(href = "#", "Terms of Use")),
                    tags$li(a(href = "#", "Educational Purpose Statement"))
                  ),
                  p(style = "color: #e74c3c; font-size: 0.85em; margin-top: 10px;",
                    "⚠️ No gambling services provided")
              ),
              # Contact & Responsibility
              div(class = "footer-section",
                  h3("Important Information"),
                  p("Project Type: Educational/Academic"),
                  p("Status: Under Construction (Testing)"),
                  p(strong("Responsible Gaming:")),
                  p(style = "font-size: 0.9em;", "This site is for educational purposes only. We do not encourage gambling. If you have gambling problems, seek help:"),
                  p(style = "font-size: 0.85em;", "🇩🇪 BZgA: 0800 1 37 27 00")
              )
          ),
          div(class = "footer-bottom",
              p(paste0("© ", format(Sys.Date(), "%Y"), 
                       " 6/49 Statistical Visualization - Educational & Testing Project | ",
                       strong("FOR EDUCATIONAL PURPOSES ONLY"), " | ",
                       "Play Responsibly | No Real Gambling Services Provided | Under Construction"))
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