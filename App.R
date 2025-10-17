library(shiny)
library(vroom)
library(dplyr)
library(janitor)
library(bslib)
library(shinyjs)
library(plotly)
library(waiter)
library(tidyr)
library(purrr)
library(DT)




# ---------- UI helper theme ----------
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#0a0e27",
  fg = "#e8eaed",
  primary = "#8b5cf6",
  secondary = "#ec4899",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)


# Source main files
script_folder <- "."

# Source main scripts
main_files <- c(
  "PrepareData.R",
  "DashboardModule.R"
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

# Source all metric modules in the 'dashboard' folder
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




# -------------------------
# Top-level UI: Lotto 6aus49 Dashboard
# -------------------------
# -------------------------
# Single Page UI: Lotto 6aus49 Professional Website
# -------------------------
# -------------------------
# Single Page UI: Lotto 6aus49 Educational/Testing Platform
# -------------------------
ui <- fluidPage(
  theme = app_theme,
  
  # Include CSS, JS libraries
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
    useShinyjs(),
    use_waiter(),
    
    # Fix sidebar overlay
    tags$script(HTML("
      $(document).ready(function() {
        function fixSidebarOverlay() {
          $('.bslib-sidebar-layout > .main').css({
            'opacity': '1',
            'filter': 'none',
            'pointer-events': 'auto',
            'transition': 'none'
          });
          $('.sidebar-backdrop, .bslib-sidebar-backdrop').remove();
          $('.bslib-sidebar-layout').css({
            'display': 'grid',
            'grid-template-columns': 'auto 1fr',
            'gap': '20px'
          });
        }
        fixSidebarOverlay();
        setTimeout(fixSidebarOverlay, 100);
        setTimeout(fixSidebarOverlay, 500);
        const observer = new MutationObserver(fixSidebarOverlay);
        observer.observe(document.body, { childList: true, subtree: true, attributes: true });
      });
    ")),
    
    # ✅ Add responsive sidebar toggle
    tags$script(HTML("
      $(document).ready(function() {
        const toggleButton = $('<button class=\"sidebar-toggle-btn\">☰ Menu</button>')
          .css({
            position: 'fixed',
            top: '15px',
            left: '15px',
            background: '#8b5cf6',
            color: '#fff',
            border: 'none',
            padding: '10px 14px',
            borderRadius: '8px',
            fontSize: '18px',
            cursor: 'pointer',
            zIndex: 2000,
            display: 'none'
          })
          .appendTo('body')
          .on('click', function() {
            const layout = document.querySelector('.bslib-sidebar-layout');
            if (layout) {
              const open = layout.dataset.sidebarOpen === 'true';
              layout.dataset.sidebarOpen = !open;
            }
          });

        function checkScreen() {
          if (window.innerWidth < 768) {
            toggleButton.show();
          } else {
            toggleButton.hide();
          }
        }
        checkScreen();
        $(window).on('resize', checkScreen);
      });
    "))
  ),
  
  
  # ⚠️ DISCLAIMER BANNER (Top Priority)
  # div(class = "disclaimer-banner",
  #     "⚠️ ",
  #     strong("EDUCATIONAL & TESTING PURPOSE ONLY"),
  #     " | This website is under construction and for statistical analysis demonstration purposes only"
  # ),
  
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
      # div(class = "welcome-section",
      #     h2("Welcome to 6/49 Statistical Visualization"),
      #     p(strong("Educational Platform:"), " Discover the power of data analysis for understanding lottery number patterns. This professional tool provides insights into historical draws, frequency analysis, and pattern recognition using statistical methods."),
      # ),
      
      # Main Analyzer Section
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          class = "control-panel",
          open = "desktop",  # Keep this
          position = "left",  # ✅ ADD THIS
          max_height_mobile = NULL,  # ✅ ADD THIS
          h3("Analysis Settings", style = "margin-top: 0; color: #e8eaed;"),
          lotteryInputUI("inputs1")
        ),
        # Main content
        div(
          style = "padding: 0; min-height: 100vh;",  # Removed excessive padding
          # NEW (fast):
          dashboardUI("dashboard1")
        ),
        fillable = FALSE,
        border = FALSE,  # ✅ ADD THIS
        border_radius = FALSE  # ✅ ADD THIS
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
                  p("Professional analysis tools for demonstrating statistical methods with public Lotto 6aus49 data. Based on historical data and modern statistical approaches."),
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
                  p("Status: Under Construction (Testing)")
                  # p(strong("Responsible Gaming:")),
                  # p(style = "font-size: 0.9em;", "This site is for educational purposes only. We do not encourage gambling. If you have gambling problems, seek help:"),
                  # p(style = "font-size: 0.85em;", "🇩🇪 BZgA: 0800 1 37 27 00")
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