# --- Production-ready Shiny options for DigitalOcean ---
if (!requireNamespace("config", quietly = TRUE)) {
  stop("Package 'config' is required for configuration management.")
}

cfg <- tryCatch(config::get(), error = function(e) {
  message("⚠ Using default config (no config.yml found)")
  list(maxRequestSize = 10, sessionTimeout = 3600)
})

options(
  shiny.maxRequestSize = cfg$maxRequestSize * 1024^2,
  shiny.sanitize.errors = TRUE,
  shiny.reactlog = FALSE,
  shiny.autoreload = FALSE,
  shiny.enableBookmarking = "disable",
  shiny.session.timeout = cfg$sessionTimeout,
  repos = c(CRAN = "https://cloud.r-project.org/"),
  shiny.error = function(e) {
    message("[Shiny Error] ", Sys.time(), " - ", e$message)
    showNotification("Ein unerwarteter Fehler ist aufgetreten. Bitte laden Sie die Seite neu.", type = "error")
  },
  shiny.fullstacktrace = FALSE,
  mc.cores = min(2, parallel::detectCores() - 1)
)

Sys.setenv(R_THREADS = 1)

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
library(DBI)
library(RSQLite)
library(sodium)
library(httr)
library(DBI)
library(mailR) # For email verification


# ---------- Load Custom Modules ----------
source_safe <- function(file) {
  tryCatch({
    source(file)
    cat("✓ Loaded:", file, "\n")
  }, error = function(e) {
    cat("✗ ERROR loading", file, ":", conditionMessage(e), "\n")
    stop(e)
  })
}

source_safe("translations.R")
source_safe("PrepareData.R")
source_safe("DashboardModule.R")
source_safe("EnhancedAuthenticationSystem.R")  # ✅ NEW
source_safe("AuthenticationUI.R")              # ✅ NEW

# Initialize database
if (!file.exists("lottery_users.db")) init_database()

# Load dashboard metrics
dashboard_path <- "dashboard"
if (dir.exists(dashboard_path)) {
  metric_files <- list.files(path = dashboard_path, pattern = "\\.R$", full.names = TRUE)
  if (length(metric_files) > 0) {
    for (f in metric_files) source_safe(f)
  }
}

# ---------- UI Theme ----------
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

# ============================================================================
# UI - SEPARATE, with language parameter
# ============================================================================
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  
  fluidPage(
    theme = app_theme,
    useShinyjs(),
    
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "6/49 Lotto-Analyse Tool - Kostenlos, interaktiv, bildungsbasiert."),
      tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
      
      # ✅ Redirect script for Stripe payments
      tags$script(HTML("
        Shiny.addCustomMessageHandler('redirect', function(url) {
          window.location.href = url;
        });
      "))
    ),
    
    # ==================== CONDITIONAL PANELS ====================
    
    # ✅ 1. LOGIN/REGISTER SCREEN (when not logged in)
    conditionalPanel(
      condition = "output.logged_in == false",
      auth_ui("auth_module")
    ),
    
    # ✅ 2. MAIN APP (when logged in)
    conditionalPanel(
      condition = "output.logged_in == true",
      
      # Language switcher
      div(class = "lang-switcher",
          tags$a(href = "?lang=de", class = paste0("lang-btn", if(LANG == "de") " active" else ""), "🇩🇪 DE"),
          tags$a(href = "?lang=en", class = paste0("lang-btn", if(LANG == "en") " active" else ""), "🇬🇧 EN")
      ),
      
      # Header
      div(class = "professional-header", role = "banner",
          div(class = "header-content",
              div(class = "logo-section",
                  span("🎲", class = "logo-icon"),
                  div(class = "logo-text",
                      h1(t("title", LANG), span(class = "testing-badge", t("testing_badge", LANG))),
                      p(t("subtitle", LANG))
                  )
              ),
              div(class = "header-nav", role = "navigation",
                  a(href = "#", t("nav_home", LANG)),
                  a(href = "#analyzer", t("nav_analyzer", LANG)),
                  a(href = "#subscription", "💳 Subscription"),  # ✅ NEW
                  a(href = "#educational", t("nav_educational", LANG)),
                  a(href = "#disclaimer", t("nav_disclaimer", LANG)),
                  # ✅ User menu
                  div(class = "user-menu",
                      textOutput("user_display"),
                      actionButton("logout_btn", "Logout", class = "btn-sm")
                  )
              )
          )
      ),
      
      # Main Content
      div(class = "main-content",
          
          # ✅ Subscription Section (NEW)
          div(id = "subscription", role = "region",
              subscription_ui("subscription_module")
          ),
          
          # Main Analyzer Section
          div(id = "analyzer", role = "region",
              layout_sidebar(
                sidebar = sidebar(
                  class = "control-panel",
                  open = "desktop",
                  position = "left",
                  h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #e8eaed;"),
                  lotteryInputUI("inputs1", lang = LANG)
                ),
                div(style = "padding: 0; min-height: 100vh;",
                    dashboardUI("dashboard1")
                ),
                fillable = FALSE
              )
          ),
          
          # Educational Notice
          div(class = "educational-notice", role = "note",
              h3(t("notice_title", LANG)),
              tags$ul(
                tags$li(strong(t("notice_1", LANG)), t("notice_1b", LANG)),
                tags$li(t("notice_2", LANG), strong(t("notice_2b", LANG)), t("notice_2c", LANG)),
                tags$li(t("notice_3", LANG)),
                tags$li(t("notice_4", LANG), strong(t("notice_4b", LANG)), t("notice_4c", LANG)),
                tags$li(t("notice_5", LANG)),
                tags$li(strong(t("notice_6", LANG)), t("notice_6b", LANG))
              )
          ),
          
          # Educational Section
          div(id = "educational", role = "region",
              style = "margin-top: 40px; padding: 30px; background: rgba(255,255,255,0.03); border-radius: 12px;",
              h2(t("edu_title", LANG), style = "color: #e8eaed;"),
              p(style = "color: rgba(255,255,255,0.7); line-height: 1.8;", t("edu_intro", LANG))
          )
      ),
      
      # Footer
      div(class = "professional-footer", role = "contentinfo",
          div(class = "footer-content",
              p(paste0("© ", format(Sys.Date(), "%Y"), " 6/49 ", t("footer_copyright", LANG)))
          )
      )
    )
  )
}

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {
  
  # ==================== AUTHENTICATION ====================
  user_info <- auth_server("auth_module")
  
  # Control visibility of login vs main app
  output$logged_in <- reactive({
    !is.null(user_info())
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Display username in header
  output$user_display <- renderText({
    req(user_info())
    paste0("👤 ", user_info()$username, " (", user_info()$subscription$plan_type, ")")
  })
  
  # Logout
  observeEvent(input$logout_btn, {
    user_info(NULL)
    session$reload()
  })
  
  # ==================== SUBSCRIPTION MODULE ====================
  observe({
    req(user_info())
    subscription_server("subscription_module", user_info)
  })
  
  # ==================== MAIN DASHBOARD ====================
  observe({
    req(user_info())
    
    # Check rate limits before allowing dashboard access
    rate_check <- check_rate_limit(user_info()$id, "dashboard_view")
    
    if (!rate_check$allowed) {
      showNotification(
        paste0("Rate limit reached! Upgrade your plan. (", 
               rate_check$current, "/", rate_check$limit, " today)"),
        type = "warning",
        duration = 10
      )
      return()
    }
    
    # Log dashboard access
    log_user_action(user_info()$id, "dashboard_view", "User accessed dashboard")
    
    # Load modules
    input_controls <- lotteryInputServer("inputs1")
    dashboardServer("dashboard1", input_controls = input_controls)
  })
  
  # ==================== PAYMENT SUCCESS HANDLER ====================
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$payment)) {
      if (query$payment == "success") {
        showNotification("✅ Payment successful! Your subscription has been upgraded.", 
                         type = "message", duration = 10)
        # Refresh user data
        if (!is.null(user_info())) {
          user_data <- user_info()
          user_data$subscription <- get_user_subscription(user_data$id)
          user_info(user_data)
        }
      } else if (query$payment == "cancel") {
        showNotification("Payment cancelled. You can try again anytime.", 
                         type = "warning", duration = 5)
      }
    }
  })
  
  # Health check
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$health)) {
      session$sendCustomMessage("health", "ok")
    }
  })
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server, enableBookmarking = "url")