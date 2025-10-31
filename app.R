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
source_safe("EnhancedAuthenticationSystem.R")
source_safe("AuthenticationUI.R")
source_safe("EmailVerificationSystem.R")  # Add this!

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
# UI - Clean Version (NO inline CSS)
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
      
      # Only redirect script - NO CSS
      tags$script(HTML("
        Shiny.addCustomMessageHandler('redirect', function(url) {
          window.location.href = url;
        });
      "))
    ),
    
    # ==================== FLOATING AUTH BUTTON ====================
    conditionalPanel(
      condition = "output.logged_in == false",
      actionButton("show_auth_modal", "🔓 Login / Sign Up", class = "floating-auth-btn")
    ),
    
    conditionalPanel(
      condition = "output.logged_in == true",
      div(class = "floating-auth-btn user-menu",
          textOutput("user_display_top"),
          actionButton("logout_btn_top", "Logout", class = "btn-sm")
      )
    ),
    
    # ==================== AUTH MODAL ====================
    shinyjs::hidden(
      div(id = "auth_modal_overlay",
          div(class = "auth-modal-container",
              actionButton("close_auth_modal", "×", class = "auth-modal-close"),
              auth_ui("auth_module")
          )
      )
    ),
    
    # ==================== LANGUAGE SWITCHER ====================
    div(class = "lang-switcher",
        tags$a(href = "?lang=de", class = paste0("lang-btn", if(LANG == "de") " active" else ""), "🇩🇪 DE"),
        tags$a(href = "?lang=en", class = paste0("lang-btn", if(LANG == "en") " active" else ""), "🇬🇧 EN")
    ),
    
    # ==================== HEADER ====================
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
                conditionalPanel(
                  condition = "output.logged_in == true",
                  a(href = "#subscription", "💳 Subscription")
                ),
                a(href = "#educational", t("nav_educational", LANG)),
                a(href = "#disclaimer", t("nav_disclaimer", LANG))
            )
        )
    ),
    
    # ==================== MAIN CONTENT ====================
    div(class = "main-content",
        
        # PUBLIC DASHBOARD
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
        
        # SUBSCRIPTION SECTION (logged in only)
        conditionalPanel(
          condition = "output.logged_in == true",
          div(id = "subscription", role = "region", class = "subscription-section",
              subscription_ui("subscription_module")
          )
        ),
        
        # PREMIUM TEASER (logged out only)
        conditionalPanel(
          condition = "output.logged_in == false",
          div(id = "premium-teaser", class = "premium-teaser-container",
              h2(class = "premium-teaser-title", "🔓 Unlock Premium Features"),
              
              div(class = "pricing-grid",
                  div(class = "premium-overlay",
                      span(class = "premium-badge", "🔒 PREMIUM"),
                      h3(style = "color: #e8eaed;", "Advanced Pattern Detection"),
                      p(style = "color: rgba(255,255,255,0.7);",
                        "AI-powered insights to identify complex patterns..."),
                      
                      div(class = "premium-unlock-card",
                          h4("Unlock This Feature"),
                          p("Sign up for free or upgrade to premium"),
                          actionButton("show_auth_from_teaser1", "Get Started", 
                                       class = "auth-trigger-btn")
                      )
                  ),
                  
                  div(class = "premium-overlay",
                      span(class = "premium-badge", "🔒 PREMIUM"),
                      h3(style = "color: #e8eaed;", "Statistical Forecasting"),
                      p(style = "color: rgba(255,255,255,0.7);",
                        "Advanced probability models and trend analysis..."),
                      
                      div(class = "premium-unlock-card",
                          h4("Unlock This Feature"),
                          p("Upgrade to premium for advanced analytics"),
                          actionButton("show_auth_from_teaser2", "Upgrade Now", 
                                       class = "auth-trigger-btn")
                      )
                  )
              )
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
        div(id = "educational", role = "region", class = "educational-section",
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
}

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {
  
  # ============================================================================
  # SECTION 1: AUTHENTICATION SETUP
  # ============================================================================
  
  # Initialize authentication
  user_info <- auth_server("auth_module")
  
  # ✅ NEW: Rate limiting for authentication attempts
  auth_attempts <- reactiveVal(list())
  
  # Helper function to check rate limit
  check_auth_rate_limit <- function(identifier, max_attempts = 5, window_minutes = 15) {
    attempts <- auth_attempts()
    cutoff_time <- Sys.time() - (window_minutes * 60)
    
    # Clean old attempts
    attempts <- Filter(function(x) {
      x$identifier == identifier && x$time > cutoff_time
    }, attempts)
    
    if (length(attempts) >= max_attempts) {
      return(list(
        allowed = FALSE,
        message = paste("Too many attempts. Try again in", window_minutes, "minutes.")
      ))
    }
    
    # Add current attempt
    attempts[[length(attempts) + 1]] <- list(
      identifier = identifier,
      time = Sys.time()
    )
    auth_attempts(attempts)
    
    list(allowed = TRUE, message = "")
  }
  
  # Expose login status for UI
  output$logged_in <- reactive({
    !is.null(user_info())
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Display username in header
  output$user_display_top <- renderText({
    req(user_info())
    paste0("👤 ", user_info()$username)
  })
  
  # ============================================================================
  # SECTION 2: MODAL CONTROLS
  # ============================================================================
  
  # Show auth modal
  observeEvent(input$show_auth_modal, {
    shinyjs::show("auth_modal_overlay")
  })
  
  observeEvent(input$show_auth_from_teaser1, {
    shinyjs::show("auth_modal_overlay")
  })
  
  observeEvent(input$show_auth_from_teaser2, {
    shinyjs::show("auth_modal_overlay")
  })
  
  # Close auth modal
  observeEvent(input$close_auth_modal, {
    shinyjs::hide("auth_modal_overlay")
  })
  
  # Auto-hide modal on successful login
  observe({
    req(user_info())
    shinyjs::hide("auth_modal_overlay")
  })
  
  # Logout handler
  observeEvent(input$logout_btn_top, {
    user_info(NULL)
    session$reload()
  })
  
  # ============================================================================
  # SECTION 3: SUBSCRIPTION MANAGEMENT
  # ============================================================================
  
  # Initialize subscription module
  observe({
    req(user_info())
    subscription_server("subscription_module", user_info)
  })
  
  # ============================================================================
  # SECTION 4: DASHBOARD (PUBLIC ACCESS)
  # ============================================================================
  
  input_controls <- lotteryInputServer("inputs1")
  dashboardServer("dashboard1", input_controls = input_controls)
  
  # ============================================================================
  # SECTION 5: PAYMENT VERIFICATION (Stripe)
  # ============================================================================
  
  # ✅ FIXED: Unified payment handler with proper error handling
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    # Handle NEW Stripe Checkout flow (with session_id verification)
    if (!is.null(query$session_id)) {
      
      # ✅ NEW: Add error handling wrapper
      result <- tryCatch({
        verify_stripe_payment(query$session_id)
      }, error = function(e) {
        message("Payment verification error: ", e$message)
        list(success = FALSE, error = paste("Verification failed:", e$message))
      })
      
      if (!is.null(result) && result$success) {
        # Determine language for notification
        lang <- query$lang %||% "de"
        success_msg <- if (lang == "de") {
          paste("✅ Zahlung erfolgreich! Willkommen im", result$plan_type, "Plan!")
        } else {
          paste("✅ Payment successful! Welcome to", result$plan_type, "plan!")
        }
        
        showNotification(
          success_msg,
          type = "message",
          duration = 10
        )
        
        # Refresh user data
        if (!is.null(user_info())) {
          updated_user <- user_info()
          updated_user$subscription <- get_user_subscription(result$user_id)
          user_info(updated_user)
        }
        
        # ✅ NEW: Wrap email sending in tryCatch
        tryCatch({
          user <- get_user_info(result$user_id)
          send_subscription_confirmation(
            email = user$email,
            username = user$username,
            plan_type = result$plan_type,
            plan_details = subscription_plans[[result$plan_type]]
          )
        }, error = function(e) {
          message("Email sending failed: ", e$message)
          # Don't fail the whole transaction if email fails
        })
        
        # Redirect to clean URL with language preserved
        clean_url <- paste0("?lang=", lang)
        shinyjs::delay(2000, {
          session$sendCustomMessage("redirect", clean_url)
        })
        
      } else {
        lang <- query$lang %||% "de"
        error_msg <- if (lang == "de") {
          paste("❌ Zahlungsüberprüfung fehlgeschlagen:", result$error)
        } else {
          paste("❌ Payment verification failed:", result$error)
        }
        
        showNotification(
          error_msg,
          type = "error",
          duration = 10
        )
      }
    }
    
    # Handle OLD payment flow (for backwards compatibility or fallback)
    else if (!is.null(query$payment)) {
      lang <- query$lang %||% "de"
      
      if (query$payment == "success") {
        success_msg <- if (lang == "de") {
          "✅ Zahlung erfolgreich!"
        } else {
          "✅ Payment successful!"
        }
        
        showNotification(
          success_msg, 
          type = "message", 
          duration = 10
        )
        
        if (!is.null(user_info())) {
          user_data <- user_info()
          user_data$subscription <- get_user_subscription(user_data$id)
          user_info(user_data)
        }
        
      } else if (query$payment == "cancel") {
        cancel_msg <- if (lang == "de") {
          "⚠️ Zahlung abgebrochen."
        } else {
          "⚠️ Payment cancelled."
        }
        
        showNotification(
          cancel_msg, 
          type = "warning", 
          duration = 5
        )
      }
    }
  })
  
  # ============================================================================
  # SECTION 6: STRIPE CHECKOUT HANDLERS
  # ============================================================================
  
  # ✅ FIXED: Changed user_data() to user_info() throughout
  # Handle BASIC plan upgrade
  observeEvent(input$upgrade_basic, {
    req(user_info())  # ✅ FIXED: was user_data()
    
    # ✅ NEW: Check rate limit
    rate_check <- check_auth_rate_limit(
      identifier = paste0("upgrade_", user_info()$id),
      max_attempts = 3,
      window_minutes = 60
    )
    
    if (!rate_check$allowed) {
      showNotification(
        rate_check$message,
        type = "warning",
        duration = 10
      )
      return()
    }
    
    base_url <- session$clientData$url_protocol
    host <- session$clientData$url_hostname
    port <- session$clientData$url_port
    
    # Build URLs correctly
    if (port == "") {
      app_url <- paste0(base_url, "//", host)
    } else {
      app_url <- paste0(base_url, "//", host, ":", port)
    }
    
    # Get current language from query string
    query <- parseQueryString(session$clientData$url_search)
    lang_param <- if (!is.null(query$lang)) paste0("&lang=", query$lang) else ""
    
    checkout <- tryCatch({
      create_stripe_checkout(
        user_id = user_info()$id,  # ✅ FIXED: was user_data()
        plan_type = "basic",
        success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}", lang_param),
        cancel_url = paste0(app_url, "?payment=cancel", lang_param)
      )
    }, error = function(err) {
      message("Checkout error: ", err$message)
      list(success = FALSE, error = err$message)
    })
    
    if (!is.null(checkout$success) && checkout$success) {
      session$sendCustomMessage("redirect", checkout$url)
    } else {
      error_msg <- if (!is.null(checkout$error)) checkout$error else "Unknown error occurred"
      showNotification(paste("Error:", error_msg), type = "error", duration = 10)
    }
  })
  
  # Handle PREMIUM plan upgrade
  observeEvent(input$upgrade_premium, {
    req(user_info())  # ✅ FIXED: was user_data()
    
    # ✅ NEW: Check rate limit
    rate_check <- check_auth_rate_limit(
      identifier = paste0("upgrade_", user_info()$id),
      max_attempts = 3,
      window_minutes = 60
    )
    
    if (!rate_check$allowed) {
      showNotification(
        rate_check$message,
        type = "warning",
        duration = 10
      )
      return()
    }
    
    base_url <- session$clientData$url_protocol
    host <- session$clientData$url_hostname
    port <- session$clientData$url_port
    
    if (port == "") {
      app_url <- paste0(base_url, "//", host)
    } else {
      app_url <- paste0(base_url, "//", host, ":", port)
    }
    
    # Get current language from query string
    query <- parseQueryString(session$clientData$url_search)
    lang_param <- if (!is.null(query$lang)) paste0("&lang=", query$lang) else ""
    
    checkout <- tryCatch({
      create_stripe_checkout(
        user_id = user_info()$id,  # ✅ FIXED: was user_data()
        plan_type = "premium",
        success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}", lang_param),
        cancel_url = paste0(app_url, "?payment=cancel", lang_param)
      )
    }, error = function(err) {
      message("Checkout error: ", err$message)
      list(success = FALSE, error = err$message)
    })
    
    if (!is.null(checkout$success) && checkout$success) {
      session$sendCustomMessage("redirect", checkout$url)
    } else {
      error_msg <- if (!is.null(checkout$error)) checkout$error else "Unknown error occurred"
      showNotification(paste("Error:", error_msg), type = "error", duration = 10)
    }
  })
  
  # ============================================================================
  # SECTION 5B: EMAIL VERIFICATION HANDLER
  # ============================================================================
  
  # Handle email verification links (?verify=TOKEN)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$verify)) {
      result <- tryCatch({
        handle_email_verification(query$verify)
      }, error = function(e) {
        list(success = FALSE, error = paste("Verification error:", e$message))
      })
      
      lang <- query$lang %||% "de"
      
      if (result$success) {
        success_msg <- if (lang == "de") {
          "✅ Email erfolgreich verifiziert! Sie können sich jetzt anmelden."
        } else {
          "✅ Email verified successfully! You can now log in."
        }
        
        showNotification(
          success_msg,
          type = "message",
          duration = 10
        )
        
        # Redirect to clean URL
        shinyjs::delay(2000, {
          session$sendCustomMessage("redirect", paste0("?lang=", lang))
        })
        
      } else {
        error_msg <- if (lang == "de") {
          paste("❌ Verifizierung fehlgeschlagen:", result$error)
        } else {
          paste("❌ Verification failed:", result$error)
        }
        
        showNotification(
          error_msg,
          type = "error",
          duration = 10
        )
      }
    }
  })
  
  # ============================================================================
  # SECTION 7: SESSION CLEANUP
  # ============================================================================
  
  # ✅ NEW: Proper session cleanup to prevent memory leaks
  session$onSessionEnded(function() {
    message("🧹 Cleaning up session for user: ", 
            ifelse(is.null(user_info()), "anonymous", user_info()$username))
    
    # Clear authentication data
    user_info(NULL)
    auth_attempts(list())
    
    # Force garbage collection
    gc()
    gc()
    
    message("✅ Session cleanup completed")
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")