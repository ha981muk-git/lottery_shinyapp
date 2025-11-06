# ============================================================================
# EnhancedAuthenticationSystem.R - Compatibility Layer
# ============================================================================
# This file maintains backward compatibility while using modular architecture

# Load modular components
source("DatabaseManager.R")
source("StripeManager.R")
source("AuthManager.R")
source("SubscriptionManager.R")

# ============================================================================
# BACKWARD COMPATIBILITY WRAPPERS
# ============================================================================
# Your app.R still calls these functions, so we keep them but delegate to modules

# Database functions (delegate to database_manager.R)
init_database <- function(db_path = "lottery_users.db") {
  # Just call the modular version
  source("DatabaseManager.R")
  init_database(db_path)
}

backup_database <- function(db_path = "lottery_users.db") {
  source("DatabaseManager.R")
  backup_database(db_path)
}

# Auth functions (delegate to auth_manager.R)
register_user <- function(username, email, password, db_path = "lottery_users.db") {
  source("AuthManager.R")
  register_user(username, email, password, db_path)
}

verify_user <- function(username, password, csrf_token = NULL, db_path = "lottery_users.db") {
  source("AuthManager.R")
  verify_user(username, password, csrf_token, db_path)
}

get_user_info <- function(user_id, db_path = "lottery_users.db") {
  source("AuthManager.R")
  get_user_info(user_id, db_path)
}

get_user_subscription <- function(user_id, db_path = "lottery_users.db") {
  source("AuthManager.R")
  get_user_subscription(user_id, db_path)
}

verify_email_token <- function(token, db_path = "lottery_users.db") {
  source("AuthManager.R")
  verify_email_token(token, db_path)
}

request_password_reset <- function(email, db_path = "lottery_users.db") {
  source("AuthManager.R")
  request_password_reset(email, db_path)
}

reset_password <- function(token, new_password, db_path = "lottery_users.db") {
  source("AuthManager.R")
  reset_password(token, new_password, db_path)
}

log_user_action <- function(user_id, action, details = "", db_path = "lottery_users.db") {
  source("DatabaseManager.R")
  db_log_action(user_id, action, details, db_path)
}

check_rate_limit <- function(user_id, action = "api_call", db_path = "lottery_users.db") {
  source("AuthManager.R")
  check_rate_limit(user_id, action, db_path)
}

# Stripe functions (delegate to subscription_manager.R)
create_stripe_checkout <- function(user_id, plan_type, success_url, cancel_url, db_path = "lottery_users.db") {
  source("SubscriptionManager.R")
  create_checkout_session(user_id, plan_type, success_url, cancel_url, db_path)
}

verify_stripe_payment <- function(session_id, db_path = "lottery_users.db") {
  source("SubscriptionManager.R")
  verify_and_upgrade(session_id, db_path)
}

upgrade_subscription <- function(user_id, plan_type, stripe_subscription_id = NULL, db_path = "lottery_users.db") {
  source("SubscriptionManager.R")
  upgrade_subscription(user_id, plan_type, stripe_subscription_id, db_path)
}

cancel_stripe_subscription <- function(subscription_id) {
  source("StripeManager.R")
  stripe_cancel_subscription(subscription_id)
}

# Shiny modules (keep as-is from your existing file)
auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user_info <- reactiveVal(NULL)
    
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pass)
      res <- verify_user(input$login_user, input$login_pass)
      
      if (res$success) {
        # ✅ Use a fallback to handle missing user_id safely
        uid <- res$user_id %||% user_info()$id
        
        user <- get_user_info(uid)
        subscription <- get_user_subscription(uid)
        
        # ✅ Keep naming consistent with rest of your app (user_info reactive)
        user_info(list(
          id = uid,
          username = user$username,
          email = user$email,
          email_verified = res$email_verified,
          subscription = subscription
        ))
        
        
        output$login_status_ui <- renderUI({
          div(class = "auth-status success", "✅ Login successful!")
        })
      } else {
        output$login_status_ui <- renderUI({
          div(class = "auth-status error", paste("❌", res$error))
        })
      }
    })
    
    observeEvent(input$reg_btn, {
      req(input$reg_user, input$reg_email, input$reg_pass)
      res <- register_user(input$reg_user, input$reg_email, input$reg_pass)
      
      if (res$success) {
        output$reg_status_ui <- renderUI({
          div(class = "auth-status success", 
              "✅ Account created! Check your email for verification link.")
        })
      } else {
        output$reg_status_ui <- renderUI({
          div(class = "auth-status error", paste("❌", res$error))
        })
      }
    })
    
    return(user_info)
    
  })
}

subscription_server <- function(id, user_info) {
  moduleServer(id, function(input, output, session) {
    # Keep all your existing subscription_server code here
    # (I won't repeat it - just copy from your existing file)
  })
}

# Export subscription plans for backward compatibility
subscription_plans <- list(
  free = list(
    name = "Free",
    price = 0,
    currency = "EUR",
    interval = "month",
    features = c("Basic number analysis", "Limited historical data"),
    limits = list(api_calls_per_day = 100, exports_per_month = 5)
  ),
  basic = list(
    name = "Basic",
    price = 9.99,
    currency = "EUR",
    interval = "month",
    features = c("Advanced pattern detection", "Full historical data"),
    limits = list(api_calls_per_day = 1000, exports_per_month = 100)
  ),
  premium = list(
    name = "Premium",
    price = 24.99,
    currency = "EUR",
    interval = "month",
    features = c("Everything in Basic", "AI-powered insights"),
    limits = list(api_calls_per_day = 10000, exports_per_month = -1)
  )
)