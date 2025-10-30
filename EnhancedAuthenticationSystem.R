# ============================================================================
# EnhancedAuthenticationSystem.R - Complete Auth + Subscription Backend
# ===========================================================================

# ----------------------------------------------------------------------------
# 1. DATABASE INITIALIZATION (Enhanced)
# ----------------------------------------------------------------------------
init_database <- function(db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  
  # Users table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      email TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_login TIMESTAMP,
      is_active INTEGER DEFAULT 1,
      email_verified INTEGER DEFAULT 0,
      verification_token TEXT,
      reset_token TEXT,
      reset_token_expiry TIMESTAMP
    )
  ")
  
  # Subscriptions table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS subscriptions (
      subscription_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      plan_type TEXT NOT NULL,
      status TEXT NOT NULL,
      stripe_subscription_id TEXT,
      stripe_customer_id TEXT,
      start_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      end_date TIMESTAMP,
      auto_renew INTEGER DEFAULT 1,
      FOREIGN KEY (user_id) REFERENCES users(user_id)
    )
  ")
  
  # Usage tracking table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS usage_logs (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      action TEXT NOT NULL,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      details TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id)
    )
  ")
  
  dbDisconnect(con)
  message("✓ Database initialized with enhanced tables")
  return(TRUE)
}


# ----------------------------------------------------------------------------
# 2. USER MANAGEMENT (Enhanced)
# ----------------------------------------------------------------------------
register_user <- function(username, email, password, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  # Validation
  if (nchar(password) < 8) {
    return(list(success = FALSE, error = "Password must be at least 8 characters"))
  }
  
  if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)) {
    return(list(success = FALSE, error = "Invalid email format"))
  }
  
  password_hash <- sodium::password_store(password)
  verification_token <- paste0(sample(c(0:9, letters), 32, replace = TRUE), collapse = "")
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO users (username, email, password_hash, verification_token)
      VALUES (?, ?, ?, ?)
    ", params = list(username, email, password_hash, verification_token))
    
    user_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    
    # Create free subscription
    dbExecute(con, "
      INSERT INTO subscriptions (user_id, plan_type, status)
      VALUES (?, 'free', 'active')
    ", params = list(user_id))
    
    # Log registration
    log_user_action(user_id, "registration", "New user registered", db_path)
    
    list(
      success = TRUE, 
      user_id = user_id,
      verification_token = verification_token,
      message = "Account created! Please verify your email."
    )
  }, error = function(e) {
    if (grepl("UNIQUE constraint failed: users.username", e$message)) {
      return(list(success = FALSE, error = "Username already exists"))
    } else if (grepl("UNIQUE constraint failed: users.email", e$message)) {
      return(list(success = FALSE, error = "Email already registered"))
    }
    list(success = FALSE, error = e$message)
  })
}

verify_user <- function(username, password, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT user_id, password_hash, is_active, email_verified FROM users
    WHERE username = ? OR email = ?
  ", params = list(username, username))
  
  if (nrow(user) == 0) return(list(success = FALSE, error = "User not found"))
  if (!user$is_active) return(list(success = FALSE, error = "Account inactive"))
  
  if (sodium::password_verify(user$password_hash, password)) {
    dbExecute(con, "
      UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE user_id = ?
    ", params = list(user$user_id))
    
    log_user_action(user$user_id, "login", "User logged in", db_path)
    
    list(
      success = TRUE, 
      user_id = user$user_id,
      email_verified = as.logical(user$email_verified)
    )
  } else {
    list(success = FALSE, error = "Invalid password")
  }
}

get_user_subscription <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  s <- dbGetQuery(con, "
    SELECT plan_type, status, start_date, end_date, auto_renew 
    FROM subscriptions
    WHERE user_id = ? AND status = 'active' 
    ORDER BY subscription_id DESC LIMIT 1
  ", params = list(user_id))
  
  if (nrow(s) == 0) return(list(plan_type = "free", status = "active"))
  as.list(s[1,])
}

get_user_info <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT username, email, created_at, email_verified FROM users
    WHERE user_id = ?
  ", params = list(user_id))
  
  if (nrow(user) == 0) return(NULL)
  as.list(user[1,])
}


# ----------------------------------------------------------------------------
# 3. EMAIL VERIFICATION & PASSWORD RESET
# ----------------------------------------------------------------------------
verify_email_token <- function(token, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT user_id FROM users WHERE verification_token = ? AND email_verified = 0
  ", params = list(token))
  
  if (nrow(user) == 0) {
    return(list(success = FALSE, error = "Invalid or expired token"))
  }
  
  dbExecute(con, "
    UPDATE users SET email_verified = 1, verification_token = NULL WHERE user_id = ?
  ", params = list(user$user_id))
  
  log_user_action(user$user_id, "email_verified", "Email verified", db_path)
  
  list(success = TRUE, message = "Email verified successfully!")
}

request_password_reset <- function(email, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "SELECT user_id FROM users WHERE email = ?", params = list(email))
  
  if (nrow(user) == 0) {
    # Don't reveal if email exists (security)
    return(list(success = TRUE, message = "If email exists, reset link sent"))
  }
  
  reset_token <- paste0(sample(c(0:9, letters), 32, replace = TRUE), collapse = "")
  expiry <- format(Sys.time() + 3600, "%Y-%m-%d %H:%M:%S") # 1 hour
  
  dbExecute(con, "
    UPDATE users SET reset_token = ?, reset_token_expiry = ? WHERE user_id = ?
  ", params = list(reset_token, expiry, user$user_id))
  
  log_user_action(user$user_id, "password_reset_requested", "Reset token generated", db_path)
  
  list(
    success = TRUE, 
    reset_token = reset_token,
    message = "Reset link sent to email"
  )
}

reset_password <- function(token, new_password, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  if (nchar(new_password) < 8) {
    return(list(success = FALSE, error = "Password must be at least 8 characters"))
  }
  
  user <- dbGetQuery(con, "
    SELECT user_id FROM users 
    WHERE reset_token = ? AND reset_token_expiry > CURRENT_TIMESTAMP
  ", params = list(token))
  
  if (nrow(user) == 0) {
    return(list(success = FALSE, error = "Invalid or expired reset token"))
  }
  
  password_hash <- sodium::password_store(new_password)
  
  dbExecute(con, "
    UPDATE users SET password_hash = ?, reset_token = NULL, reset_token_expiry = NULL 
    WHERE user_id = ?
  ", params = list(password_hash, user$user_id))
  
  log_user_action(user$user_id, "password_reset", "Password changed via reset", db_path)
  
  list(success = TRUE, message = "Password reset successful")
}


# ----------------------------------------------------------------------------
# 4. SUBSCRIPTION PLANS & STRIPE
# ----------------------------------------------------------------------------
subscription_plans <- list(
  free = list(
    name = "Free",
    price = 0,
    currency = "EUR",
    interval = "month",
    features = c(
      "Basic number analysis",
      "Limited historical data",
      "Community support",
      "Educational resources"
    ),
    limits = list(
      api_calls_per_day = 100,
      exports_per_month = 5
    )
  ),
  basic = list(
    name = "Basic",
    price = 9.99,
    currency = "EUR",
    interval = "month",
    features = c(
      "Advanced pattern detection",
      "Full historical data access",
      "Custom date ranges",
      "Export functionality",
      "Priority email support"
    ),
    limits = list(
      api_calls_per_day = 1000,
      exports_per_month = 100
    )
  ),
  premium = list(
    name = "Premium",
    price = 24.99,
    currency = "EUR",
    interval = "month",
    features = c(
      "Everything in Basic",
      "AI-powered insights",
      "Statistical forecasting",
      "API access",
      "Custom notifications",
      "24/7 priority support",
      "Early access to features"
    ),
    limits = list(
      api_calls_per_day = 10000,
      exports_per_month = -1  # unlimited
    )
  )
)

STRIPE_SECRET_KEY <- Sys.getenv("STRIPE_SECRET_KEY")

create_stripe_checkout <- function(user_id, plan_type, success_url, cancel_url, db_path = "lottery_users.db") {
  if (STRIPE_SECRET_KEY == "" || is.null(STRIPE_SECRET_KEY)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  plan <- subscription_plans[[plan_type]]
  
  # Get or create Stripe customer
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- tryCatch({
    dbGetQuery(con, "
      SELECT u.email, s.stripe_customer_id 
      FROM users u
      LEFT JOIN subscriptions s ON u.user_id = s.user_id
      WHERE u.user_id = ?
      LIMIT 1
    ", params = list(user_id))
  }, error = function(err) {
    message("Database error: ", err$message)
    return(NULL)
  })
  
  if (is.null(user) || nrow(user) == 0) {
    return(list(success = FALSE, error = "User not found"))
  }
  
  customer_id <- user$stripe_customer_id
  
  if (is.na(customer_id) || customer_id == "") {
    # Create Stripe customer
    customer_res <- tryCatch({
      POST(
        url = "https://api.stripe.com/v1/customers",
        add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)),
        body = list(email = user$email),
        encode = "form"
      )
    }, error = function(err) {
      message("Stripe customer creation error: ", err$message)
      return(NULL)
    })
    
    if (!is.null(customer_res) && status_code(customer_res) == 200) {
      customer_id <- content(customer_res)$id
      tryCatch({
        dbExecute(con, "
          UPDATE subscriptions 
          SET stripe_customer_id = ? 
          WHERE user_id = ?
        ", params = list(customer_id, user_id))
      }, error = function(err) {
        message("Failed to save customer ID: ", err$message)
      })
    } else {
      return(list(success = FALSE, error = "Failed to create Stripe customer"))
    }
  }
  
  # Create checkout session
  # Create checkout session
  session_res <- tryCatch({
    POST(
      url = "https://api.stripe.com/v1/checkout/sessions",
      add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)),
      body = list(
        customer = customer_id,
        "payment_method_types[]" = "card",
        "line_items[0][price_data][currency]" = tolower(plan$currency),
        "line_items[0][price_data][unit_amount]" = as.integer(plan$price * 100),
        "line_items[0][price_data][product_data][name]" = plan$name,
        "line_items[0][price_data][recurring][interval]" = plan$interval,
        "line_items[0][quantity]" = 1,
        mode = "subscription",
        success_url = success_url,
        cancel_url = cancel_url,
        "metadata[user_id]" = as.character(user_id),
        "metadata[plan_type]" = plan_type
      ),
      encode = "form"
    )
  }, error = function(err) {
    message("Stripe checkout error: ", err$message)
    return(NULL)
  })
  
  if (!is.null(session_res) && status_code(session_res) == 200) {
    session_data <- content(session_res)
    log_user_action(user_id, "checkout_initiated", paste("Plan:", plan_type), db_path)
    return(list(success = TRUE, url = session_data$url, session_id = session_data$id))
  } else {
    error_msg <- if (!is.null(session_res)) {
      tryCatch(content(session_res)$error$message, error = function(err) "Unknown error")
    } else {
      "Failed to connect to Stripe"
    }
    return(list(success = FALSE, error = error_msg))
  }
}


# Add after create_stripe_checkout function
verify_stripe_payment <- function(session_id, db_path = "lottery_users.db") {
  if (STRIPE_SECRET_KEY == "" || is.null(STRIPE_SECRET_KEY)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  # Retrieve checkout session from Stripe
  session_res <- tryCatch({
    GET(
      url = paste0("https://api.stripe.com/v1/checkout/sessions/", session_id),
      add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY))
    )
  }, error = function(err) {
    message("Stripe session retrieval error: ", err$message)
    return(NULL)
  })
  
  if (is.null(session_res) || status_code(session_res) != 200) {
    return(list(success = FALSE, error = "Could not verify payment"))
  }
  
  session_data <- content(session_res)
  
  # Check payment status
  if (session_data$payment_status != "paid") {
    return(list(success = FALSE, error = "Payment not completed"))
  }
  
  # Extract metadata
  user_id <- as.numeric(session_data$metadata$user_id)
  plan_type <- session_data$metadata$plan_type
  stripe_subscription_id <- session_data$subscription
  
  # Upgrade subscription in database
  result <- upgrade_subscription(
    user_id = user_id,
    plan_type = plan_type,
    stripe_subscription_id = stripe_subscription_id,
    db_path = db_path
  )
  
  if (result$success) {
    return(list(
      success = TRUE,
      user_id = user_id,
      plan_type = plan_type,
      message = "Payment verified and subscription upgraded"
    ))
  } else {
    return(list(success = FALSE, error = result$message))
  }
}

upgrade_subscription <- function(user_id, plan_type, stripe_subscription_id = NULL, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  # Deactivate old subscription
  dbExecute(con, "UPDATE subscriptions SET status = 'cancelled' WHERE user_id = ? AND status = 'active'",
            params = list(user_id))
  
  # Create new subscription
  dbExecute(con, "
    INSERT INTO subscriptions (user_id, plan_type, status, stripe_subscription_id)
    VALUES (?, ?, 'active', ?)
  ", params = list(user_id, plan_type, stripe_subscription_id))
  
  log_user_action(user_id, "subscription_upgraded", paste("New plan:", plan_type), db_path)
  
  list(success = TRUE, message = paste("Upgraded to", plan_type))
}


# ----------------------------------------------------------------------------
# 5. USAGE TRACKING & RATE LIMITING
# ----------------------------------------------------------------------------
log_user_action <- function(user_id, action, details = "", db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "
    INSERT INTO usage_logs (user_id, action, details)
    VALUES (?, ?, ?)
  ", params = list(user_id, action, details))
}

check_rate_limit <- function(user_id, action = "api_call", db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  subscription <- get_user_subscription(user_id, db_path)
  plan <- subscription_plans[[subscription$plan_type]]
  
  # Count actions today
  count <- dbGetQuery(con, "
    SELECT COUNT(*) as count FROM usage_logs
    WHERE user_id = ? AND action = ? AND DATE(timestamp) = DATE('now')
  ", params = list(user_id, action))$count
  
  limit <- plan$limits$api_calls_per_day
  
  list(
    allowed = count < limit,
    current = count,
    limit = limit,
    plan = subscription$plan_type
  )
}


# ----------------------------------------------------------------------------
# 6. AUTH SERVER MODULE (Enhanced)
# ----------------------------------------------------------------------------
auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user_data <- reactiveVal(NULL)
    
    # Login
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pass)
      
      res <- verify_user(input$login_user, input$login_pass)
      
      if (res$success) {
        user_info <- get_user_info(res$user_id)
        subscription <- get_user_subscription(res$user_id)
        
        user_data(list(
          id = res$user_id,
          username = user_info$username,
          email = user_info$email,
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
    
    # Registration
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
    
    return(user_data)
  })
}


# ----------------------------------------------------------------------------
# 7. SUBSCRIPTION SERVER MODULE
# ----------------------------------------------------------------------------
subscription_server <- function(id, user_data) {
  moduleServer(id, function(input, output, session) {
    
    # Current plan banner
    output$current_plan_banner <- renderUI({
      req(user_data())
      
      sub <- user_data()$subscription
      plan <- subscription_plans[[sub$plan_type]]
      
      div(class = "current-plan-banner",
          h3(paste("Current Plan:", plan$name)),
          p(paste("Status:", sub$status)),
          if (sub$plan_type != "free") {
            p(paste("Next billing:", format(as.Date(sub$end_date), "%B %d, %Y")))
          }
      )
    })
    
    # Plan buttons
    output$free_btn <- renderUI({
      req(user_data())
      current_plan <- user_data()$subscription$plan_type
      
      if (current_plan == "free") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("downgrade_free"), "Downgrade", 
                     class = "pricing-btn pricing-btn-primary")
      }
    })
    
    output$basic_btn <- renderUI({
      req(user_data())
      current_plan <- user_data()$subscription$plan_type
      
      if (current_plan == "basic") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("upgrade_basic"), 
                     if(current_plan == "free") "Upgrade" else "Switch Plan",
                     class = "pricing-btn pricing-btn-primary")
      }
    })
    
    output$premium_btn <- renderUI({
      req(user_data())
      current_plan <- user_data()$subscription$plan_type
      
      if (current_plan == "premium") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("upgrade_premium"), "Upgrade", 
                     class = "pricing-btn pricing-btn-primary")
      }
    })
    
    # Handle upgrades
    # Handle upgrades - FIXED
    observeEvent(input$upgrade_basic, {
      req(user_data())
      
      base_url <- session$clientData$url_protocol
      host <- session$clientData$url_hostname
      port <- session$clientData$url_port
      
      # Build URLs correctly
      if (port == "") {
        app_url <- paste0(base_url, "//", host)
      } else {
        app_url <- paste0(base_url, "//", host, ":", port)
      }
      
      checkout <- tryCatch({
        create_stripe_checkout(
          user_id = user_data()$id,
          plan_type = "basic",
          success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}"),
          cancel_url = paste0(app_url, "?payment=cancel")
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
    
    observeEvent(input$upgrade_premium, {
      req(user_data())
      
      base_url <- session$clientData$url_protocol
      host <- session$clientData$url_hostname
      port <- session$clientData$url_port
      
      if (port == "") {
        app_url <- paste0(base_url, "//", host)
      } else {
        app_url <- paste0(base_url, "//", host, ":", port)
      }
      
      checkout <- tryCatch({
        create_stripe_checkout(
          user_id = user_data()$id,
          plan_type = "premium",
          success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}"),
          cancel_url = paste0(app_url, "?payment=cancel")
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
  })
}