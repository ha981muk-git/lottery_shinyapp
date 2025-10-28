# ============================================================================
# auth_system.R - Authentication + Subscription System
# ============================================================================

# ----------------------------------------------------------------------------
# 1. DATABASE INITIALIZATION
# ----------------------------------------------------------------------------
init_database <- function(db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      email TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_login TIMESTAMP,
      is_active INTEGER DEFAULT 1,
      email_verified INTEGER DEFAULT 0
    )
  ")
  
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
  
  dbDisconnect(con)
  message("✓ Database initialized")
  return(TRUE)
}


# ----------------------------------------------------------------------------
# 2. USER MANAGEMENT
# ----------------------------------------------------------------------------
register_user <- function(username, email, password, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  password_hash <- sodium::password_store(password)
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO users (username, email, password_hash)
      VALUES (?, ?, ?)
    ", params = list(username, email, password_hash))
    
    user_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    dbExecute(con, "
      INSERT INTO subscriptions (user_id, plan_type, status)
      VALUES (?, 'free', 'active')
    ", params = list(user_id))
    
    list(success = TRUE, user_id = user_id)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

verify_user <- function(username, password, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT user_id, password_hash, is_active FROM users
    WHERE username = ? OR email = ?
  ", params = list(username, username))
  
  if (nrow(user) == 0) return(list(success = FALSE, error = "User not found"))
  if (!user$is_active) return(list(success = FALSE, error = "Account inactive"))
  
  if (sodium::password_verify(user$password_hash, password)) {
    dbExecute(con, "
      UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE user_id = ?
    ", params = list(user$user_id))
    list(success = TRUE, user_id = user$user_id)
  } else {
    list(success = FALSE, error = "Invalid password")
  }
}

get_user_subscription <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  s <- dbGetQuery(con, "
    SELECT plan_type, status FROM subscriptions
    WHERE user_id = ? AND status = 'active' ORDER BY subscription_id DESC LIMIT 1
  ", params = list(user_id))
  
  if (nrow(s) == 0) return(list(plan_type = "free", status = "active"))
  as.list(s[1,])
}



# ----------------------------------------------------------------------------
# 3. SUBSCRIPTION PLANS
# ----------------------------------------------------------------------------
subscription_plans <- list(
  free = list(name = "Free", price = 0, currency = "EUR", interval = "month"),
  basic = list(name = "Basic", price = 9.99, currency = "EUR", interval = "month"),
  premium = list(name = "Premium", price = 24.99, currency = "EUR", interval = "month")
)

# ----------------------------------------------------------------------------
# 4. STRIPE CHECKOUT (Simplified)
# ----------------------------------------------------------------------------
STRIPE_SECRET_KEY <- Sys.getenv("STRIPE_SECRET_KEY")

create_stripe_checkout <- function(user_id, plan_type, success_url, cancel_url) {
  plan <- subscription_plans[[plan_type]]
  res <- POST(
    url = "https://api.stripe.com/v1/checkout/sessions",
    add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)),
    body = list(
      payment_method_types = list("card"),
      line_items = list(
        list(
          price_data = list(
            currency = tolower(plan$currency),
            unit_amount = as.integer(plan$price * 100),
            product_data = list(name = plan$name),
            recurring = list(interval = plan$interval)
          ),
          quantity = 1
        )
      ),
      mode = "subscription",
      success_url = success_url,
      cancel_url = cancel_url
    ),
    encode = "form"
  )
  if (status_code(res) == 200) {
    content(res)
  } else {
    list(error = content(res)$error$message)
  }
}


# ----------------------------------------------------------------------------
# 5. AUTH UI MODULE
# ----------------------------------------------------------------------------
auth_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "auth-container",
        h3("User Login / Registration"),
        tabsetPanel(
          tabPanel("Login",
                   textInput(ns("login_user"), "Username or Email"),
                   passwordInput(ns("login_pass"), "Password"),
                   actionButton(ns("login_btn"), "Login", class = "btn-primary w-100"),
                   textOutput(ns("login_status"))
          ),
          tabPanel("Register",
                   textInput(ns("reg_user"), "Username"),
                   textInput(ns("reg_email"), "Email"),
                   passwordInput(ns("reg_pass"), "Password"),
                   actionButton(ns("reg_btn"), "Register", class = "btn-success w-100"),
                   textOutput(ns("reg_status"))
          )
        )
    )
  )
}

# ----------------------------------------------------------------------------
# 6. AUTH SERVER MODULE
# ----------------------------------------------------------------------------
auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user_data <- reactiveVal(NULL)
    
    observeEvent(input$login_btn, {
      res <- verify_user(input$login_user, input$login_pass)
      if (res$success) {
        user_data(list(id = res$user_id))
        output$login_status <- renderText("✅ Login successful")
      } else {
        output$login_status <- renderText(paste("❌", res$error))
      }
    })
    
    observeEvent(input$reg_btn, {
      if (input$reg_pass == "") {
        output$reg_status <- renderText("❌ Password required")
        return()
      }
      res <- register_user(input$reg_user, input$reg_email, input$reg_pass)
      if (res$success) {
        output$reg_status <- renderText("✅ Account created")
      } else {
        output$reg_status <- renderText(paste("❌", res$error))
      }
    })
    
    return(user_data)
  })
}





