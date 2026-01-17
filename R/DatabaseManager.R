# ============================================================================
# database_manager.R - Database Operations & Schema Management
# ============================================================================

# ----------------------------------------------------------------------------
# DATABASE INITIALIZATION
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
  message("✓ Database initialized with all tables")
  return(TRUE)
}

# ----------------------------------------------------------------------------
# DATABASE BACKUP
# ----------------------------------------------------------------------------
backup_database <- function(db_path = "lottery_users.db") {
  if (!file.exists(db_path)) return(FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- paste0("backups/lottery_users_", timestamp, ".db")
  
  dir.create("backups", showWarnings = FALSE)
  file.copy(db_path, backup_path)
  
  # Keep only last 7 backups
  backups <- list.files("backups", pattern = "^lottery_users_.*\.db$", full.names = TRUE)
  if (length(backups) > 7) {
    old_backups <- sort(backups)[1:(length(backups) - 7)]
    file.remove(old_backups)
  }
  
  message("✓ Database backed up to: ", backup_path)
  return(TRUE)
}

# ----------------------------------------------------------------------------
# USER QUERIES
# ----------------------------------------------------------------------------
db_get_user <- function(user_id = NULL, username = NULL, email = NULL, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  if (!is.null(user_id)) {
    query <- "SELECT * FROM users WHERE user_id = ?"
    params <- list(user_id)
  } else if (!is.null(username)) {
    query <- "SELECT * FROM users WHERE username = ?"
    params <- list(username)
  } else if (!is.null(email)) {
    query <- "SELECT * FROM users WHERE email = ?"
    params <- list(email)
  } else {
    return(NULL)
  }
  
  result <- dbGetQuery(con, query, params = params)
  if (nrow(result) == 0) return(NULL)
  as.list(result[1,])
}

db_create_user <- function(username, email, password_hash, verification_token, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO users (username, email, password_hash, verification_token)
      VALUES (?, ?, ?, ?)
    ", params = list(username, email, password_hash, verification_token))
    
    user_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    list(success = TRUE, user_id = user_id)
  }, error = function(e) {
    if (grepl("UNIQUE constraint failed: users.username", e$message)) {
      return(list(success = FALSE, error = "Username already exists"))
    } else if (grepl("UNIQUE constraint failed: users.email", e$message)) {
      return(list(success = FALSE, error = "Email already registered"))
    }
    list(success = FALSE, error = e$message)
  })
}

db_update_user_login <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "
    UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE user_id = ?
  ", params = list(user_id))
}

db_verify_email <- function(token, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  user <- dbGetQuery(con, "
    SELECT user_id FROM users WHERE verification_token = ? AND email_verified = 0
  ", params = list(token))
  
  if (nrow(user) == 0) return(NULL)
  
  dbExecute(con, "
    UPDATE users SET email_verified = 1, verification_token = NULL WHERE user_id = ?
  ", params = list(user$user_id))
  
  return(user$user_id)
}

db_create_reset_token <- function(email, reset_token, expiry, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  user <- dbGetQuery(con, "SELECT user_id FROM users WHERE email = ?", params = list(email))
  if (nrow(user) == 0) return(NULL)
  
  dbExecute(con, "
    UPDATE users SET reset_token = ?, reset_token_expiry = ? WHERE user_id = ?
  ", params = list(reset_token, expiry, user$user_id))
  
  return(user$user_id)
}

db_reset_password <- function(token, password_hash, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  user <- dbGetQuery(con, "
    SELECT user_id FROM users 
    WHERE reset_token = ? AND reset_token_expiry > CURRENT_TIMESTAMP
  ", params = list(token))
  
  if (nrow(user) == 0) return(NULL)
  
  dbExecute(con, "
    UPDATE users SET password_hash = ?, reset_token = NULL, reset_token_expiry = NULL 
    WHERE user_id = ?
  ", params = list(password_hash, user$user_id))
  
  return(user$user_id)
}

# ----------------------------------------------------------------------------
# SUBSCRIPTION QUERIES
# ----------------------------------------------------------------------------
db_get_subscription <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  result <- dbGetQuery(con, "
    SELECT plan_type, status, start_date, end_date, auto_renew, 
           stripe_subscription_id, stripe_customer_id
    FROM subscriptions
    WHERE user_id = ? AND status = 'active' 
    ORDER BY subscription_id DESC LIMIT 1
  ", params = list(user_id))
  
  if (nrow(result) == 0) {
    return(list(plan_type = "free", status = "active"))
  }
  as.list(result[1,])
}

db_create_subscription <- function(user_id, plan_type = "free", stripe_customer_id = NULL, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "
    INSERT INTO subscriptions (user_id, plan_type, status, stripe_customer_id)
    VALUES (?, ?, 'active', ?)
  ", params = list(user_id, plan_type, stripe_customer_id))
}

db_update_subscription <- function(user_id, plan_type, stripe_subscription_id = NULL, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Deactivate old subscriptions
  dbExecute(con, "
    UPDATE subscriptions SET status = 'cancelled' 
    WHERE user_id = ? AND status = 'active'
  ", params = list(user_id))
  
  # Create new subscription
  dbExecute(con, "
    INSERT INTO subscriptions (user_id, plan_type, status, stripe_subscription_id)
    VALUES (?, ?, 'active', ?)
  ", params = list(user_id, plan_type, stripe_subscription_id))
  
  return(TRUE)
}

db_update_stripe_customer <- function(user_id, customer_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "
    UPDATE subscriptions 
    SET stripe_customer_id = ? 
    WHERE user_id = ? AND status = 'active'
  ", params = list(customer_id, user_id))
}

# ----------------------------------------------------------------------------
# USAGE LOGS
# ----------------------------------------------------------------------------
db_log_action <- function(user_id, action, details = "", db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "
    INSERT INTO usage_logs (user_id, action, details)
    VALUES (?, ?, ?)
  ", params = list(user_id, action, details))
}

db_get_daily_usage <- function(user_id, action = "api_call", db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  result <- dbGetQuery(con, "
    SELECT COUNT(*) as count FROM usage_logs
    WHERE user_id = ? AND action = ? AND DATE(timestamp) = DATE('now')
  ", params = list(user_id, action))
  
  return(result$count)
}
