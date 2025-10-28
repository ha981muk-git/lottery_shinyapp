---
editor_options: 
  markdown: 
    wrap: 72
---

# 🚀 Complete Authentication & Payment System Setup Guide

## 📋 Table of Contents

1.  [Prerequisites](#prerequisites)
2.  [Installation](#installation)
3.  [Stripe Setup](#stripe-setup)
4.  [Database Configuration](#database-configuration)
5.  [Security Hardening](#security-hardening)
6.  [Deployment](#deployment)
7.  [Testing](#testing)
8.  [Monitoring](#monitoring)

------------------------------------------------------------------------

## 1. Prerequisites {#prerequisites}

### Required R Packages

``` r
install.packages(c(
  "shiny",
  "shinymanager",  # Authentication
  "DBI",           # Database interface
  "RSQLite",       # SQLite database
  "sodium",        # Password hashing
  "httr",          # HTTP requests for Stripe
  "jose",          # JWT tokens
  "pool",          # Database connection pooling
  "config",        # Configuration management
  "logger"         # Logging
))
```

### System Requirements

-   R \>= 4.0.0
-   SQLite3
-   SSL certificate (for production)
-   SMTP server (for email verification)

------------------------------------------------------------------------

## 2. Installation {#installation}

### Step 1: File Structure

```         
lottery-app/
├── app.R                    # Main app file
├── auth_system.R            # Authentication system
├── translations.R           # Existing translations
├── PrepareData.R           # Existing data prep
├── DashboardModule.R       # Existing dashboard
├── dashboard/              # Metric files
├── config.yml              # Configuration file
├── .env                    # Environment variables (DO NOT COMMIT)
├── www/
│   └── Home.css           # Existing styles
├── logs/                   # Application logs
└── backups/               # Database backups
```

### Step 2: Create Configuration File

**config.yml:**

``` yaml
default:
  app_name: "6/49 Lottery Analysis"
  db_path: "lottery_users.db"
  session_timeout: 3600
  max_file_size: 10485760  # 10MB
  
  rate_limits:
    free:
      daily: 10
      concurrent: 1
    basic:
      daily: 100
      concurrent: 2
    premium:
      daily: 1000
      concurrent: 5
    enterprise:
      daily: 10000
      concurrent: 10

development:
  debug: true
  stripe_mode: "test"
  log_level: "DEBUG"

production:
  debug: false
  stripe_mode: "live"
  log_level: "INFO"
  force_ssl: true
```

### Step 3: Environment Variables

**Create .env file:**

``` bash
# Stripe Keys (get from https://dashboard.stripe.com/apikeys)
STRIPE_SECRET_KEY=sk_test_your_test_key_here
STRIPE_PUBLISHABLE_KEY=pk_test_your_test_key_here
STRIPE_WEBHOOK_SECRET=whsec_your_webhook_secret_here

# Database Encryption
DB_PASSPHRASE=your_very_secure_passphrase_here_min_32_chars

# Email Configuration (for SendGrid, Mailgun, etc.)
EMAIL_SERVICE=sendgrid
EMAIL_API_KEY=your_email_api_key
EMAIL_FROM=noreply@yourdomain.com

# Session Secret
SESSION_SECRET=your_random_session_secret_min_32_chars

# Admin Credentials (initial setup)
ADMIN_USERNAME=admin
ADMIN_PASSWORD=change_this_immediately_after_first_login
ADMIN_EMAIL=admin@yourdomain.com

# App URL
APP_URL=https://yourdomain.com
```

**Load environment variables in R:**

``` r
# Add to beginning of app.R
if (file.exists(".env")) {
  readRenviron(".env")
}
```

------------------------------------------------------------------------

## 3. Stripe Setup {#stripe-setup}

### Step 1: Create Stripe Account

1.  Go to <https://stripe.com>
2.  Sign up for account
3.  Verify your business details

### Step 2: Create Products & Prices

**Via Stripe Dashboard:** 1. Go to Products → Add Product 2. Create
products: - **Basic Plan**: €9.99/month - **Premium Plan**:
€24.99/month - **Enterprise Plan**: €99.99/month

3.  Copy Price IDs (e.g., `price_1xxxxx`)

**Via Stripe API (R code):**

``` r
library(httr)

create_stripe_products <- function() {
  STRIPE_SECRET_KEY <- Sys.getenv("STRIPE_SECRET_KEY")
  
  plans <- list(
    list(name = "Basic Plan", price = 999, interval = "month"),
    list(name = "Premium Plan", price = 2499, interval = "month"),
    list(name = "Enterprise Plan", price = 9999, interval = "month")
  )
  
  for (plan in plans) {
    # Create product
    product_response <- POST(
      "https://api.stripe.com/v1/products",
      add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)),
      body = list(name = plan$name),
      encode = "form"
    )
    
    product_id <- content(product_response)$id
    
    # Create price
    price_response <- POST(
      "https://api.stripe.com/v1/prices",
      add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)),
      body = list(
        product = product_id,
        unit_amount = plan$price,
        currency = "eur",
        recurring = list(interval = plan$interval)
      ),
      encode = "form"
    )
    
    price_id <- content(price_response)$id
    message(paste("Created:", plan$name, "- Price ID:", price_id))
  }
}
```

### Step 3: Update Subscription Plans

**Add Price IDs to auth_system.R:**

``` r
subscription_plans <- list(
  basic = list(
    name = "Basic",
    price = 9.99,
    stripe_price_id = "price_1xxxxxxxxxxxxx",  # Add this
    # ... rest of config
  ),
  premium = list(
    name = "Premium",
    price = 24.99,
    stripe_price_id = "price_1yyyyyyyyyyyyy",  # Add this
    # ... rest of config
  )
)
```

### Step 4: Setup Webhook

**Create webhook endpoint in your app:**

Add to **auth_system.R:**

``` r
# Stripe webhook handler
handle_stripe_webhook <- function(payload, signature) {
  STRIPE_WEBHOOK_SECRET <- Sys.getenv("STRIPE_WEBHOOK_SECRET")
  
  # Verify webhook signature
  tryCatch({
    event <- stripe_verify_webhook(payload, signature, STRIPE_WEBHOOK_SECRET)
    
    event_type <- event$type
    
    switch(event_type,
      "checkout.session.completed" = {
        handle_checkout_completed(event$data$object)
      },
      "customer.subscription.updated" = {
        handle_subscription_updated(event$data$object)
      },
      "customer.subscription.deleted" = {
        handle_subscription_cancelled(event$data$object)
      },
      "invoice.payment_succeeded" = {
        handle_payment_succeeded(event$data$object)
      },
      "invoice.payment_failed" = {
        handle_payment_failed(event$data$object)
      }
    )
    
    return(list(success = TRUE))
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}
```

**Register webhook in Stripe Dashboard:** 1. Go to Developers → Webhooks
2. Add endpoint: `https://yourdomain.com/webhook/stripe` 3. Select
events: - `checkout.session.completed` -
`customer.subscription.updated` - `customer.subscription.deleted` -
`invoice.payment_succeeded` - `invoice.payment_failed`

4.  Copy webhook signing secret to `.env`

------------------------------------------------------------------------

## 4. Database Configuration {#database-configuration}

### Enhanced Database Schema

**Add these tables to init_database():**

``` r
init_database <- function(db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  
  # Enable foreign keys
  dbExecute(con, "PRAGMA foreign_keys = ON")
  
  # Users table with more fields
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      email TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      first_name TEXT,
      last_name TEXT,
      phone TEXT,
      country TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_login TIMESTAMP,
      is_active INTEGER DEFAULT 1,
      email_verified INTEGER DEFAULT 0,
      email_verification_token TEXT,
      password_reset_token TEXT,
      password_reset_expires TIMESTAMP,
      failed_login_attempts INTEGER DEFAULT 0,
      locked_until TIMESTAMP
    )
  ")
  
  # Subscriptions table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS subscriptions (
      subscription_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      plan_type TEXT NOT NULL,
      status TEXT NOT NULL,
      stripe_subscription_id TEXT UNIQUE,
      stripe_customer_id TEXT,
      stripe_price_id TEXT,
      start_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      end_date TIMESTAMP,
      current_period_start TIMESTAMP,
      current_period_end TIMESTAMP,
      cancel_at_period_end INTEGER DEFAULT 0,
      cancelled_at TIMESTAMP,
      auto_renew INTEGER DEFAULT 1,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Payment history
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS payments (
      payment_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      subscription_id INTEGER,
      amount REAL NOT NULL,
      currency TEXT DEFAULT 'EUR',
      stripe_payment_id TEXT UNIQUE,
      stripe_invoice_id TEXT,
      status TEXT NOT NULL,
      payment_method TEXT,
      payment_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      metadata TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
      FOREIGN KEY (subscription_id) REFERENCES subscriptions(subscription_id)
    )
  ")
  
  # Usage logs (with IP tracking)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS usage_logs (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      action TEXT NOT NULL,
      details TEXT,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      ip_address TEXT,
      user_agent TEXT,
      session_id TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # API keys (for enterprise users)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS api_keys (
      key_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      api_key TEXT UNIQUE NOT NULL,
      key_name TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_used TIMESTAMP,
      expires_at TIMESTAMP,
      is_active INTEGER DEFAULT 1,
      rate_limit INTEGER,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Email verification queue
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS email_queue (
      queue_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      email_type TEXT NOT NULL,
      recipient TEXT NOT NULL,
      subject TEXT,
      body TEXT,
      status TEXT DEFAULT 'pending',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      sent_at TIMESTAMP,
      error TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Create indexes for performance
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_users_email ON users(email)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_subscriptions_user ON subscriptions(user_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_usage_logs_user ON usage_logs(user_id, timestamp)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_payments_user ON payments(user_id)")
  
  dbDisconnect(con)
  message("✓ Database initialized successfully")
}
```

### Database Connection Pooling

**For better performance:**

``` r
library(pool)

# Create connection pool
db_pool <- dbPool(
  drv = SQLite(),
  dbname = "lottery_users.db",
  minSize = 1,
  maxSize = 10
)

# Use pooled connections
get_user <- function(user_id) {
  conn <- poolCheckout(db_pool)
  on.exit(poolReturn(conn))
  
  dbGetQuery(conn, "SELECT * FROM users WHERE user_id = ?", 
             params = list(user_id))
}

# Close pool on app shutdown
onStop(function() {
  poolClose(db_pool)
})
```

------------------------------------------------------------------------

## 5. Security Hardening {#security-hardening}

### Password Security

**Enhanced password hashing:**

``` r
library(sodium)

# Hash password with strong parameters
hash_password <- function(password) {
  # Minimum requirements check
  if (nchar(password) < 8) {
    stop("Password must be at least 8 characters")
  }
  if (!grepl("[A-Z]", password)) {
    stop("Password must contain uppercase letter")
  }
  if (!grepl("[0-9]", password)) {
    stop("Password must contain number")
  }
  if (!grepl("[^A-Za-z0-9]", password)) {
    stop("Password must contain special character")
  }
  
  # Use sodium for secure hashing
  password_store(password)
}

# Verify password
verify_password <- function(hash, password) {
  password_verify(hash, password)
}
```

### Account Lockout (Brute Force Protection)

**Add to auth_system.R:**

``` r
check_account_locked <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT locked_until, failed_login_attempts
    FROM users
    WHERE user_id = ?
  ", params = list(user_id))
  
  if (nrow(user) == 0) return(list(locked = FALSE))
  
  # Check if account is locked
  if (!is.na(user$locked_until)) {
    locked_until <- as.POSIXct(user$locked_until)
    if (Sys.time() < locked_until) {
      remaining <- difftime(locked_until, Sys.time(), units = "mins")
      return(list(
        locked = TRUE,
        message = paste("Account locked. Try again in", 
                       round(remaining), "minutes")
      ))
    } else {
      # Unlock account
      dbExecute(con, "
        UPDATE users
        SET locked_until = NULL, failed_login_attempts = 0
        WHERE user_id = ?
      ", params = list(user_id))
    }
  }
  
  return(list(locked = FALSE))
}

handle_failed_login <- function(user_id, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "
    UPDATE users
    SET failed_login_attempts = failed_login_attempts + 1
    WHERE user_id = ?
  ", params = list(user_id))
  
  attempts <- dbGetQuery(con, "
    SELECT failed_login_attempts FROM users WHERE user_id = ?
  ", params = list(user_id))$failed_login_attempts
  
  # Lock account after 5 failed attempts
  if (attempts >= 5) {
    lock_until <- Sys.time() + (30 * 60)  # Lock for 30 minutes
    dbExecute(con, "
      UPDATE users
      SET locked_until = ?
      WHERE user_id = ?
    ", params = list(format(lock_until), user_id))
    
    return(list(locked = TRUE, attempts = attempts))
  }
  
  return(list(locked = FALSE, attempts = attempts))
}
```

### SQL Injection Prevention

**Always use parameterized queries:**

``` r
# ✅ GOOD - Parameterized
user <- dbGetQuery(con, "
  SELECT * FROM users WHERE username = ?
", params = list(username))

# ❌ BAD - SQL Injection vulnerable
user <- dbGetQuery(con, paste0("
  SELECT * FROM users WHERE username = '", username, "'
"))
```

### XSS Protection

**Sanitize user inputs:**

``` r
library(htmltools)

sanitize_input <- function(text) {
  # Remove HTML tags
  text <- gsub("<[^>]*>", "", text)
  # Escape special characters
  htmlEscape(text)
}
```

### CSRF Protection

**Add CSRF tokens:**

``` r
# Generate token
generate_csrf_token <- function(session) {
  token <- sodium::random(32) %>% sodium::bin2hex()
  session$userData$csrf_token <- token
  token
}

# Verify token
verify_csrf_token <- function(session, token) {
  identical(session$userData$csrf_token, token)
}
```

------------------------------------------------------------------------

## 6. Deployment {#deployment}

### Option A: DigitalOcean App Platform

**1. Create app.yaml:**

``` yaml
name: lottery-analysis-platform
region: fra
services:
  - name: web
    github:
      repo: yourusername/lottery-app
      branch: main
      deploy_on_push: true
    build_command: |
      Rscript -e "install.packages(c('shiny', 'shinymanager', 'DBI', 'RSQLite', 'sodium', 'httr', 'jose', 'pool'))"
    run_command: R -e "shiny::runApp(port=8080, host='0.0.0.0')"
    envs:
      - key: STRIPE_SECRET_KEY
        scope: RUN_TIME
        type: SECRET
      - key: DB_PASSPHRASE
        scope: RUN_TIME
        type: SECRET
    health_check:
      http_path: /?health=check
    instance_count: 1
    instance_size_slug: professional-xs
    
databases:
  - name: lottery-db
    engine: SQLITE
    production: true
```

**2. Deploy:**

``` bash
# Install doctl CLI
brew install doctl  # macOS
# or download from: https://docs.digitalocean.com/reference/doctl/

# Authenticate
doctl auth init

# Create app
doctl apps create --spec app.yaml

# Update environment variables
doctl apps update YOUR_APP_ID --env STRIPE_SECRET_KEY=sk_live_xxx
```

### Option B: ShinyProxy (Docker)

**1. Create Dockerfile:**

``` dockerfile
FROM rocker/shiny:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinymanager', 'DBI', 'RSQLite', \
    'sodium', 'httr', 'jose', 'pool', 'config', 'logger', \
    'vroom', 'dplyr', 'janitor', 'bslib', 'shinyjs', \
    'plotly', 'waiter', 'tidyr', 'purrr', 'DT'))"

# Copy app files
COPY app.R /srv/shiny-server/
COPY auth_system.R /srv/shiny-server/
COPY translations.R /srv/shiny-server/
COPY PrepareData.R /srv/shiny-server/
COPY DashboardModule.R /srv/shiny-server/
COPY config.yml /srv/shiny-server/
COPY dashboard/ /srv/shiny-server/dashboard/
COPY www/ /srv/shiny-server/www/

# Create directories
RUN mkdir -p /srv/shiny-server/logs /srv/shiny-server/backups

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=3838, host='0.0.0.0')"]
```

**2. Build and run:**

``` bash
# Build image
docker build -t lottery-app .

# Run container
docker run -d \
  -p 3838:3838 \
  -e STRIPE_SECRET_KEY=sk_live_xxx \
  -e DB_PASSPHRASE=xxx \
  -v $(pwd)/data:/srv/shiny-server/data \
  --name lottery-app \
  lottery-app
```

**3. Docker Compose (with nginx):**

``` yaml
version: '3.8'

services:
  shiny-app:
    build: .
    env_file: .env
    volumes:
      - ./data:/srv/shiny-server/data
      - ./logs:/srv/shiny-server/logs
    restart: unless-stopped
    networks:
      - app-network
  
  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - shiny-app
    networks:
      - app-network

networks:
  app-network:
    driver: bridge
```

### Option C: Posit Connect (Formerly RStudio Connect)

**1. Install Posit Connect** **2. Deploy via rsconnect:**

``` r
library(rsconnect)

# Configure account
rsconnect::setAccountInfo(
  name = "your-server",
  server = "https://connect.yourserver.com",
  key = "your-api-key"
)

# Deploy
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app.R",
    "auth_system.R",
    "translations.R",
    "PrepareData.R",
    "DashboardModule.R",
    "config.yml",
    "dashboard/",
    "www/"
  ),
  appTitle = "Lottery Analysis Platform",
  contentCategory = "site"
)
```

------------------------------------------------------------------------

## 7. Testing {#testing}

### Unit Tests

**Create tests/test_auth.R:**

``` r
library(testthat)
library(DBI)
library(RSQLite)

test_that("User registration works", {
  # Setup test database
  test_db <- "test_users.db"
  init_database(test_db)
  
  # Test registration
  result <- register_user(
    username = "testuser",
    email = "test@example.com",
    password = "Test123!@#",
    db_path = test_db
  )
  
  expect_true(result$success)
  expect_true(!is.null(result$user_id))
  
  # Cleanup
  unlink(test_db)
})

test_that("Password verification works", {
  test_db <- "test_users.db"
  init_database(test_db)
  
  # Register user
  register_user("testuser", "test@example.com", "Test123!@#", test_db)
  
  # Test correct password
  result <- verify_user("testuser", "Test123!@#", test_db)
  expect_true(result$success)
  
  # Test wrong password
  result <- verify_user("testuser", "WrongPassword", test_db)
  expect_false(result$success)
  
  unlink(test_db)
})

test_that("Rate limiting works", {
  test_db <- "test_users.db"
  init_database(test_db)
  
  # Create user with free plan
  register_user("testuser", "test@example.com", "Test123!@#", test_db)
  
  # Log 10 analyses (free plan limit)
  for (i in 1:10) {
    log_usage(1, "analysis", db_path = test_db)
  }
  
  # Check rate limit
  status <- check_rate_limit(1, test_db)
  expect_false(status$allowed)
  
  unlink(test_db)
})
```

**Run tests:**

``` r
testthat::test_dir("tests/")
```

### Integration Tests

**Test Stripe integration:**

``` r
test_stripe_integration <- function() {
  # Use Stripe test mode
  Sys.setenv(STRIPE_SECRET_KEY = "sk_test_xxx")
  
  # Test checkout creation
  checkout <- create_stripe_checkout(
    user_id = 1,
    plan_type = "premium",
    success_url = "http://localhost/success",
    cancel_url = "http://localhost/cancel"
  )
  
  expect_true(checkout$success)
  expect_true(!is.null(checkout$checkout_url))
  
  message("✓ Stripe integration test passed")
}
```

### Load Testing

**Using shinyloadtest:**

``` r
library(shinyloadtest)

# Record session
shinyloadtest::record_session("http://localhost:3838")

# Run load test (50 concurrent users)
shinyloadtest::load_runs(
  app_url = "http://localhost:3838",
  recording = "recording.log",
  workers = 50,
  duration = 300  # 5 minutes
)
```

------------------------------------------------------------------------

## 8. Monitoring {#monitoring}

### Application Logging

**Enhanced logging system:**

``` r
library(logger)

# Configure logger
log_appender(appender_file("logs/app.log"))
log_layout(layout_glue_colors)
log_threshold(INFO)

# Log functions
log_user_action <- function(user_id, action, details = NULL) {
  log_info("User {user_id} performed {action}", 
           user_id = user_id, action = action)
  
  if (!is.null(details)) {
    log_debug("Details: {details}", details = details)
  }
}

log_error_with_context <- function(error, context) {
  log_error("Error in {context}: {error}", 
            context = context, error = error)
}

log_payment <- function(user_id, amount, status) {
  log_info("Payment: User {user_id}, Amount: €{amount}, Status: {status}",
           user_id = user_id, amount = amount, status = status)
}
```

### Performance Monitoring

**Track response times:**

``` r
track_performance <- function(func, name) {
  start_time <- Sys.time()
  result <- func()
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  log_info("Performance: {name} took {elapsed}s", 
           name = name, elapsed = round(elapsed, 2))
  
  # Alert if slow
  if (elapsed > 5) {
    log_warn("SLOW: {name} took {elapsed}s", 
             name = name, elapsed = round(elapsed, 2))
  }
  
  result
}

# Usage
filtered_data <- track_performance(
  function() get_filtered_data(weeks, range),
  "data_filtering"
)
```

### Health Check Endpoint

**Add to server:**

``` r
observe({
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query$health)) {
    # Check database
    db_ok <- tryCatch({
      con <- dbConnect(SQLite(), "lottery_users.db")
      dbDisconnect(con)
      TRUE
    }, error = function(e) FALSE)
    
    # Check Stripe
    stripe_ok <- tryCatch({
      httr::GET("https://api.stripe.com/v1/customers?limit=1",
                add_headers(Authorization = paste("Bearer", STRIPE_SECRET_KEY)))
      TRUE
    }, error = function(e) FALSE)
    
    status <- list(
      status = if (db_ok && stripe_ok) "healthy" else "unhealthy",
      database = db_ok,
      stripe = stripe_ok,
      timestamp = Sys.time()
    )
    
    session$sendCustomMessage("health_check", status)
  }
})
```

### Error Tracking with Sentry

``` r
library(httr)

send_to_sentry <- function(error, context = NULL) {
  SENTRY_DSN <- Sys.getenv("SENTRY_DSN")
  
  if (nchar(SENTRY_DSN) == 0) return()
  
  payload <- list(
    message = error$message,
    level = "error",
    platform = "r",
    server_name = Sys.info()["nodename"],
    timestamp = as.numeric(Sys.time()),
    extra = context
  )
  
  POST(
    SENTRY_DSN,
    body = toJSON(payload, auto_unbox = TRUE),
    content_type_json()
  )
}
```

------------------------------------------------------------------------

## 9. Backup & Recovery

### Automated Database Backups

``` r
backup_database <- function(db_path = "lottery_users.db") {
  backup_dir <- "backups"
  if (!dir.exists(backup_dir)) dir.create(backup_dir)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_dir, paste0("backup_", timestamp, ".db"))
  
  file.copy(db_path, backup_file)
  
  log_info("Database backed up to {backup_file}", backup_file = backup_file)
  
  # Keep only last 30 backups
  backups <- list.files(backup_dir, pattern = "^backup_.*\\.db$", full.names = TRUE)
  if (length(backups) > 30) {
    old_backups <- head(sort(backups), -30)
    file.remove(old_backups)
  }
}

# Schedule backups (run daily)
library(taskscheduleR)
taskscheduler_create(
  taskname = "lottery_backup",
  rscript = "backup_script.R",
  schedule = "DAILY",
  starttime = "03:00"
)
```

------------------------------------------------------------------------

## 10. Go-Live Checklist

### Pre-Launch

-   [ ] Change all default passwords
-   [ ] Switch Stripe to live mode
-   [ ] Setup SSL certificate
-   [ ] Configure email service
-   [ ] Test all payment flows
-   [ ] Run load tests
-   [ ] Setup monitoring
-   [ ] Configure backups
-   [ ] Review security settings
-   [ ] Test error handling
-   [ ] Prepare documentation

### Launch Day

-   [ ] Deploy to production
-   [ ] Verify health checks
-   [ ] Test user registration
-   [ ] Test subscription purchase
-   [ ] Monitor logs for errors
-   [ ] Check database connections
-   [ ] Verify webhook delivery

### Post-Launch

-   [ ] Monitor user feedback
-   [ ] Track error rates
-   [ ] Review performance metrics
-   [ ] Check payment success rates
-   [ ] Monitor resource usage
-   [ ] Plan scaling if needed

------------------------------------------------------------------------

## 11. Maintenance

### Weekly Tasks

-   Review error logs
-   Check failed payments
-   Monitor subscription cancellations
-   Review user feedback

### Monthly Tasks

-   Analyze usage patterns
-   Review pricing strategy
-   Update dependencies
-   Security audit
-   Database optimization

### Quarterly Tasks

-   Review and update features
-   A/B test pricing
-   User satisfaction survey
-   Compliance review (GDPR, etc.)

------------------------------------------------------------------------

## 12. Support Resources

### Documentation

-   **Stripe Docs**: <https://stripe.com/docs>
-   **shinymanager**: <https://github.com/datastorm-open/shinymanager>
-   **Shiny**: <https://shiny.rstudio.com>

### Community

-   **Stack Overflow**: Tag `[r] [shiny]`
-   **RStudio Community**: <https://community.rstudio.com>
-   **Stripe Discord**: <https://stripe.com/discord>

------------------------------------------------------------------------

## Troubleshooting

### Common Issues

**Issue: "Failed to connect to database"**

``` r
# Solution: Check permissions
file.info("lottery_users.db")$mode
# Should be readable/writable

# Fix permissions
Sys.chmod("lottery_users.db", mode = "0666")
```

**Issue: "Stripe webhook not receiving events"**

``` r
# Solution: Verify webhook signature
# Check logs for signature verification errors
# Ensure STRIPE_WEBHOOK_SECRET is correct
```

**Issue: "Session timeout too short"**

``` r
# Solution: Increase timeout in config
options(shiny.session.timeout = 7200)  # 2 hours
```

------------------------------------------------------------------------

This comprehensive guide provides everything needed to transform your
lottery app into
