---
editor_options: 
  markdown: 
    wrap: 72
---

# 🎲 Complete Authentication & Subscription System Setup Guide

## 📁 File Structure

Your project should now have these new files:

```         
your-project/
├── app.R                              # ✅ UPDATED - Main app with auth integration
├── AuthenticationUI.R                 # ✅ NEW - Separated auth UI components
├── EnhancedAuthenticationSystem.R     # ✅ NEW - Complete backend logic
├── EmailVerificationSystem.R          # ✅ NEW - Email & password reset
├── lottery_users.db                   # ✅ AUTO-CREATED - SQLite database
├── .env                               # ✅ CREATE THIS - Environment variables
└── (your existing files...)
```

------------------------------------------------------------------------

## 🚀 Step 1: Install Required R Packages

Run this in your R console:

``` r
install.packages(c(
  "DBI",
  "RSQLite", 
  "sodium",
  "httr",
  "mailR",
  "jose"
))
```

------------------------------------------------------------------------

## 🔐 Step 2: Create `.env` File (IMPORTANT!)

Create a file named `.env` in your project root:

``` bash
# Stripe Configuration (for payments)
STRIPE_SECRET_KEY=sk_test_your_stripe_secret_key_here
STRIPE_PUBLISHABLE_KEY=pk_test_your_stripe_publishable_key_here

# Email Configuration (for verification & password reset)
SMTP_SERVER=smtp.gmail.com
SMTP_PORT=587
SENDER_EMAIL=your-email@gmail.com
SENDER_PASSWORD=your-app-specific-password
APP_URL=https://lotteryinsights.dpdns.org

# Optional: Session Configuration
SESSION_TIMEOUT=3600
MAX_REQUEST_SIZE=10
```

### 📧 Gmail Setup (for emails):

1.  **Enable 2-Factor Authentication** on your Gmail account
2.  **Generate App Password**:
    -   Go to Google Account → Security
    -   Under "2-Step Verification", click "App passwords"
    -   Generate a password for "Mail" on "Other device"
    -   Copy the 16-character password to `SENDER_PASSWORD`

------------------------------------------------------------------------

## 💳 Step 3: Get Stripe API Keys (for subscriptions)

1.  **Sign up** at [stripe.com](https://stripe.com)
2.  **Get Test Keys**:
    -   Go to Developers → API keys
    -   Copy **Secret key** (starts with `sk_test_...`)
    -   Copy **Publishable key** (starts with `pk_test_...`)
3.  **Add to `.env`** file

### Stripe Webhook (Optional - for production):

``` bash
# In Stripe Dashboard:
# Developers → Webhooks → Add endpoint
# URL: https://your-domain.com/stripe-webhook
# Events: checkout.session.completed, customer.subscription.updated
```

------------------------------------------------------------------------

## 🗄️ Step 4: Initialize Database

The database will auto-create on first run, but you can manually
initialize:

``` r
library(shiny)
source("EnhancedAuthenticationSystem.R")
init_database()
```

This creates `lottery_users.db` with tables: - `users` - User accounts -
`subscriptions` - Subscription data - `usage_logs` - Activity tracking

------------------------------------------------------------------------

## 🧪 Step 5: Test the System

### Test Registration:

``` r
# In R console:
source("EnhancedAuthenticationSystem.R")

# Create a test user
result <- register_user(
  username = "testuser",
  email = "test@example.com", 
  password = "securepassword123"
)

print(result)
# Should return: list(success = TRUE, user_id = 1, verification_token = "...")
```

### Test Login:

``` r
result <- verify_user("testuser", "securepassword123")
print(result)
# Should return: list(success = TRUE, user_id = 1, email_verified = FALSE)
```

### Test Subscription Check:

``` r
sub <- get_user_subscription(user_id = 1)
print(sub)
# Should return: list(plan_type = "free", status = "active")
```

------------------------------------------------------------------------

## 🎨 Step 6: Customize Subscription Plans

Edit `EnhancedAuthenticationSystem.R`, find `subscription_plans` list:

``` r
subscription_plans <- list(
  free = list(
    name = "Free",
    price = 0,
    currency = "EUR",
    interval = "month",
    features = c(
      "Basic number analysis",
      "Limited historical data",
      # Add/remove features here
    ),
    limits = list(
      api_calls_per_day = 100,
      exports_per_month = 5
    )
  ),
  # ... modify basic and premium similarly
)
```

------------------------------------------------------------------------

## 🔒 Step 7: Security Hardening (Production)

### A. Environment Variables:

``` bash
# Load .env file automatically (add to app.R top):
if (file.exists(".env")) {
  readRenviron(".env")
}
```

### B. Password Requirements:

Currently enforces: - ✅ Minimum 8 characters - ⚠️ Add complexity
requirements if needed:

``` r
# In register_user(), add:
if (!grepl("[A-Z]", password)) {
  return(list(success = FALSE, error = "Password must contain uppercase"))
}
if (!grepl("[0-9]", password)) {
  return(list(success = FALSE, error = "Password must contain number"))
}
```

### C. Rate Limiting:

Already included! Check `check_rate_limit()` function.

### D. SQL Injection Protection:

✅ All queries use parameterized statements (safe by default)

------------------------------------------------------------------------

## 📧 Step 8: Email Verification Flow

### How it works:

1.  **User registers** → System generates `verification_token`
2.  **Email sent** with verification link: `?verify=TOKEN`
3.  **User clicks link** → `handle_email_verification(token)` runs
4.  **Email verified** → Welcome email sent

### Testing without email:

``` r
# Manually verify a user:
verify_email_token(token = "their_verification_token")
```

### Integration in app.R:

``` r
# Add this to your server function:
observe({
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query$verify)) {
    result <- handle_email_verification(query$verify)
    if (result$success) {
      showNotification("✅ Email verified!", type = "message")
    } else {
      showNotification("❌ Invalid verification link", type = "error")
    }
  }
})
```

------------------------------------------------------------------------

## 🔄 Step 9: Password Reset Flow

### How it works:

1.  **User clicks "Forgot Password"** (you need to add this button)
2.  **Enters email** → `request_password_reset(email)`
3.  **Email sent** with reset link: `?reset=TOKEN`
4.  **User clicks link** → Shows `password_reset_ui()`
5.  **Enters new password** → `reset_password(token, new_pass)`

### Add to your app:

``` r
# In UI (add to auth_ui):
div(style = "text-align: center; margin-top: 15px;",
  actionLink(ns("forgot_password"), "Forgot Password?")
)

# In server:
observeEvent(input$forgot_password, {
  showModal(modalDialog(
    title = "Reset Password",
    textInput(session$ns("reset_email"), "Enter your email"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(session$ns("send_reset"), "Send Reset Link")
    )
  ))
})

observeEvent(input$send_reset, {
  result <- request_password_reset(input$reset_email)
  showNotification(result$message)
  removeModal()
})
```

------------------------------------------------------------------------

## 📊 Step 10: Usage Analytics

Track user actions:

``` r
# Automatically logged:
- registration
- login
- email_verified
- password_reset
- subscription_upgraded
- dashboard_view

# View logs:
library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "lottery_users.db")
logs <- dbGetQuery(con, "SELECT * FROM usage_logs ORDER BY timestamp DESC LIMIT 100")
dbDisconnect(con)
print(logs)
```

------------------------------------------------------------------------

## 🐛 Troubleshooting

### Issue: Subscription buttons not showing

**Cause:** User not logged in or `user_info()` is NULL

**Fix:** Check console for errors, ensure `auth_server()` is running

### Issue: "Stripe not configured" error

**Cause:** Missing `STRIPE_SECRET_KEY` in environment

**Fix:**

``` r
# Check if set:
Sys.getenv("STRIPE_SECRET_KEY")
# Should NOT be empty
```

### Issue: Emails not sending

**Cause:** Wrong SMTP credentials or Gmail blocking

**Fix:** 1. Enable "Less secure app access" (not recommended) 2. Use App
Password (recommended) 3. Check `validate_email_config()` returns TRUE

### Issue: Database locked

**Cause:** Multiple connections open

**Fix:**

``` r
# Always use on.exit(dbDisconnect(con))
# Already implemented in all functions
```

------------------------------------------------------------------------

## 🎯 What's Working Now

✅ **User Registration** with validation\
✅ **Secure Login** with bcrypt hashing\
✅ **Subscription Management** (Free/Basic/Premium)\
✅ **Stripe Checkout** integration\
✅ **Email Verification** system\
✅ **Password Reset** flow\
✅ **Rate Limiting** per plan\
✅ **Usage Tracking** & analytics\
✅ **Professional UI** with responsive design

------------------------------------------------------------------------

## 🚀 Next Steps (Optional Enhancements)

### 1. **Social Login** (OAuth)

``` r
# Add Google/Facebook login
library(googleAuthR)
library(httr)
```

### 2. **Two-Factor Authentication**

``` r
# Add TOTP 2FA
library(oath)
```

### 3. **Admin Dashboard**

``` r
# View all users, subscriptions, logs
# Approve/reject accounts
```

### 4. **Payment History**

``` r
# Track all transactions
# Generate invoices
```

### 5. **Notification System**

``` r
# In-app notifications
# Email digests
```

------------------------------------------------------------------------

## 📝 Testing Checklist

Before going live:

-   [ ] Test user registration
-   [ ] Test login with correct/wrong password
-   [ ] Test email verification link
-   [ ] Test password reset flow
-   [ ] Test subscription upgrade (Stripe test mode)
-   [ ] Test rate limiting
-   [ ] Test logout functionality
-   [ ] Test concurrent sessions
-   [ ] Test SQL injection attempts (should fail safely)
-   [ ] Test XSS attempts (should be sanitized)
-   [ ] Load test with 10+ concurrent users
-   [ ] Check database backup strategy

------------------------------------------------------------------------

## 🆘 Support

If you encounter issues:

1.  **Check console logs** for error messages
2.  **Verify .env file** is properly configured
3.  **Test database** connection manually
4.  **Check Stripe dashboard** for webhook logs
5.  **Review email server** logs in Gmail

------------------------------------------------------------------------

## 📄 License & Disclaimer

This is an **educational platform**. Always: - ✅ Comply with gambling
regulations in your region - ✅ Add proper disclaimers about
randomness - ✅ Implement responsible gambling features - ✅ Secure user
data (GDPR compliance if EU) - ✅ Get legal review before accepting
payments

------------------------------------------------------------------------

**Ready to launch? 🚀**

Run your app:

``` r
shiny::runApp()
```

Your users can now: 1. Register → Verify email → Login 2. View
subscription plans 3. Upgrade with Stripe 4. Access dashboard features
based on their plan!
