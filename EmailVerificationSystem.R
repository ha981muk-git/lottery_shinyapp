# ============================================================================
# EmailVerificationSystem.R - Email Verification & Password Reset
# ============================================================================

library(mailR)
library(httr)

# ----------------------------------------------------------------------------
# 1. EMAIL CONFIGURATION
# ----------------------------------------------------------------------------
EMAIL_CONFIG <- list(
  smtp_server = Sys.getenv("SMTP_SERVER", "smtp.gmail.com"),
  smtp_port = as.integer(Sys.getenv("SMTP_PORT", "587")),
  sender_email = Sys.getenv("SENDER_EMAIL"),
  sender_password = Sys.getenv("SENDER_PASSWORD"),
  sender_name = "Lottery Insights",
  app_url = Sys.getenv("APP_URL", "https://lotteryinsights.dpdns.org")
)

# Validate configuration
validate_email_config <- function() {
  missing <- c()
  if (is.null(EMAIL_CONFIG$sender_email) || EMAIL_CONFIG$sender_email == "") {
    missing <- c(missing, "SENDER_EMAIL")
  }
  if (is.null(EMAIL_CONFIG$sender_password) || EMAIL_CONFIG$sender_password == "") {
    missing <- c(missing, "SENDER_PASSWORD")
  }
  
  if (length(missing) > 0) {
    warning(paste("⚠️ Email not configured. Missing:", paste(missing, collapse = ", ")))
    # Log to file in production
    if (file.exists("logs")) {
      cat(paste0("[", Sys.time(), "] Email config error: ", 
                 paste(missing, collapse = ", "), "\n"),
          file = "logs/email_errors.log", append = TRUE)
    }
    return(FALSE)
  }
  TRUE
}


# ----------------------------------------------------------------------------
# 2. EMAIL SENDING FUNCTION
# ----------------------------------------------------------------------------
send_email <- function(to, subject, body_html, body_text = NULL) {
  if (!validate_email_config()) {
    message("⚠ Email not configured. Token would be: ", 
            substr(body_html, 1, 100), "...")
    return(list(success = FALSE, error = "Email not configured"))
  }
  
  tryCatch({
    send.mail(
      from = paste0(EMAIL_CONFIG$sender_name, " <", EMAIL_CONFIG$sender_email, ">"),
      to = to,
      subject = subject,
      body = body_html,
      html = TRUE,
      smtp = list(
        host.name = EMAIL_CONFIG$smtp_server,
        port = EMAIL_CONFIG$smtp_port,
        user.name = EMAIL_CONFIG$sender_email,
        passwd = EMAIL_CONFIG$sender_password,
        ssl = TRUE
      ),
      authenticate = TRUE,
      send = TRUE
    )
    
    list(success = TRUE)
  }, error = function(e) {
    message("Email error: ", e$message)
    list(success = FALSE, error = e$message)
  })
}


# ----------------------------------------------------------------------------
# 3. EMAIL TEMPLATES
# ----------------------------------------------------------------------------
email_template <- function(title, content, cta_text = NULL, cta_url = NULL) {
  cta_html <- if (!is.null(cta_text) && !is.null(cta_url)) {
    sprintf('
      <div style="text-align: center; margin: 30px 0;">
        <a href="%s" style="
          display: inline-block;
          padding: 14px 32px;
          background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
          color: white;
          text-decoration: none;
          border-radius: 8px;
          font-weight: 600;
          font-size: 16px;
        ">%s</a>
      </div>
    ', cta_url, cta_text)
  } else ""
  
  sprintf('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body style="margin: 0; padding: 0; font-family: Arial, sans-serif; background: #f4f4f4;">
  <table width="100%%" cellpadding="0" cellspacing="0" style="background: #f4f4f4; padding: 40px 0;">
    <tr>
      <td align="center">
        <table width="600" cellpadding="0" cellspacing="0" style="background: white; border-radius: 12px; overflow: hidden; box-shadow: 0 4px 12px rgba(0,0,0,0.1);">
          
          <!-- Header -->
          <tr>
            <td style="background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%); padding: 30px; text-align: center;">
              <h1 style="margin: 0; color: white; font-size: 32px;">🎲 Lottery Insights</h1>
              <p style="margin: 10px 0 0 0; color: rgba(255,255,255,0.9); font-size: 14px;">Educational Lottery Analysis</p>
            </td>
          </tr>
          
          <!-- Content -->
          <tr>
            <td style="padding: 40px 30px;">
              <h2 style="color: #333; margin: 0 0 20px 0; font-size: 24px;">%s</h2>
              <div style="color: #666; line-height: 1.8; font-size: 16px;">
                %s
              </div>
              %s
            </td>
          </tr>
          
          <!-- Footer -->
          <tr>
            <td style="background: #f8f9fa; padding: 25px 30px; text-align: center; border-top: 1px solid #e0e0e0;">
              <p style="margin: 0 0 10px 0; color: #999; font-size: 13px;">
                This email was sent from Lottery Insights.<br>
                If you didn\'t request this, please ignore this email.
              </p>
              <p style="margin: 0; color: #999; font-size: 12px;">
                © %s Lottery Insights | Educational Platform Only
              </p>
            </td>
          </tr>
          
        </table>
      </td>
    </tr>
  </table>
</body>
</html>
  ', title, content, cta_html, format(Sys.Date(), "%Y"))
}


# ----------------------------------------------------------------------------
# 4. VERIFICATION EMAIL
# ----------------------------------------------------------------------------
send_verification_email <- function(email, username, token) {
  verification_url <- paste0(EMAIL_CONFIG$app_url, "?verify=", token)
  
  content <- sprintf('
    <p>Welcome to Lottery Insights, <strong>%s</strong>!</p>
    <p>Thank you for registering. To complete your account setup and start exploring lottery analytics, please verify your email address.</p>
    <p style="background: #f8f9fa; padding: 15px; border-left: 4px solid #667eea; margin: 20px 0;">
      <strong>Why verify?</strong> This helps us ensure account security and enables password recovery.
    </p>
    <p>Click the button below to verify your email:</p>
  ', username)
  
  html_body <- email_template(
    title = "Verify Your Email Address",
    content = content,
    cta_text = "Verify Email",
    cta_url = verification_url
  )
  
  send_email(
    to = email,
    subject = "🎲 Verify Your Lottery Insights Account",
    body_html = html_body
  )
}


# ----------------------------------------------------------------------------
# 5. PASSWORD RESET EMAIL
# ----------------------------------------------------------------------------
send_password_reset_email <- function(email, username, token) {
  reset_url <- paste0(EMAIL_CONFIG$app_url, "?reset=", token)
  
  content <- sprintf('
    <p>Hi <strong>%s</strong>,</p>
    <p>We received a request to reset your password. If you didn\'t make this request, you can safely ignore this email.</p>
    <p style="background: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0;">
      <strong>⏱ This link expires in 1 hour</strong> for security reasons.
    </p>
    <p>Click the button below to reset your password:</p>
  ', username)
  
  html_body <- email_template(
    title = "Reset Your Password",
    content = content,
    cta_text = "Reset Password",
    cta_url = reset_url
  )
  
  send_email(
    to = email,
    subject = "🔐 Password Reset Request - Lottery Insights",
    body_html = html_body
  )
}


# ----------------------------------------------------------------------------
# 6. WELCOME EMAIL (After Email Verification)
# ----------------------------------------------------------------------------
send_welcome_email <- function(email, username) {
  content <- sprintf('
    <p>Hi <strong>%s</strong>,</p>
    <p>🎉 Your email has been verified! Welcome to Lottery Insights.</p>
    <p><strong>What you can do now:</strong></p>
    <ul style="color: #666; line-height: 2;">
      <li>📊 Explore historical lottery data and patterns</li>
      <li>📈 Analyze number frequencies and trends</li>
      <li>🎓 Learn about probability and statistics</li>
      <li>💎 Upgrade for advanced features anytime</li>
    </ul>
    <p style="background: #e8f5e9; padding: 15px; border-left: 4px solid #10b981; margin: 20px 0;">
      <strong>📚 Remember:</strong> This is an educational platform. All analysis is for learning purposes only.
    </p>
  ', username)
  
  html_body <- email_template(
    title = "Welcome to Lottery Insights! 🎲",
    content = content,
    cta_text = "Start Exploring",
    cta_url = EMAIL_CONFIG$app_url
  )
  
  send_email(
    to = email,
    subject = "🎲 Welcome to Lottery Insights!",
    body_html = html_body
  )
}


# ----------------------------------------------------------------------------
# 7. SUBSCRIPTION CONFIRMATION EMAIL
# ----------------------------------------------------------------------------
send_subscription_confirmation <- function(email, username, plan_type, plan_details) {
  features <- paste0("<li>", plan_details$features, "</li>", collapse = "\n")
  
  content <- sprintf('
    <p>Hi <strong>%s</strong>,</p>
    <p>🎉 Thank you for subscribing to the <strong>%s</strong> plan!</p>
    <p><strong>Your plan includes:</strong></p>
    <ul style="color: #666; line-height: 2;">
      %s
    </ul>
    <p style="background: #e8f5e9; padding: 15px; border-left: 4px solid #10b981; margin: 20px 0;">
      <strong>Price:</strong> €%.2f/%s<br>
      <strong>Status:</strong> Active<br>
      <strong>Next billing:</strong> %s
    </p>
    <p>Manage your subscription anytime from your account dashboard.</p>
  ', 
                     username, 
                     plan_details$name,
                     features,
                     plan_details$price,
                     plan_details$interval,
                     format(Sys.Date() + 30, "%B %d, %Y")
  )
  
  html_body <- email_template(
    title = "Subscription Confirmed ✅",
    content = content,
    cta_text = "Go to Dashboard",
    cta_url = EMAIL_CONFIG$app_url
  )
  
  send_email(
    to = email,
    subject = sprintf("✅ %s Subscription Confirmed", plan_details$name),
    body_html = html_body
  )
}


# ----------------------------------------------------------------------------
# 8. PASSWORD RESET UI MODULE
# ----------------------------------------------------------------------------
password_reset_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .reset-container {
          max-width: 500px;
          margin: 60px auto;
          padding: 40px;
          background: white;
          border-radius: 16px;
          box-shadow: 0 20px 60px rgba(0,0,0,0.1);
        }
        .reset-header {
          text-align: center;
          margin-bottom: 30px;
        }
        .reset-header h2 {
          color: #333;
          margin-bottom: 10px;
        }
      "))
    ),
    
    div(class = "reset-container",
        div(class = "reset-header",
            h2("🔐 Reset Password"),
            p("Enter your new password below")
        ),
        
        passwordInput(ns("new_password"), "New Password", placeholder = "Min. 8 characters"),
        passwordInput(ns("confirm_password"), "Confirm Password", placeholder = "Re-enter password"),
        
        actionButton(ns("reset_btn"), "Reset Password", class = "auth-btn auth-btn-primary"),
        
        uiOutput(ns("reset_status"))
    )
  )
}

password_reset_server <- function(id, reset_token) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$reset_btn, {
      req(input$new_password, input$confirm_password)
      
      if (input$new_password != input$confirm_password) {
        output$reset_status <- renderUI({
          div(class = "auth-status error", "❌ Passwords don't match")
        })
        return()
      }
      
      if (nchar(input$new_password) < 8) {
        output$reset_status <- renderUI({
          div(class = "auth-status error", "❌ Password must be at least 8 characters")
        })
        return()
      }
      
      res <- reset_password(reset_token, input$new_password)
      
      if (res$success) {
        output$reset_status <- renderUI({
          div(class = "auth-status success", 
              "✅ Password reset successful! Redirecting to login...")
        })
        
        # Redirect to login after 2 seconds
        shinyjs::delay(2000, {
          session$sendCustomMessage("redirect", EMAIL_CONFIG$app_url)
        })
      } else {
        output$reset_status <- renderUI({
          div(class = "auth-status error", paste("❌", res$error))
        })
      }
    })
  })
}


# ----------------------------------------------------------------------------
# 9. EMAIL VERIFICATION HANDLER
# ----------------------------------------------------------------------------
handle_email_verification <- function(token) {
  res <- verify_email_token(token)
  
  if (res$success) {
    # Get user info to send welcome email
    user <- get_user_info_by_token(token)
    if (!is.null(user)) {
      send_welcome_email(user$email, user$username)
    }
  }
  
  res
}

# Helper function to get user info by verification token
get_user_info_by_token <- function(token, db_path = "lottery_users.db") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  user <- dbGetQuery(con, "
    SELECT username, email FROM users WHERE verification_token = ?
  ", params = list(token))
  
  if (nrow(user) == 0) return(NULL)
  as.list(user[1,])
}