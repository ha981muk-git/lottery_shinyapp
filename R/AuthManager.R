# ============================================================================
# auth_manager.R - Authentication & User Management
# ============================================================================



# ----------------------------------------------------------------------------
# USER REGISTRATION
# ----------------------------------------------------------------------------
register_user <- function(username, email, password, db_path = "lottery_users.db") {
  # Validation
  if (nchar(password) < 8) {
    return(list(success = FALSE, error = "Password must be at least 8 characters"))
  }
  
  if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email)) {
    return(list(success = FALSE, error = "Invalid email format"))
  }
  
  # Hash password
  password_hash <- sodium::password_store(password)
  
  # Generate verification token
  verification_token <- paste0(sample(c(0:9, letters), 32, replace = TRUE), collapse = "")
  
  # Create user in database
  result <- db_create_user(username, email, password_hash, verification_token, db_path)
  
  if (!result$success) {
    return(result)
  }
  
  # Create free subscription
  db_create_subscription(result$user_id, "free", NULL, db_path)
  
  # Log registration
  db_log_action(result$user_id, "registration", "New user registered", db_path)
  
  return(list(
    success = TRUE,
    user_id = result$user_id,
    verification_token = verification_token,
    message = "Account created! Please verify your email."
  ))
}

# ----------------------------------------------------------------------------
# USER LOGIN
# ----------------------------------------------------------------------------
verify_user <- function(username, password, csrf_token = NULL, db_path = "lottery_users.db") {
  # CSRF validation
  if (!is.null(csrf_token) && nchar(csrf_token) < 32) {
    return(list(success = FALSE, error = "Invalid security token"))
  }
  
  # Get user (works with username or email)
  user <- db_get_user(username = username, db_path = db_path)
  if (is.null(user)) {
    user <- db_get_user(email = username, db_path = db_path)
  }
  
  if (is.null(user)) {
    return(list(success = FALSE, error = "User not found"))
  }
  
  if (!user$is_active) {
    return(list(success = FALSE, error = "Account inactive"))
  }
  
  # Verify password
  if (sodium::password_verify(user$password_hash, password)) {
    db_update_user_login(user$user_id, db_path)
    db_log_action(user$user_id, "login", "User logged in", db_path)
    
    return(list(
      success = TRUE,
      user_id = user$user_id,
      email_verified = as.logical(user$email_verified)
    ))
  } else {
    return(list(success = FALSE, error = "Invalid password"))
  }
}

# ----------------------------------------------------------------------------
# GET USER INFO
# ----------------------------------------------------------------------------
get_user_info <- function(user_id, db_path = "lottery_users.db") {
  user <- db_get_user(user_id = user_id, db_path = db_path)
  
  if (is.null(user)) return(NULL)
  
  return(list(
    username = user$username,
    email = user$email,
    created_at = user$created_at,
    email_verified = as.logical(user$email_verified)
  ))
}

get_user_subscription <- function(user_id, db_path = "lottery_users.db") {
  return(db_get_subscription(user_id, db_path))
}

# ----------------------------------------------------------------------------
# EMAIL VERIFICATION
# ----------------------------------------------------------------------------
verify_email_token <- function(token, db_path = "lottery_users.db") {
  user_id <- db_verify_email(token, db_path)
  
  if (is.null(user_id)) {
    return(list(success = FALSE, error = "Invalid or expired token"))
  }
  
  db_log_action(user_id, "email_verified", "Email verified", db_path)
  
  return(list(success = TRUE, message = "Email verified successfully!"))
}

# ----------------------------------------------------------------------------
# PASSWORD RESET
# ----------------------------------------------------------------------------
request_password_reset <- function(email, db_path = "lottery_users.db") {
  reset_token <- paste0(sample(c(0:9, letters), 32, replace = TRUE), collapse = "")
  expiry <- format(Sys.time() + 3600, "%Y-%m-%d %H:%M:%S") # 1 hour
  
  user_id <- db_create_reset_token(email, reset_token, expiry, db_path)
  
  # Don't reveal if email exists (security)
  if (!is.null(user_id)) {
    db_log_action(user_id, "password_reset_requested", "Reset token generated", db_path)
  }
  
  return(list(
    success = TRUE,
    reset_token = if(!is.null(user_id)) reset_token else NULL,
    message = "If email exists, reset link sent"
  ))
}

reset_password <- function(token, new_password, db_path = "lottery_users.db") {
  if (nchar(new_password) < 8) {
    return(list(success = FALSE, error = "Password must be at least 8 characters"))
  }
  
  password_hash <- sodium::password_store(new_password)
  user_id <- db_reset_password(token, password_hash, db_path)
  
  if (is.null(user_id)) {
    return(list(success = FALSE, error = "Invalid or expired reset token"))
  }
  
  db_log_action(user_id, "password_reset", "Password changed via reset", db_path)
  
  return(list(success = TRUE, message = "Password reset successful"))
}

# ----------------------------------------------------------------------------
# RATE LIMITING
# ----------------------------------------------------------------------------
check_rate_limit <- function(user_id, action = "api_call", db_path = "lottery_users.db") {
  subscription <- db_get_subscription(user_id, db_path)
  
  # Get plan limits
  
  plan <- subscription_plans[[subscription$plan_type]]
  
  # Count today's usage
  count <- db_get_daily_usage(user_id, action, db_path)
  limit <- plan$limits$api_calls_per_day
  
  return(list(
    allowed = count < limit,
    current = count,
    limit = limit,
    plan = subscription$plan_type
  ))
}
