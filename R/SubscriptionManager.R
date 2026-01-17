# ============================================================================
# subscription_manager.R - Subscription Business Logic
# ============================================================================



# ----------------------------------------------------------------------------
# CREATE CHECKOUT SESSION
# ----------------------------------------------------------------------------
create_checkout_session <- function(user_id, plan_type, success_url, cancel_url, db_path = "lottery_users.db") {
  # Get user info
  user <- db_get_user(user_id = user_id, db_path = db_path)
  if (is.null(user)) {
    return(list(success = FALSE, error = "User not found"))
  }
  
  # Get existing subscription
  subscription <- db_get_subscription(user_id, db_path)
  customer_id <- subscription$stripe_customer_id
  
  # Create Stripe checkout
  result <- stripe_create_checkout(
    user_id = user_id,
    email = user$email,
    customer_id = customer_id,
    plan_type = plan_type,
    success_url = success_url,
    cancel_url = cancel_url
  )
  
  if (result$success) {
    # Save customer ID if new
    if (!is.null(result$customer_id) && (is.null(customer_id) || is.na(customer_id))) {
      db_update_stripe_customer(user_id, result$customer_id, db_path)
    }
    
    # Log checkout initiation
    db_log_action(user_id, "checkout_initiated", paste("Plan:", plan_type), db_path)
  }
  
  return(result)
}

# ----------------------------------------------------------------------------
# VERIFY AND COMPLETE PAYMENT
# ----------------------------------------------------------------------------
verify_and_upgrade <- function(session_id, db_path = "lottery_users.db") {
  # Verify payment with Stripe
  result <- stripe_verify_session(session_id)
  
  if (!result$success) {
    return(result)
  }
  
  # Upgrade subscription in database
  upgrade_result <- upgrade_subscription(
    user_id = result$user_id,
    plan_type = result$plan_type,
    stripe_subscription_id = result$subscription_id,
    db_path = db_path
  )
  
  return(upgrade_result)
}

# ----------------------------------------------------------------------------
# UPGRADE SUBSCRIPTION
# ----------------------------------------------------------------------------
upgrade_subscription <- function(user_id, plan_type, stripe_subscription_id = NULL, db_path = "lottery_users.db") {
  success <- db_update_subscription(user_id, plan_type, stripe_subscription_id, db_path)
  
  if (success) {
    db_log_action(user_id, "subscription_upgraded", paste("New plan:", plan_type), db_path)
    return(list(success = TRUE, message = paste("Upgraded to", plan_type)))
  } else {
    return(list(success = FALSE, message = "Failed to upgrade subscription"))
  }
}

# ----------------------------------------------------------------------------
# DOWNGRADE TO FREE
# ----------------------------------------------------------------------------
downgrade_to_free <- function(user_id, db_path = "lottery_users.db") {
  # Get current subscription
  subscription <- db_get_subscription(user_id, db_path)
  
  # Cancel Stripe subscription if exists
  if (!is.null(subscription$stripe_subscription_id) && 
      !is.na(subscription$stripe_subscription_id) && 
      subscription$stripe_subscription_id != "") {
    
    cancel_result <- stripe_cancel_subscription(subscription$stripe_subscription_id)
    
    if (!cancel_result$success) {
      return(list(
        success = FALSE,
        error = paste("Could not cancel Stripe subscription:", cancel_result$error)
      ))
    }
  }
  
  # Update database
  success <- db_update_subscription(user_id, "free", NULL, db_path)
  
  if (success) {
    db_log_action(user_id, "subscription_downgraded", "Downgraded to free", db_path)
    return(list(success = TRUE, message = "Downgraded to Free plan"))
  } else {
    return(list(success = FALSE, error = "Failed to downgrade subscription"))
  }
}

# ----------------------------------------------------------------------------
# GET SUBSCRIPTION STATUS
# ----------------------------------------------------------------------------
get_subscription_status <- function(user_id, db_path = "lottery_users.db") {
  subscription <- db_get_subscription(user_id, db_path)
  
  # If has Stripe subscription, check status
  if (!is.null(subscription$stripe_subscription_id) && 
      !is.na(subscription$stripe_subscription_id) &&
      subscription$stripe_subscription_id != "") {
    
    stripe_status <- stripe_get_subscription(subscription$stripe_subscription_id)
    
    if (stripe_status$success) {
      subscription$stripe_status = stripe_status$status
      subscription$next_billing = stripe_status$current_period_end
    }
  }
  
  return(subscription)
}