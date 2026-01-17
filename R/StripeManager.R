# ============================================================================
# stripe_manager.R - Stripe Payment Integration
# ============================================================================


# ----------------------------------------------------------------------------
# SUBSCRIPTION PLANS CONFIGURATION
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

# ----------------------------------------------------------------------------
# STRIPE API KEY
# ----------------------------------------------------------------------------
get_stripe_key <- function() {
  key <- Sys.getenv("STRIPE_SECRET_KEY")
  if (key == "" || is.null(key)) {
    warning("STRIPE_SECRET_KEY not configured")
    return(NULL)
  }
  return(key)
}

# ----------------------------------------------------------------------------
# CUSTOMER MANAGEMENT
# ----------------------------------------------------------------------------
stripe_create_customer <- function(email) {
  stripe_key <- get_stripe_key()
  if (is.null(stripe_key)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  tryCatch({
    response <- POST(
      url = "https://api.stripe.com/v1/customers",
      add_headers(Authorization = paste("Bearer", stripe_key)),
      body = list(email = email),
      encode = "form"
    )
    
    if (status_code(response) == 200) {
      customer <- content(response)
      return(list(success = TRUE, customer_id = customer$id))
    } else {
      error_msg <- content(response)$error$message
      return(list(success = FALSE, error = error_msg))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# ----------------------------------------------------------------------------
# CHECKOUT SESSION
# ----------------------------------------------------------------------------
stripe_create_checkout <- function(user_id, email, customer_id, plan_type, success_url, cancel_url) {
  stripe_key <- get_stripe_key()
  if (is.null(stripe_key)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  # Get plan details
  plan <- subscription_plans[[plan_type]]
  if (is.null(plan)) {
    return(list(success = FALSE, error = "Invalid plan type"))
  }
  
  # Create or use existing customer
  if (is.null(customer_id) || is.na(customer_id) || customer_id == "") {
    customer_result <- stripe_create_customer(email)
    if (!customer_result$success) {
      return(customer_result)
    }
    customer_id <- customer_result$customer_id
  }
  
  tryCatch({
    response <- POST(
      url = "https://api.stripe.com/v1/checkout/sessions",
      add_headers(Authorization = paste("Bearer", stripe_key)),
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
    
    if (status_code(response) == 200) {
      session <- content(response)
      return(list(
        success = TRUE, 
        url = session$url, 
        session_id = session$id,
        customer_id = customer_id
      ))
    } else {
      error_msg <- content(response)$error$message
      return(list(success = FALSE, error = error_msg))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# ----------------------------------------------------------------------------
# PAYMENT VERIFICATION
# ----------------------------------------------------------------------------
stripe_verify_session <- function(session_id) {
  stripe_key <- get_stripe_key()
  if (is.null(stripe_key)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  tryCatch({
    response <- GET(
      url = paste0("https://api.stripe.com/v1/checkout/sessions/", session_id),
      add_headers(Authorization = paste("Bearer", stripe_key))
    )
    
    if (status_code(response) != 200) {
      return(list(success = FALSE, error = "Could not verify session"))
    }
    
    session <- content(response)
    
    if (session$payment_status != "paid") {
      return(list(success = FALSE, error = "Payment not completed"))
    }
    
    return(list(
      success = TRUE,
      user_id = as.numeric(session$metadata$user_id),
      plan_type = session$metadata$plan_type,
      subscription_id = session$subscription,
      customer_id = session$customer
    ))
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# ----------------------------------------------------------------------------
# SUBSCRIPTION MANAGEMENT
# ----------------------------------------------------------------------------
stripe_cancel_subscription <- function(subscription_id) {
  stripe_key <- get_stripe_key()
  if (is.null(stripe_key)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  tryCatch({
    response <- DELETE(
      url = paste0("https://api.stripe.com/v1/subscriptions/", subscription_id),
      add_headers(Authorization = paste("Bearer", stripe_key))
    )
    
    if (status_code(response) == 200) {
      return(list(success = TRUE))
    } else {
      error_msg <- content(response)$error$message
      return(list(success = FALSE, error = error_msg))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

stripe_get_subscription <- function(subscription_id) {
  stripe_key <- get_stripe_key()
  if (is.null(stripe_key)) {
    return(list(success = FALSE, error = "Stripe not configured"))
  }
  
  tryCatch({
    response <- GET(
      url = paste0("https://api.stripe.com/v1/subscriptions/", subscription_id),
      add_headers(Authorization = paste("Bearer", stripe_key))
    )
    
    if (status_code(response) == 200) {
      subscription <- content(response)
      return(list(
        success = TRUE,
        status = subscription$status,
        current_period_end = as.POSIXct(subscription$current_period_end, origin = "1970-01-01")
      ))
    } else {
      return(list(success = FALSE, error = "Could not retrieve subscription"))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}