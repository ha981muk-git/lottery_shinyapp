# ============================================================================
# shiny_subscription_modules.R - Subscription Management UI & Server
# ============================================================================
source("SubscriptionManager.R")

# ----------------------------------------------------------------------------
# SUBSCRIPTION UI MODULE
# ----------------------------------------------------------------------------
subscription_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    # Current plan banner
    uiOutput(ns("current_plan_banner")),
    
    # Pricing cards
    div(class = "pricing-container",
        # Free Plan
        div(class = "pricing-card",
            h3("Free"),
            div(class = "price", "€0", tags$span("/month")),
            tags$ul(
              tags$li("Basic number analysis"),
              tags$li("Limited historical data"),
              tags$li("Community support"),
              tags$li("Educational resources")
            ),
            uiOutput(ns("free_btn"))
        ),
        
        # Basic Plan
        div(class = "pricing-card pricing-card-featured",
            div(class = "badge", "POPULAR"),
            h3("Basic"),
            div(class = "price", "€9.99", tags$span("/month")),
            tags$ul(
              tags$li("Advanced pattern detection"),
              tags$li("Full historical data"),
              tags$li("Custom date ranges"),
              tags$li("Export functionality"),
              tags$li("Priority email support")
            ),
            uiOutput(ns("basic_btn"))
        ),
        
        # Premium Plan
        div(class = "pricing-card",
            h3("Premium"),
            div(class = "price", "€24.99", tags$span("/month")),
            tags$ul(
              tags$li("Everything in Basic"),
              tags$li("AI-powered insights"),
              tags$li("Statistical forecasting"),
              tags$li("API access"),
              tags$li("24/7 priority support")
            ),
            uiOutput(ns("premium_btn"))
        )
    )
  )
}

# ----------------------------------------------------------------------------
# SUBSCRIPTION SERVER MODULE
# ----------------------------------------------------------------------------
subscription_server <- function(id, user_info, db_path = "lottery_users.db") {
  moduleServer(id, function(input, output, session) {
    
    # Current plan banner
    output$current_plan_banner <- renderUI({
      req(user_info())
      
      sub <- user_info()$subscription
      
      div(class = "current-plan-banner",
          h3(paste("Current Plan:", toupper(sub$plan_type))),
          p(paste("Status:", sub$status)),
          if (sub$plan_type != "free" && !is.null(sub$end_date)) {
            p(paste("Next billing:", format(as.Date(sub$end_date), "%B %d, %Y")))
          }
      )
    })
    
    # Dynamic buttons
    output$free_btn <- renderUI({
      req(user_info())
      current_plan <- user_info()$subscription$plan_type
      
      if (current_plan == "free") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("downgrade_free"), "Downgrade", 
                     class = "pricing-btn pricing-btn-secondary")
      }
    })
    
    output$basic_btn <- renderUI({
      req(user_info())
      current_plan <- user_info()$subscription$plan_type
      
      if (current_plan == "basic") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("upgrade_basic"), 
                     if(current_plan == "free") "Upgrade" else "Switch Plan",
                     class = "pricing-btn pricing-btn-primary")
      }
    })
    
    output$premium_btn <- renderUI({
      req(user_info())
      current_plan <- user_info()$subscription$plan_type
      
      if (current_plan == "premium") {
        tags$button(class = "pricing-btn pricing-btn-current", "Current Plan")
      } else {
        actionButton(session$ns("upgrade_premium"), "Upgrade", 
                     class = "pricing-btn pricing-btn-primary")
      }
    })
    
    # Upgrade to Basic
    observeEvent(input$upgrade_basic, {
      req(user_info())
      
      app_url <- get_app_url(session)
      
      result <- create_checkout_session(
        user_id = user_info()$id,
        plan_type = "basic",
        success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}"),
        cancel_url = paste0(app_url, "?payment=cancel"),
        db_path = db_path
      )
      
      if (result$success) {
        session$sendCustomMessage("redirect", result$url)
      } else {
        showNotification(paste("Error:", result$error), type = "error", duration = 10)
      }
    })
    
    # Upgrade to Premium
    observeEvent(input$upgrade_premium, {
      req(user_info())
      
      app_url <- get_app_url(session)
      
      result <- create_checkout_session(
        user_id = user_info()$id,
        plan_type = "premium",
        success_url = paste0(app_url, "?session_id={CHECKOUT_SESSION_ID}"),
        cancel_url = paste0(app_url, "?payment=cancel"),
        db_path = db_path
      )
      
      if (result$success) {
        session$sendCustomMessage("redirect", result$url)
      } else {
        showNotification(paste("Error:", result$error), type = "error", duration = 10)
      }
    })
    
    # Downgrade to Free
    observeEvent(input$downgrade_free, {
      req(user_info())
      
      showModal(modalDialog(
        title = "⚠️ Confirm Downgrade",
        "Are you sure you want to downgrade to the Free plan? You will lose access to premium features immediately.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_downgrade"), "Yes, Downgrade", 
                       class = "btn-danger")
        )
      ))
    })
    
    # Confirm downgrade
    observeEvent(input$confirm_downgrade, {
      req(user_info())
      
      current_user_id <- isolate(user_info()$id)
      
      result <- downgrade_to_free(current_user_id, db_path)
      
      if (result$success) {
        # Update user info
        updated_user <- isolate(user_info())
        updated_user$subscription <- get_user_subscription(current_user_id, db_path)
        user_info(updated_user)
        
        removeModal()
        showNotification("✅ Successfully downgraded to Free plan", 
                         type = "message", duration = 3)
        
        shinyjs::delay(1500, {
          session$reload()
        })
      } else {
        removeModal()
        showNotification(paste("❌ Downgrade failed:", result$error), 
                         type = "error")
      }
    })
  })
}

# ----------------------------------------------------------------------------
# HELPER FUNCTION
# ----------------------------------------------------------------------------
get_app_url <- function(session) {
  base_url <- session$clientData$url_protocol
  host <- session$clientData$url_hostname
  port <- session$clientData$url_port
  
  if (port == "") {
    return(paste0(base_url, "//", host))
  } else {
    return(paste0(base_url, "//", host, ":", port))
  }
}