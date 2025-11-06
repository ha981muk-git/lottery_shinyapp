# ============================================================================
# shiny_auth_modules.R - Shiny Authentication UI & Server Modules
# ============================================================================

source("AuthManager.R")

# ----------------------------------------------------------------------------
# AUTH UI MODULE
# ----------------------------------------------------------------------------
auth_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "auth-container",
        # Login Form
        div(class = "auth-form",
            h3("Login"),
            textInput(ns("login_user"), "Username or Email"),
            passwordInput(ns("login_pass"), "Password"),
            actionButton(ns("login_btn"), "Login", class = "btn-primary"),
            uiOutput(ns("login_status_ui")),
            br(),
            actionLink(ns("show_reset"), "Forgot password?")
        ),
        
        # Registration Form
        div(class = "auth-form",
            h3("Register"),
            textInput(ns("reg_user"), "Username"),
            textInput(ns("reg_email"), "Email"),
            passwordInput(ns("reg_pass"), "Password (min 8 characters)"),
            actionButton(ns("reg_btn"), "Register", class = "btn-primary"),
            uiOutput(ns("reg_status_ui"))
        )
    )
  )
}

# ----------------------------------------------------------------------------
# AUTH SERVER MODULE
# ----------------------------------------------------------------------------
auth_server <- function(id, db_path = "lottery_users.db") {
  moduleServer(id, function(input, output, session) {
    user_info <- reactiveVal(NULL)
    
    
    # Login handler
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pass)
      
      result <- verify_user(input$login_user, input$login_pass, db_path = db_path)
      
      if (result$success) {
        # ✅ Use fallback if API doesn’t return user_id
        uid <- result$user_id %||% user_info()$id
        
        user <- get_user_info(uid, db_path)
        subscription <- get_user_subscription(uid, db_path)
        
        # ✅ Consistent naming — app uses user_info() reactive
        user_info(list(
          id = uid,
          username = user$username,
          email = user$email,
          email_verified = result$email_verified,
          subscription = subscription
        ))
        
        
        output$login_status_ui <- renderUI({
          div(class = "auth-status success", "✅ Login successful!")
        })
      } else {
        output$login_status_ui <- renderUI({
          div(class = "auth-status error", paste("❌", result$error))
        })
      }
    })
    
    # Registration handler
    observeEvent(input$reg_btn, {
      req(input$reg_user, input$reg_email, input$reg_pass)
      
      result <- register_user(
        username = input$reg_user,
        email = input$reg_email,
        password = input$reg_pass,
        db_path = db_path
      )
      
      if (result$success) {
        output$reg_status_ui <- renderUI({
          div(class = "auth-status success",
              "✅ Account created! Check your email for verification link.")
        })
      } else {
        output$reg_status_ui <- renderUI({
          div(class = "auth-status error", paste("❌", result$error))
        })
      }
    })
    
    # Password reset link
    observeEvent(input$show_reset, {
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
      req(input$reset_email)
      result <- request_password_reset(input$reset_email, db_path)
      
      removeModal()
      showNotification(result$message, type = "message")
    })
    
    return(user_info)
    
  })
}

# ----------------------------------------------------------------------------
# USER PROFILE UI MODULE
# ----------------------------------------------------------------------------
profile_ui <- function(id) {
  ns <- NS(id)
  
  div(class = "profile-container",
      h3("User Profile"),
      uiOutput(ns("user_info")),
      hr(),
      h4("Account Actions"),
      actionButton(ns("logout_btn"), "Logout", class = "btn-secondary")
  )
}

# ----------------------------------------------------------------------------
# USER PROFILE SERVER MODULE
# ----------------------------------------------------------------------------
profile_server <- function(id, user_info) {
  moduleServer(id, function(input, output, session) {
    
    output$user_info <- renderUI({
      req(user_info())
      
      user <- user_info()
      
      tagList(
        p(strong("Username: "), user$username),
        p(strong("Email: "), user$email),
        p(strong("Email Verified: "), 
          if(user$email_verified) "✅ Yes" else "❌ No"),
        p(strong("Plan: "), user$subscription$plan_type),
        p(strong("Status: "), user$subscription$status)
      )
    })
    
    observeEvent(input$logout_btn, {
      user_info(NULL)
      showNotification("Logged out successfully", type = "message")
    })
  })
}