# ============================================================================
# AuthenticationUI.R - Separated Authentication UI Components
# ============================================================================

# ----------------------------------------------------------------------------
# 1. LOGIN/REGISTRATION UI
# ----------------------------------------------------------------------------
auth_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .auth-wrapper {
          min-height: 100vh;
          display: flex;
          align-items: center;
          justify-content: center;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          padding: 20px;
        }
        .auth-card {
          background: white;
          border-radius: 16px;
          box-shadow: 0 20px 60px rgba(0,0,0,0.3);
          max-width: 440px;
          width: 100%;
          padding: 40px;
        }
        .auth-header {
          text-align: center;
          margin-bottom: 30px;
        }
        .auth-header h2 {
          color: #333;
          margin-bottom: 8px;
          font-size: 28px;
        }
        .auth-header p {
          color: #666;
          font-size: 14px;
        }
        .auth-tabs {
          display: flex;
          gap: 10px;
          margin-bottom: 30px;
          border-bottom: 2px solid #e0e0e0;
        }
        .auth-tab {
          flex: 1;
          padding: 12px;
          border: none;
          background: transparent;
          cursor: pointer;
          font-weight: 600;
          color: #999;
          transition: all 0.3s;
          border-bottom: 3px solid transparent;
          margin-bottom: -2px;
        }
        .auth-tab.active {
          color: #667eea;
          border-bottom-color: #667eea;
        }
        .auth-tab:hover {
          color: #667eea;
        }
        .auth-form {
          display: none;
        }
        .auth-form.active {
          display: block;
        }
        .form-group {
          margin-bottom: 20px;
        }
        .form-group label {
          display: block;
          margin-bottom: 8px;
          color: #333;
          font-weight: 500;
          font-size: 14px;
        }
        .form-group input {
          width: 100%;
          padding: 12px 16px;
          border: 2px solid #e0e0e0;
          border-radius: 8px;
          font-size: 14px;
          transition: all 0.3s;
        }
        .form-group input:focus {
          outline: none;
          border-color: #667eea;
          box-shadow: 0 0 0 3px rgba(102,126,234,0.1);
        }
        .auth-btn {
          width: 100%;
          padding: 14px;
          border: none;
          border-radius: 8px;
          font-weight: 600;
          font-size: 16px;
          cursor: pointer;
          transition: all 0.3s;
          margin-top: 10px;
        }
        .auth-btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
        }
        .auth-btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 20px rgba(102,126,234,0.4);
        }
        .auth-status {
          margin-top: 15px;
          padding: 12px;
          border-radius: 8px;
          font-size: 14px;
          text-align: center;
        }
        .auth-status.success {
          background: #d4edda;
          color: #155724;
          border: 1px solid #c3e6cb;
        }
        .auth-status.error {
          background: #f8d7da;
          color: #721c24;
          border: 1px solid #f5c6cb;
        }
        .auth-divider {
          text-align: center;
          margin: 20px 0;
          color: #999;
          font-size: 12px;
        }
      "))
    ),
    
    div(class = "auth-wrapper",
        div(class = "auth-card",
            div(class = "auth-header",
                h2("🎲 Lottery Insights"),
                p("Educational Lottery Analysis Platform")
            ),
            
            div(class = "auth-tabs",
                tags$button(class = "auth-tab active", onclick = "switchAuthTab('login')", "Login"),
                tags$button(class = "auth-tab", onclick = "switchAuthTab('register')", "Register")
            ),
            
            # Login Form
            div(id = "login-form", class = "auth-form active",
                div(class = "form-group",
                    tags$label("Username or Email"),
                    textInput(ns("login_user"), NULL, placeholder = "Enter username or email")
                ),
                div(class = "form-group",
                    tags$label("Password"),
                    passwordInput(ns("login_pass"), NULL, placeholder = "Enter password")
                ),
                actionButton(ns("login_btn"), "Login", class = "auth-btn auth-btn-primary"),
                uiOutput(ns("login_status_ui"))
            ),
            
            # Registration Form
            div(id = "register-form", class = "auth-form",
                div(class = "form-group",
                    tags$label("Username"),
                    textInput(ns("reg_user"), NULL, placeholder = "Choose a username")
                ),
                div(class = "form-group",
                    tags$label("Email"),
                    textInput(ns("reg_email"), NULL, placeholder = "your@email.com")
                ),
                div(class = "form-group",
                    tags$label("Password"),
                    passwordInput(ns("reg_pass"), NULL, placeholder = "Min. 8 characters")
                ),
                actionButton(ns("reg_btn"), "Create Account", class = "auth-btn auth-btn-primary"),
                uiOutput(ns("reg_status_ui"))
            ),
            
            div(class = "auth-divider", "Educational platform - Free tier available")
        )
    ),
    
    tags$script(HTML("
      function switchAuthTab(tab) {
        document.querySelectorAll('.auth-tab').forEach(t => t.classList.remove('active'));
        document.querySelectorAll('.auth-form').forEach(f => f.classList.remove('active'));
        
        if (tab === 'login') {
          document.querySelector('.auth-tab:first-child').classList.add('active');
          document.getElementById('login-form').classList.add('active');
        } else {
          document.querySelector('.auth-tab:last-child').classList.add('active');
          document.getElementById('register-form').classList.add('active');
        }
      }
    "))
  )
}


# ----------------------------------------------------------------------------
# 2. SUBSCRIPTION MANAGEMENT UI
# ----------------------------------------------------------------------------
subscription_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .subscription-container {
          max-width: 1200px;
          margin: 40px auto;
          padding: 20px;
        }
        .subscription-header {
          text-align: center;
          margin-bottom: 50px;
        }
        .subscription-header h2 {
          color: #e8eaed;
          font-size: 36px;
          margin-bottom: 10px;
        }
        .subscription-header p {
          color: rgba(255,255,255,0.7);
          font-size: 18px;
        }
        .current-plan-banner {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          padding: 30px;
          border-radius: 16px;
          margin-bottom: 40px;
          color: white;
          text-align: center;
        }
        .current-plan-banner h3 {
          margin: 0 0 10px 0;
          font-size: 24px;
        }
        .current-plan-banner p {
          margin: 5px 0;
          opacity: 0.9;
        }
        .pricing-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 30px;
          margin-top: 40px;
        }
        .pricing-card {
          background: rgba(255,255,255,0.05);
          border: 2px solid rgba(255,255,255,0.1);
          border-radius: 16px;
          padding: 35px;
          text-align: center;
          transition: all 0.3s;
          position: relative;
        }
        .pricing-card:hover {
          transform: translateY(-8px);
          border-color: #667eea;
          box-shadow: 0 20px 40px rgba(102,126,234,0.3);
        }
        .pricing-card.featured {
          border-color: #667eea;
          background: rgba(102,126,234,0.1);
        }
        .pricing-card .badge {
          position: absolute;
          top: -12px;
          right: 20px;
          background: #667eea;
          color: white;
          padding: 6px 16px;
          border-radius: 20px;
          font-size: 12px;
          font-weight: 600;
        }
        .pricing-card h3 {
          color: #e8eaed;
          font-size: 28px;
          margin-bottom: 15px;
        }
        .pricing-card .price {
          font-size: 48px;
          font-weight: 700;
          color: #667eea;
          margin: 20px 0;
        }
        .pricing-card .price span {
          font-size: 20px;
          color: rgba(255,255,255,0.6);
        }
        .pricing-card ul {
          list-style: none;
          padding: 0;
          margin: 30px 0;
          text-align: left;
        }
        .pricing-card ul li {
          padding: 12px 0;
          color: rgba(255,255,255,0.8);
          border-bottom: 1px solid rgba(255,255,255,0.1);
        }
        .pricing-card ul li:before {
          content: '✓';
          color: #10b981;
          font-weight: bold;
          margin-right: 12px;
        }
        .pricing-btn {
          width: 100%;
          padding: 14px;
          border: none;
          border-radius: 8px;
          font-weight: 600;
          font-size: 16px;
          cursor: pointer;
          transition: all 0.3s;
          margin-top: 20px;
        }
        .pricing-btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
        }
        .pricing-btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 20px rgba(102,126,234,0.4);
        }
        .pricing-btn-current {
          background: rgba(255,255,255,0.1);
          color: rgba(255,255,255,0.5);
          cursor: not-allowed;
        }
      "))
    ),
    
    div(class = "subscription-container",
        div(class = "subscription-header",
            h2("Choose Your Plan"),
            p("Unlock advanced features and support the project")
        ),
        
        # Current Plan Banner
        uiOutput(ns("current_plan_banner")),
        
        # Pricing Cards
        div(class = "pricing-grid",
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
            div(class = "pricing-card",
                h3("Basic"),
                div(class = "price", "€9.99", tags$span("/month")),
                tags$ul(
                  tags$li("Advanced pattern detection"),
                  tags$li("Full historical data access"),
                  tags$li("Custom date ranges"),
                  tags$li("Export functionality"),
                  tags$li("Priority email support")
                ),
                uiOutput(ns("basic_btn"))
            ),
            
            # Premium Plan
            div(class = "pricing-card featured",
                span(class = "badge", "MOST POPULAR"),
                h3("Premium"),
                div(class = "price", "€24.99", tags$span("/month")),
                tags$ul(
                  tags$li("Everything in Basic"),
                  tags$li("AI-powered insights"),
                  tags$li("Statistical forecasting"),
                  tags$li("API access"),
                  tags$li("Custom notifications"),
                  tags$li("24/7 priority support"),
                  tags$li("Early access to features")
                ),
                uiOutput(ns("premium_btn"))
            )
        )
    )
  )
}