# --- Production-ready Shiny options for DigitalOcean ---

options(
  # Max upload size (adjust as needed)
  shiny.maxRequestSize = 10*1024^2,  # 10 MB
  
  # Sanitize errors for end users (security)
  shiny.sanitize.errors = TRUE,
  
  # Disable reactive log (slightly improves performance)
  shiny.reactlog = FALSE,
  
  # Disable automatic app reload (good for production)
  shiny.autoreload = FALSE,
  
  # Disable bookmarking if not needed
  shiny.enableBookmarking = "disable",
  
  # Session timeout (in seconds)
  shiny.session.timeout = 3600,  # 1 hour
  
  # CRAN repository for package installs
  repos = c(CRAN = "https://cloud.r-project.org/"),
  
  # Override default error handler
  shiny.error = function(e) {
    # Log the error internally (server console or log file)
    message("[Shiny Error] ", Sys.time(), " - ", e$message)
    
    # Friendly error message to user
    stop("An unexpected error occurred. Please contact support.")
  },
  
  # Reduce stack trace verbosity
  shiny.fullstacktrace = FALSE
)

# Limit CPU threads to 1 to avoid overloading 1 vCPU container
Sys.setenv(R_THREADS = 1)

library(shiny)
library(vroom)
library(dplyr)
library(janitor)
library(bslib)
library(shinyjs)
library(plotly)
library(waiter)
library(tidyr)
library(purrr)
library(DT)


# ---------- UI helper theme ----------
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#0a0e27",
  fg = "#e8eaed",
  primary = "#8b5cf6",
  secondary = "#ec4899",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

# Load translations
tryCatch({
  if (!file.exists("translations.R")) {
    stop("translations.R not found")
  }
  source("translations.R")
  cat("✓ Translations loaded\n")
}, error = function(e) {
  cat("✗ CRITICAL ERROR:", conditionMessage(e), "\n")
  stop(e)
})

# Load PrepareData.R
tryCatch({
  if (!file.exists("PrepareData.R")) {
    stop("PrepareData.R not found")
  }
  source("PrepareData.R")
  cat("✓ PrepareData loaded\n")
}, error = function(e) {
  cat("✗ CRITICAL ERROR:", conditionMessage(e), "\n")
  stop(e)
})

# Load DashboardModule.R
tryCatch({
  if (!file.exists("DashboardModule.R")) {
    stop("DashboardModule.R not found")
  }
  source("DashboardModule.R")
  cat("✓ DashboardModule loaded\n")
}, error = function(e) {
  cat("✗ CRITICAL ERROR:", conditionMessage(e), "\n")
  stop(e)
})

# Load metric files from dashboard folder (optional but warn if empty)
dashboard_path <- "dashboard"
if (!dir.exists(dashboard_path)) {
  cat("⚠ Warning: dashboard folder not found\n")
} else {
  metric_files <- list.files(
    path = dashboard_path,
    pattern = "\\.R$",
    full.names = TRUE
  )
  
  if (length(metric_files) == 0) {
    cat("⚠ Warning: No .R files in dashboard folder\n")
  } else {
    for (f in metric_files) {
      tryCatch({
        source(f)
        cat("✓ Loaded metric:", basename(f), "\n")
      }, error = function(e) {
        cat("✗ ERROR loading", basename(f), ":", conditionMessage(e), "\n")
        stop(e)
      })
    }
  }
}


# ============================================================================
# UI - SEPARATE, with language parameter
ui <- function(request) {
  # ✅ Get language from URL or default to German
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  
  fluidPage(
    # conditionalPanel(
    #   condition = "$('html').hasClass('shiny-busy')",
    #   div(style = "position: fixed; top: 50%; left: 50%; 
    #            transform: translate(-50%, -50%); z-index: 9999;",
    #       h3("Loading... (first load may take 30–60 seconds)"),
    #       tags$img(src = "spinner.gif")
    #   )
    # ),
    theme = app_theme,
    
    tags$head(
      # ==================== SEO META TAGS (GERMAN OPTIMIZED) ====================
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "6/49 Lotto-Analyse Tool - Kostenlos, interaktiv, bildungsbasiert. Analysieren Sie Lottozahlen-Muster, Häufigkeiten und Trends mit unserem statistischen Dashboard."),
      tags$meta(name = "keywords", content = "Lotto Analyse, 6/49, Lotto 6 aus 49, Zahlenanalyse, Statistik, Zahlenmuster, Häufigkeitsanalyse, Lottovorhersage, Bildungstool"),
      tags$meta(name = "author", content = "Lottery Insights"),
      tags$meta(name = "robots", content = "index, follow"),
      tags$meta(name = "language", content = if(LANG == "de") "de" else "en"),
      tags$meta(name = "geo.placename", content = "Deutschland"),
      tags$meta(name = "geo.region", content = "DE"),
      tags$meta(name = "google-site-verification", content = "SCaDZ-eWJCu14j6urMNGER1iqoqwf_1imzwnm5PjMeo"),
      
      # Open Graph Tags (Social Media - German)
      tags$meta(property = "og:title", content = "6/49 Lotto-Analyse Tool"),
      tags$meta(property = "og:description", content = "Kostenloses, interaktives Bildungs-Dashboard zur Analyse von Lottomustern und Zahlenstatistiken"),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/"),
      tags$meta(property = "og:locale", content = "de_DE"),
      
      # Canonical Tag
      tags$link(rel = "canonical", href = "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/"),
      
      # Alternate Links for language versions
      tags$link(rel = "alternate", hreflang = "de", href = "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/?lang=de"),
      tags$link(rel = "alternate", hreflang = "en", href = "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/?lang=en"),
      tags$link(rel = "alternate", hreflang = "x-default", href = "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/"),
      
      # Schema Markup (JSON-LD - German)
      tags$script(type = "application/ld+json", HTML('
      {
        "@context": "https://schema.org",
        "@type": "WebApplication",
        "name": "6/49 Lotto-Analyse Tool",
        "alternateName": "6 aus 49 Lottozahlen Analysator",
        "description": "Kostenloses Bildungs-Tool zur statistischen Analyse von Lottodaten und Zahlenmuster",
        "url": "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/",
        "applicationCategory": "EducationalApplication",
        "inLanguage": "de",
        "offers": {
          "@type": "Offer",
          "price": "0",
          "priceCurrency": "EUR"
        },
        "creator": {
          "@type": "Organization",
          "name": "Lottery Insights"
        }
      }
      ')),
      
      # Favicon
      tags$link(rel = "icon", type = "image/svg+xml", href = "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🎲</text></svg>"),
      
      # Existing stylesheets and scripts
      tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
      useShinyjs(),
      use_waiter(),
      
      # ✅ OPTIMIZED INLINE STYLES WITH GPU ACCELERATION
      tags$style(HTML("
            @keyframes shimmer {
              0% { background-position: -200% 0; }
              100% { background-position: 200% 0; }
            }
            @keyframes fadeIn {
              from { opacity: 0; transform: translateY(5px) translateZ(0); }
              to { opacity: 1; transform: translateY(0) translateZ(0); }
            }
            .skeleton-card {
              height: 200px;
              background: linear-gradient(90deg, 
                rgba(139,92,246,0.08) 25%, 
                rgba(139,92,246,0.15) 50%, 
                rgba(139,92,246,0.08) 75%);
              background-size: 200% 100%;
              animation: shimmer 1.2s linear infinite;
              will-change: background-position;
              border-radius: 12px;
              margin-bottom: 20px;
            }
            .metric-container {
              animation: fadeIn 0.3s ease-out;
              will-change: opacity, transform;
            }
            @media (prefers-reduced-motion: reduce) {
              * { animation: none !important; transition: none !important; }
            }
      ")),
      
      # ✅ FIX: STOP MUTATIONOBSERVER AFTER 3 SECONDS + DEBOUNCE
      tags$script(HTML("
        $(document).ready(function() {
          let debounceTimer;
          function fixSidebarOverlay() {
            $('.bslib-sidebar-layout > .main').css({
              'opacity': '1',
              'filter': 'none',
              'pointer-events': 'auto',
              'transition': 'none'
            });
            $('.sidebar-backdrop, .bslib-sidebar-backdrop').remove();
            $('.bslib-sidebar-layout').css({
              'display': 'grid',
              'grid-template-columns': 'auto 1fr',
              'gap': '20px'
            });
          }
          fixSidebarOverlay();
          setTimeout(fixSidebarOverlay, 100);
          setTimeout(fixSidebarOverlay, 500);
          
          // Debounced observer - only runs every 100ms
          const observer = new MutationObserver(() => {
            clearTimeout(debounceTimer);
            debounceTimer = setTimeout(fixSidebarOverlay, 100);
          });
          observer.observe(document.body, { childList: true, subtree: true });
          
          // CRITICAL: Stop observing after 3 seconds - sidebar is stable
          setTimeout(() => observer.disconnect(), 3000);
        });
      ")),
      
      # Responsive sidebar toggle
      tags$script(HTML("
        $(document).ready(function() {
          const toggleButton = $('<button class=\"sidebar-toggle-btn\">☰ Menü</button>')
            .css({
              position: 'fixed',
              top: '15px',
              left: '15px',
              background: '#8b5cf6',
              color: '#fff',
              border: 'none',
              padding: '10px 14px',
              borderRadius: '8px',
              fontSize: '18px',
              cursor: 'pointer',
              zIndex: 2000,
              display: 'none'
            })
            .appendTo('body')
            .on('click', function() {
              const layout = document.querySelector('.bslib-sidebar-layout');
              if (layout) {
                const open = layout.dataset.sidebarOpen === 'true';
                layout.dataset.sidebarOpen = !open;
              }
            });

          function checkScreen() {
            if (window.innerWidth < 768) {
              toggleButton.show();
            } else {
              toggleButton.hide();
            }
          }
          checkScreen();
          $(window).on('resize', checkScreen);
        });
      "))
    ),
    
    # ✅ Language switcher
    div(class = "lang-switcher",
        tags$a(href = "?lang=de", class = paste0("lang-btn", if(LANG == "de") " active" else ""), "🇩🇪 DE"),
        tags$a(href = "?lang=en", class = paste0("lang-btn", if(LANG == "en") " active" else ""), "🇬🇧 EN")
    ),
    
    # Professional Header
    div(class = "professional-header", role = "banner",
        div(class = "header-content",
            div(class = "logo-section",
                span("🎲", class = "logo-icon"),
                div(class = "logo-text",
                    h1(t("title", LANG), 
                       span(class = "testing-badge", t("testing_badge", LANG))),
                    p(t("subtitle", LANG))
                )
            ),
            div(class = "header-nav", role = "navigation",
                a(href = "#", t("nav_home", LANG)),
                a(href = "#analyzer", t("nav_analyzer", LANG)),
                a(href = "#educational", t("nav_educational", LANG)),
                a(href = "#disclaimer", t("nav_disclaimer", LANG))
            )
        )
    ),
    
    # Main Content
    div(class = "main-content",
        # Main Analyzer Section
        div(id = "analyzer", role = "region", `aria-label` = if(LANG == "de") "Analyse-Dashboard" else "Analysis Dashboard",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                class = "control-panel",
                open = "desktop",
                position = "left",
                max_height_mobile = NULL,
                h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #e8eaed;"),
                lotteryInputUI("inputs1", lang = LANG)
              ),
              # Main content
              div(
                style = "padding: 0; min-height: 100vh;",
                dashboardUI("dashboard1"),
              ),
              fillable = FALSE,
              border = FALSE,
              border_radius = FALSE
            )
        ),
        
        # Educational Notice
        div(class = "educational-notice", role = "note",
            h3(t("notice_title", LANG)),
            tags$ul(
              tags$li(strong(t("notice_1", LANG)), t("notice_1b", LANG)),
              tags$li(t("notice_2", LANG), strong(t("notice_2b", LANG)), t("notice_2c", LANG)),
              tags$li(t("notice_3", LANG)),
              tags$li(t("notice_4", LANG), strong(t("notice_4b", LANG)), t("notice_4c", LANG)),
              tags$li(t("notice_5", LANG)),
              tags$li(strong(t("notice_6", LANG)), t("notice_6b", LANG))
            ),
            p(style = "margin-top: 15px; font-style: italic; color: rgba(255,255,255,0.7);",
              t("notice_purpose", LANG))
        ),
        
        # Additional Educational Section
        div(id = "educational", role = "region", `aria-label` = if(LANG == "de") "Bildungsinformationen" else "Educational Information",
            style = "margin-top: 40px; padding: 30px; background: rgba(255,255,255,0.03); border-radius: 12px;",
            h2(t("edu_title", LANG), style = "color: #e8eaed;"),
            p(style = "color: rgba(255,255,255,0.7); line-height: 1.8;",
              t("edu_intro", LANG)
            ),
            h3(t("edu_objectives", LANG), style = "color: #e8eaed; margin-top: 20px;"),
            tags$ul(
              style = "color: rgba(255,255,255,0.7); line-height: 1.8;",
              tags$li(t("edu_obj_1", LANG)),
              tags$li(t("edu_obj_2", LANG)),
              tags$li(t("edu_obj_3", LANG)),
              tags$li(t("edu_obj_4", LANG))
            )
        )
    ),
    
    # Professional Footer
    div(class = "professional-footer", role = "contentinfo",
        div(class = "footer-content",
            div(class = "footer-sections",
                # About Section
                div(class = "footer-section",
                    h3(t("footer_about", LANG)),
                    p(strong(t("footer_edu_only", LANG))),
                    p(t("footer_desc", LANG)),
                    p(style = "color: #ffc107; font-weight: 600;", 
                      t("footer_construction", LANG))
                ),
                # Quick Links
                div(class = "footer-section",
                    h3(t("footer_quick", LANG)),
                    tags$ul(
                      tags$li(a(href = "#", t("nav_home", LANG))),
                      tags$li(a(href = "#analyzer", t("nav_analyzer", LANG))),
                      tags$li(a(href = "#educational", t("nav_educational", LANG))),
                      tags$li(a(href = "#disclaimer", t("nav_disclaimer", LANG)))
                    )
                ),
                # Legal & Disclaimer
                div(class = "footer-section",
                    h3(t("footer_legal", LANG)),
                    tags$ul(
                      tags$li(a(href = "#", t("footer_full_disclaimer", LANG))),
                      tags$li(a(href = "#", t("footer_privacy", LANG))),
                      tags$li(a(href = "#", t("footer_terms", LANG))),
                      tags$li(a(href = "#", t("footer_edu_statement", LANG)))
                    ),
                    p(style = "color: #e74c3c; font-size: 0.85em; margin-top: 10px;",
                      t("footer_no_gambling", LANG))
                ),
                # Important Information
                div(class = "footer-section",
                    h3(t("footer_info", LANG)),
                    p(t("footer_project_type", LANG)),
                    p(t("footer_status", LANG))
                )
            ),
            div(class = "footer-bottom",
                p(paste0("© ", format(Sys.Date(), "%Y"), 
                         " 6/49 ", t("footer_copyright", LANG), " | ",
                         strong(t("footer_for_edu", LANG)), " | ",
                         t("footer_play_resp", LANG), " | ", 
                         t("footer_no_services", LANG), " | ",
                         t("footer_under_const", LANG)))
            )
        )
    )
  )
}

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {
  

  # Call input module
  input_controls <- lotteryInputServer("inputs1")
  
  # Call modules
  dashboardServer("dashboard1", input_controls = input_controls)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$health)) {
      session$sendCustomMessage("health", "ok")
    }
  })
  
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server, enableBookmarking = "url")