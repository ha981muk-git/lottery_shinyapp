# --- Production-ready Shiny options for DigitalOcean ---
options(
  shiny.maxRequestSize = 10*1024^2,
  shiny.sanitize.errors = TRUE,
  shiny.reactlog = FALSE,
  shiny.autoreload = FALSE,
  shiny.enableBookmarking = "disable",
  shiny.session.timeout = 3600,
  repos = c(CRAN = "https://cloud.r-project.org/"),
  shiny.error = function(e) {
    message("[Shiny Error] ", Sys.time(), " - ", e$message)
    stop("An unexpected error occurred. Please contact support.")
  },
  shiny.fullstacktrace = FALSE,
  mc.cores = max(1, parallel::detectCores() - 2)
)

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
  preset  = "shiny",
  bg      = "#0a0e27",
  fg      = "#e8eaed",
  primary = "#8b5cf6",
  secondary = "#ec4899",
  success = "#10b981",
  warning = "#f59e0b",
  danger  = "#ef4444",
  base_font    = font_google("Inter"),
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

# Load metric files from dashboard folder
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
# UI - LET CSS HANDLE RESPONSIVE, NOT JAVASCRIPT
# ============================================================================
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  
  fluidPage(
    theme = app_theme,
    
    tags$head(
      # SEO META TAGS
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=5"),
      tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
      tags$meta(name = "description", content = "6/49 Lotto-Analyse Tool - Kostenlos, interaktiv, bildungsbasiert."),
      tags$meta(name = "keywords", content = "Lotto Analyse, 6/49, Lotto 6 aus 49, Zahlenanalyse, Statistik"),
      tags$meta(name = "author", content = "Lottery Insights"),
      tags$meta(name = "robots", content = "index, follow"),
      tags$meta(name = "language", content = if(LANG == "de") "de" else "en"),
      tags$meta(name = "google-site-verification", content="93NjvZejo4MrJUkJ3RHuJo-_W6a3tdTAfvswURS3bbU"),
      
      tags$meta(property = "og:title", content = "6/49 Lotto-Analyse Tool"),
      tags$meta(property = "og:description", content = "Kostenloses, interaktives Bildungs-Dashboard"),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = "https://lotteryinsights.dpdns.org/"),
      
      tags$link(rel = "canonical", href = "https://lotteryinsights.dpdns.org/"),
      tags$link(rel = "alternate", hreflang = "de", href = "https://lotteryinsights.dpdns.org/?lang=de"),
      tags$link(rel = "alternate", hreflang = "en", href = "https://lotteryinsights.dpdns.org/?lang=en"),
      
      tags$script(type = "application/ld+json", HTML('
        {
          "@context": "https://schema.org",
          "@type": "WebApplication",
          "name": "6/49 Lotto-Analyse Tool",
          "url": "https://lotteryinsights.dpdns.org/",
          "applicationCategory": "EducationalApplication",
          "inLanguage": "de"
        }
      ')),
      
      tags$link(rel = "icon", type = "image/svg+xml", href = "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🎲</text></svg>"),
      
      useShinyjs(),
      use_waiter(),
      
      # ✅ MINIMAL JAVASCRIPT - ONLY FOR DRAWER OPEN/CLOSE
      tags$script(HTML("
        (function() {
          document.addEventListener('DOMContentLoaded', function() {
            var btnNav = document.getElementById('open-nav');
            var btnFilters = document.getElementById('open-filters');
            var backdrop = document.querySelector('.drawer-backdrop');
            var navDrawer = document.querySelector('.nav-drawer');
            var filtersDrawer = document.querySelector('.filters-drawer');
            
            function closeAll() {
              if (navDrawer) navDrawer.classList.remove('open');
              if (filtersDrawer) filtersDrawer.classList.remove('open');
              if (backdrop) backdrop.classList.remove('active');
              
              // Resize plots
              setTimeout(function() {
                window.dispatchEvent(new Event('resize'));
                if (window.Plotly) {
                  document.querySelectorAll('.js-plotly-plot').forEach(function(p) {
                    try { Plotly.Plots.resize(p); } catch(e) {}
                  });
                }
              }, 350);
            }
            
            if (btnNav) {
              btnNav.onclick = function() {
                if (navDrawer) navDrawer.classList.add('open');
                if (filtersDrawer) filtersDrawer.classList.remove('open');
                if (backdrop) backdrop.classList.add('active');
              };
            }
            
            if (btnFilters) {
              btnFilters.onclick = function() {
                if (filtersDrawer) filtersDrawer.classList.add('open');
                if (navDrawer) navDrawer.classList.remove('open');
                if (backdrop) backdrop.classList.add('active');
              };
            }
            
            if (backdrop) backdrop.onclick = closeAll;
            
            document.querySelectorAll('.drawer-close, .drawer-content a, .lang-btn').forEach(function(el) {
              el.addEventListener('click', closeAll);
            });
            
            // Language switcher
            var langSwitch = document.getElementById('lang-switch');
            if (langSwitch) {
              langSwitch.onchange = function() {
                var url = new URL(window.location.href);
                url.searchParams.set('lang', this.value);
                window.location.assign(url);
              };
            }
          });
          
          // Resize plots on window resize
          var resizeTimer;
          window.addEventListener('resize', function() {
            clearTimeout(resizeTimer);
            resizeTimer = setTimeout(function() {
              if (window.Plotly) {
                document.querySelectorAll('.js-plotly-plot').forEach(function(p) {
                  try { Plotly.Plots.resize(p); } catch(e) {}
                });
              }
            }, 250);
          });
        })();
      "))
    ),
    
    # Backdrop
    div(class = "drawer-backdrop"),
    
    # ==================== DESKTOP HEADER ====================
    div(class = "desktop-header",
        div(class = "desktop-logo",
            span("🎲", class = "desktop-logo-icon"),
            div(class = "desktop-logo-text",
                h1(t("title", LANG),
                   span(class = "testing-badge", t("testing_badge", LANG))),
                p(t("subtitle", LANG))
            )
        ),
        div(class = "desktop-nav",
            a(href = "#", t("nav_home", LANG)),
            a(href = "#analyzer", t("nav_analyzer", LANG)),
            a(href = "#educational", t("nav_educational", LANG)),
            a(href = "#disclaimer", t("nav_disclaimer", LANG)),
            div(class = "desktop-lang",
                tags$a(href = "?lang=de", 
                       class = paste0("lang-pill", if(LANG=="de") " active" else ""),
                       "🇩🇪 DE"),
                tags$a(href = "?lang=en",
                       class = paste0("lang-pill", if(LANG=="en") " active" else ""),
                       "🇬🇧 EN")
            )
        )
    ),
    
    # ==================== MOBILE APP BAR ====================
    div(class = "mobile-app-bar",
        tags$button(id = "open-nav", class = "icon-btn", "☰"),
        div(class = "mobile-title",
            div(t("title", LANG))
        ),
        div(class = "mobile-actions",
            tags$select(
              id = "lang-switch",
              class = "lang-select",
              tags$option(value = "de", selected = if(LANG == "de") NA else NULL, "DE"),
              tags$option(value = "en", selected = if(LANG == "en") NA else NULL, "EN")
            ),
            tags$button(id = "open-filters", class = "icon-btn", "⚙")
        )
    ),
    
    # ==================== NAV DRAWER (Mobile Only) ====================
    div(class = "nav-drawer",
        div(class = "drawer-content",
            tags$button(class="drawer-close", "✕"),
            h3(if (LANG=="de") "Navigation" else "Navigation"),
            tags$ul(
              tags$li(a(href="#", paste0("🏠 ", t("nav_home", LANG)))),
              tags$li(a(href="#analyzer", paste0("📊 ", t("nav_analyzer", LANG)))),
              tags$li(a(href="#educational", paste0("📚 ", t("nav_educational", LANG)))),
              tags$li(a(href="#disclaimer", paste0("⚠️ ", t("nav_disclaimer", LANG))))
            ),
            h3(if (LANG=="de") "Sprache" else "Language"),
            div(class = "drawer-lang",
                tags$a(href = "?lang=de", 
                       class = paste0("lang-btn", if(LANG=="de") " active" else ""),
                       "🇩🇪 DE"),
                tags$a(href = "?lang=en",
                       class = paste0("lang-btn", if(LANG=="en") " active" else ""),
                       "🇬🇧 EN")
            )
        )
    ),
    
    # ==================== FILTERS DRAWER (Mobile Only) ====================
    div(class = "filters-drawer",
        div(class = "drawer-content",
            tags$button(class="drawer-close", "✕"),
            h3(if (LANG=="de") "Analyseeinstellungen" else "Analysis Settings"),
            # ✅ Same input module will render here on mobile via CSS
            div(id = "mobile-filters-wrapper")
        )
    ),
    
    # ==================== MAIN CONTENT ====================
    div(class = "main-content",
        div(id = "analyzer", role = "region",
            # ✅ Single layout_sidebar - CSS handles responsive behavior
            layout_sidebar(
              sidebar = sidebar(
                id = "main-sidebar",
                class = "control-panel",
                position = "left",
                open = "always",  # Always render, CSS will position it
                h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #e8eaed;"),
                lotteryInputUI("inputs1", lang = LANG)
              ),
              # Main dashboard content
              div(style = "padding: 0;",
                  dashboardUI("dashboard1")
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
        
        # Educational Section
        div(id = "educational", role = "region",
            style = "margin-top: 40px; padding: 30px; background: rgba(255,255,255,0.03); border-radius: 12px;",
            h2(t("edu_title", LANG), style = "color: #e8eaed;"),
            p(style = "color: rgba(255,255,255,0.7); line-height: 1.8;",
              t("edu_intro", LANG)),
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
    
    # ==================== FOOTER ====================
    div(class = "professional-footer", role = "contentinfo",
        div(class = "footer-content",
            div(class = "footer-sections",
                div(class = "footer-section",
                    h3(t("footer_about", LANG)),
                    p(strong(t("footer_edu_only", LANG))),
                    p(t("footer_desc", LANG)),
                    p(style = "color: #ffc107; font-weight: 600;",
                      t("footer_construction", LANG))
                ),
                div(class = "footer-section",
                    h3(t("footer_quick", LANG)),
                    tags$ul(
                      tags$li(a(href = "#", t("nav_home", LANG))),
                      tags$li(a(href = "#analyzer", t("nav_analyzer", LANG))),
                      tags$li(a(href = "#educational", t("nav_educational", LANG))),
                      tags$li(a(href = "#disclaimer", t("nav_disclaimer", LANG)))
                    )
                ),
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
                div(class = "footer-section",
                    h3(t("footer_info", LANG)),
                    p(t("footer_project_type", LANG)),
                    p(t("footer_status", LANG))
                )
            ),
            div(class = "footer-bottom",
                p(paste0("© ", format(Sys.Date(), "%Y"),
                         " 6/49 ", t("footer_copyright", LANG), " | ",
                         t("footer_for_edu", LANG), " | ",
                         t("footer_play_resp", LANG)))
            )
        )
    )
  )
}

# ============================================================================
# Server - SIMPLE, NO SPECIAL MOBILE LOGIC
# ============================================================================
server <- function(input, output, session) {
  # Single input module - works everywhere
  input_controls <- lotteryInputServer("inputs1")
  
  # Dashboard uses same inputs
  dashboardServer("dashboard1", input_controls = input_controls)
  
  # Health check endpoint
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$health)) {
      session$sendCustomMessage("health", "ok")
    }
  })
  
  observeEvent(input$trigger_relayout, {
    # No action required
  })
}

# Run app
shinyApp(ui = ui, server = server, enableBookmarking = "url")