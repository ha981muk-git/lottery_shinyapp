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
  shiny.fullstacktrace = FALSE,
  
  mc.cores = max(1, parallel::detectCores() - 2)
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
# ============================================================================
# UI - SEPARATE, with language parameter
# ============================================================================
# UI - SEPARATE, with language parameter
# ============================================================================
# ============================================================================
# UI - COMPLETE FIXED VERSION
# ============================================================================
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  
  fluidPage(
    theme = app_theme,
    
    tags$head(
    # ==================== SEO META TAGS ====================
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
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
      
      # ✅ COMPLETE RESPONSIVE STYLES
      tags$style(HTML("
        /* Base animations */
        @keyframes shimmer {
          0% { background-position: -200% 0; }
          100% { background-position: 200% 0; }
        }
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(5px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        /* Desktop Header */
        .desktop-header {
          position: sticky;
          top: 0;
          z-index: 1100;
          background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
          color: white;
          padding: 20px 30px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.2);
          display: flex;
          align-items: center;
          justify-content: space-between;
        }
        
        .desktop-logo {
          display: flex;
          align-items: center;
          gap: 15px;
        }
        
        .desktop-logo-icon {
          font-size: 2.5rem;
        }
        
        .desktop-logo-text h1 {
          margin: 0;
          font-size: 1.5rem;
          font-weight: 600;
        }
        
        .desktop-logo-text p {
          margin: 5px 0 0;
          font-size: 0.9rem;
          opacity: 0.9;
        }
        
        .desktop-nav {
          display: flex;
          align-items: center;
          gap: 25px;
        }
        
        .desktop-nav a {
          color: white;
          text-decoration: none;
          font-weight: 500;
          transition: opacity 0.2s;
        }
        
        .desktop-nav a:hover {
          opacity: 0.8;
        }
        
        .desktop-lang {
          display: flex;
          gap: 8px;
          margin-left: 15px;
          padding-left: 15px;
          border-left: 1px solid rgba(255,255,255,0.3);
        }
        
        .lang-pill {
          padding: 6px 12px;
          border-radius: 20px;
          background: rgba(255,255,255,0.15);
          color: white;
          text-decoration: none;
          font-size: 0.85rem;
          font-weight: 600;
          transition: background 0.2s;
        }
        
        .lang-pill:hover {
          background: rgba(255,255,255,0.25);
        }
        
        .lang-pill.active {
          background: rgba(255,255,255,0.3);
        }
        
        .testing-badge {
          display: inline-block;
          background: #ff9800;
          color: white;
          padding: 4px 10px;
          border-radius: 12px;
          font-size: 0.7rem;
          font-weight: 700;
          margin-left: 10px;
        }
        
        /* Mobile App Bar */
        .mobile-app-bar {
          display: none;
          position: sticky;
          top: 0;
          z-index: 1100;
          background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
          color: white;
          padding: 12px 16px;
          align-items: center;
          gap: 12px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        }
        
        .mobile-title {
          flex: 1;
          font-size: 1rem;
          font-weight: 600;
          text-align: center;
          line-height: 1.3;
        }
        
        .icon-btn {
          background: rgba(255,255,255,0.15);
          border: none;
          color: white;
          padding: 10px;
          border-radius: 8px;
          cursor: pointer;
          font-size: 1.3rem;
          transition: background 0.2s;
          min-width: 44px;
          height: 44px;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .icon-btn:hover {
          background: rgba(255,255,255,0.25);
        }
        
        .mobile-actions {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .lang-select {
          background: rgba(255,255,255,0.18);
          border: none;
          color: #fff;
          padding: 8px 10px;
          border-radius: 6px;
          font-weight: 600;
          cursor: pointer;
          font-size: 0.9rem;
        }
        
        .lang-select option {
          color: #000;
          background: #fff;
        }
        
        /* Drawer Styles */
        .drawer {
          position: fixed;
          top: 0;
          bottom: 0;
          width: 85%;
          max-width: 320px;
          background: #1a1f3a;
          z-index: 1200;
          transform: translateX(-100%);
          transition: transform 0.3s ease;
          overflow-y: auto;
          box-shadow: 2px 0 10px rgba(0,0,0,0.3);
        }
        
        .drawer.drawer-right {
          right: 0;
          left: auto;
          transform: translateX(100%);
        }
        
        html.nav-open #nav-drawer {
          transform: translateX(0);
        }
        
        html.filters-open #filters-drawer {
          transform: translateX(0);
        }
        
        .drawer-content {
          padding: 20px;
          padding-top: 60px;
        }
        
        .drawer-close {
          position: absolute;
          top: 15px;
          right: 15px;
          background: rgba(255,255,255,0.1);
          border: none;
          color: white;
          padding: 8px 16px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 1.3rem;
          z-index: 10;
        }
        
        .drawer h3 {
          color: #8b5cf6;
          margin: 20px 0 15px;
          font-size: 1.1rem;
          font-weight: 600;
        }
        
        .drawer ul {
          list-style: none;
          padding: 0;
          margin: 0;
        }
        
        .drawer li {
          margin: 5px 0;
        }
        
        .drawer a {
          color: rgba(255,255,255,0.85);
          text-decoration: none;
          display: block;
          padding: 12px 15px;
          border-radius: 8px;
          transition: background 0.2s;
          font-size: 0.95rem;
        }
        
        .drawer a:hover {
          background: rgba(139,92,246,0.2);
          color: white;
        }
        
        .drawer-lang {
          display: flex;
          gap: 10px;
          margin-top: 10px;
        }
        
        .lang-btn {
          flex: 1;
          padding: 10px;
          text-align: center;
          background: rgba(255,255,255,0.08);
          color: white;
          text-decoration: none;
          border-radius: 8px;
          transition: background 0.2s;
          font-weight: 600;
        }
        
        .lang-btn.active {
          background: #8b5cf6;
        }
        
        .lang-btn:hover {
          background: rgba(139,92,246,0.4);
        }
        
        /* Backdrop */
        .drawer-backdrop {
          display: none;
          position: fixed;
          inset: 0;
          background: rgba(0,0,0,0.6);
          z-index: 1150;
          backdrop-filter: blur(2px);
        }
        
        html.nav-open .drawer-backdrop,
        html.filters-open .drawer-backdrop {
          display: block;
        }
        
        /* Desktop Sidebar visible */
        @media (min-width: 769px) {
          .mobile-app-bar { display: none !important; }
          .desktop-header { display: flex !important; }
          .drawer { display: none !important; }
          .drawer-backdrop { display: none !important; }
        }
        
        /* Mobile: hide desktop header, show mobile bar */
        @media (max-width: 768px) {
          .desktop-header { display: none !important; }
          .mobile-app-bar { display: flex !important; }
          
          /* Hide desktop sidebar on mobile */
          #sidebar-home-anchor > .sidebar {
            display: none !important;
          }
        }
        
        @media (prefers-reduced-motion: reduce) {
          * { animation: none !important; transition: none !important; }
        }
        
        /* CRITICAL FIX: Don't hide sidebar on mobile, let JS move it */
          @media (max-width: 768px) {
            /* Remove the display: none from your Home.css */
            #sidebar-home-anchor > .sidebar {
              display: block !important;
            }
            
            /* When sidebar is in filters drawer, ensure it's visible */
            #filters-container .sidebar {
              display: block !important;
              position: static !important;
              width: 100% !important;
              background: transparent !important;
            }
          }
      ")),
      
      # ✅ FIXED JAVASCRIPT - Aggressive sidebar reparenting
    tags$script(HTML("
  (function(){
    const html = document.documentElement;
    
    function openNav() {
      html.classList.add('nav-open');
      html.classList.remove('filters-open');
    }
    
    function openFilters() {
      html.classList.add('filters-open');
      html.classList.remove('nav-open');
    }
    
    function closeAll() {
      html.classList.remove('nav-open', 'filters-open');
    }
    
    function wireButtons() {
      const btnNav = document.getElementById('open-nav');
      const btnFilters = document.getElementById('open-filters');
      
      if (btnNav) {
        btnNav.removeEventListener('click', handleNavClick);
        btnNav.addEventListener('click', handleNavClick);
      }
      
      if (btnFilters) {
        btnFilters.removeEventListener('click', handleFiltersClick);
        btnFilters.addEventListener('click', handleFiltersClick);
      }
      
      document.querySelectorAll('.drawer-close').forEach(btn => {
        btn.removeEventListener('click', closeAll);
        btn.addEventListener('click', closeAll);
      });
      
      document.querySelectorAll('.drawer a, .lang-btn').forEach(link => {
        link.removeEventListener('click', closeAll);
        link.addEventListener('click', closeAll);
      });
      
      const backdrop = document.querySelector('.drawer-backdrop');
      if (backdrop) {
        backdrop.removeEventListener('click', closeAll);
        backdrop.addEventListener('click', closeAll);
      }
      
      const langSwitch = document.getElementById('lang-switch');
      if (langSwitch) {
        langSwitch.removeEventListener('change', handleLangChange);
        langSwitch.addEventListener('change', handleLangChange);
      }
    }
    
    function handleNavClick(e) {
      e.stopPropagation();
      openNav();
    }
    
    function handleFiltersClick(e) {
      e.stopPropagation();
      openFilters();
    }
    
    function handleLangChange() {
      const url = new URL(window.location.href);
      url.searchParams.set('lang', this.value);
      window.location.assign(url.toString());
    }
    
    // ✅ Initialize after DOM ready
    document.addEventListener('DOMContentLoaded', function() {
      wireButtons();
      
      // ✅ Move sidebar into mobile drawer if on small screen
      const sidebar = document.querySelector('#sidebar-home-anchor > .sidebar');
      const filtersContainer = document.querySelector('#filters-container');
      
      if (sidebar && filtersContainer && window.innerWidth <= 768) {
        filtersContainer.appendChild(sidebar);
      }
    });
    
  })();
")),
    
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
    
    # ==================== NAV DRAWER (Mobile) ====================
    div(id = "nav-drawer", class = "drawer drawer-left",
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
    
    # ==================== FILTERS DRAWER (Mobile) ====================
    div(id = "filters-drawer", class = "drawer drawer-right",
        div(class = "drawer-content",
            tags$button(class="drawer-close", "✕"),
            h3(if (LANG=="de") "Filter" else "Filters"),
            div(id = "filters-container")
        )
    ),
    
    # ==================== MAIN CONTENT ====================
    div(class = "main-content",
        div(id = "analyzer", role = "region",
            layout_sidebar(
              sidebar = sidebar(
                id = "sidebar-home-anchor",     # <- id on the actual sidebar
                class = "control-panel",
                position = "left",
                open = "desktop",
                max_height_mobile = NULL,
                h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #e8eaed;"),
                lotteryInputUI("inputs1", lang = LANG)
              ),
              div(style = "padding: 0; min-height: 100vh;",
                  dashboardUI("dashboard1")
              ),
              fillable = FALSE,
              border = FALSE,
              border_radius = FALSE
            )
        )
        ,
        
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
)}


# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {
  # Call input module
  input_controls <- lotteryInputServer("inputs1")
  
  # Call modules
  dashboardServer("dashboard1", input_controls = input_controls)
  
  # Health check endpoint
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
