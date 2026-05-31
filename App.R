library(shiny)
library(dplyr)
library(bslib)
library(shinyjs)
library(plotly)
library(waiter)
library(tidyr)
library(DT)

# ✅ Load translations
source("translations.R")

# ---------- UI helper theme ----------
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#fdf8f1",
  fg = "#2f2720",
  primary = "#c96b3b",
  secondary = "#b1916a",
  success = "#4e8a63",
  warning = "#d39b45",
  danger = "#ba5d4c",
  base_font = font_google("Instrument Sans"),
  heading_font = font_google("Instrument Sans")
)

# Source main files
script_folder <- "."

main_files <- c(
  "PrepareData.R",
  "DashboardModule.R"
)
lapply(main_files, function(f) {
  file_path <- file.path(script_folder, f)
  if (file.exists(file_path)) {
    tryCatch(source(file_path), error = function(e) {
      warning(paste("Failed to source:", f, "-", e$message))
    })
  } else {
    warning(paste("File not found:", f))
  }
})

metric_files <- list.files(
  path = file.path(script_folder, "dashboard"),
  pattern = "\\.R$",
  full.names = TRUE
)
lapply(metric_files, function(f) {
  tryCatch(source(f), error = function(e) {
    warning(paste("Failed to source metric file:", f, "-", e$message))
  })
})

# ============================================================================
# UI - SEPARATE, with language parameter
ui <- function(request) {
  # ✅ Get language from URL or default to German
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  app_base_url <- "https://lottery-insights.shinyapps.io/lottery_shinyapp_v2/"
  og_locale <- if (identical(LANG, "de")) "de_DE" else "en_US"
  feedback_form_url <- trimws(Sys.getenv("APP_FEEDBACK_FORM_URL", unset = ""))
  support_email <- trimws(Sys.getenv("APP_SUPPORT_EMAIL", unset = ""))
  newsletter_url <- trimws(Sys.getenv("APP_NEWSLETTER_URL", unset = ""))
  ga4_measurement_id <- trimws(Sys.getenv("APP_GA4_MEASUREMENT_ID", unset = ""))
  if (!nzchar(ga4_measurement_id)) {
    ga4_measurement_id <- "G-46CYMW6T38"
  }

  seo_title <- t("seo_title", LANG)
  seo_description <- t("seo_description", LANG)
  seo_keywords <- t("seo_keywords", LANG)
  og_title <- t("seo_og_title", LANG)
  og_description <- t("seo_og_description", LANG)
  schema_name <- t("seo_schema_name", LANG)
  schema_alt_name <- t("seo_schema_alt_name", LANG)
  schema_json <- sprintf('{
    "@context": "https://schema.org",
    "@type": "WebApplication",
    "name": "%s",
    "alternateName": "%s",
    "description": "%s",
    "url": "%s",
    "applicationCategory": "EducationalApplication",
    "inLanguage": "%s",
    "offers": {
      "@type": "Offer",
      "price": "0",
      "priceCurrency": "EUR"
    },
    "creator": {
      "@type": "Organization",
      "name": "Lottery Insights"
    }
  }',
  gsub('"', '\\"', schema_name, fixed = TRUE),
  gsub('"', '\\"', schema_alt_name, fixed = TRUE),
  gsub('"', '\\"', seo_description, fixed = TRUE),
  app_base_url,
  LANG
  )

  feedback_subject <- if (LANG == "de") {
    "Fehlerbericht / Feedback - 6/49 Analyse"
  } else {
    "Bug Report / Feedback - 6/49 Analysis"
  }

  feedback_body <- if (LANG == "de") {
    paste(
      "Bitte beschreibe den Fehler oder dein Feedback:",
      "",
      "Seite/Modul:",
      "Browser/Geraet:",
      "Schritte zur Reproduktion:",
      "1)",
      "2)",
      "",
      "Erwartetes Verhalten:",
      "",
      "Tatsaechliches Verhalten:",
      "",
      sep = "\n"
    )
  } else {
    paste(
      "Please describe the bug or your feedback:",
      "",
      "Page/Module:",
      "Browser/Device:",
      "Steps to reproduce:",
      "1)",
      "2)",
      "",
      "Expected behavior:",
      "",
      "Actual behavior:",
      "",
      sep = "\n"
    )
  }

  feedback_href <- if (nzchar(feedback_form_url)) {
    feedback_form_url
  } else if (nzchar(support_email)) {
    paste0(
      "mailto:",
      support_email,
      "?subject=", URLencode(feedback_subject, reserved = TRUE),
      "&body=", URLencode(feedback_body, reserved = TRUE)
    )
  } else {
    "#disclaimer"
  }

  feedback_target <- if (nzchar(feedback_form_url)) "_blank" else NULL
  feedback_rel <- if (nzchar(feedback_form_url)) "noopener noreferrer" else NULL

  lead_capture_href <- if (nzchar(newsletter_url)) {
    newsletter_url
  } else if (nzchar(feedback_form_url)) {
    feedback_form_url
  } else {
    "#analyzer"
  }

  lead_capture_target <- if (grepl("^https?://", lead_capture_href, ignore.case = TRUE)) "_blank" else NULL
  lead_capture_rel <- if (identical(lead_capture_target, "_blank")) "noopener noreferrer" else NULL
  has_direct_lead_capture <- nzchar(newsletter_url) || nzchar(feedback_form_url)
  growth_badge_key <- if (has_direct_lead_capture) "growth_badge" else "growth_badge_fallback"
  growth_subtitle_key <- if (has_direct_lead_capture) "growth_subtitle" else "growth_subtitle_fallback"
  growth_cta_primary_key <- if (has_direct_lead_capture) "growth_cta_primary" else "growth_cta_primary_fallback"
  growth_sticky_cta_key <- if (has_direct_lead_capture) "growth_sticky_cta" else "growth_sticky_cta_fallback"

  privacy_href <- paste0("privacy.html?lang=", LANG)
  terms_href <- paste0("terms.html?lang=", LANG)
  disclaimer_href <- paste0("disclaimer.html?lang=", LANG)
  methodology_href <- paste0("methodology.html?lang=", LANG)
  
  fluidPage(
    theme = app_theme,
    
    tags$head(
      tags$title(seo_title),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = seo_description),
      tags$meta(name = "keywords", content = seo_keywords),
      tags$meta(name = "author", content = "Lottery Insights"),
      tags$meta(name = "robots", content = "index, follow"),
      tags$meta(name = "language", content = LANG),
      tags$meta(name = "geo.placename", content = "Deutschland"),
      tags$meta(name = "geo.region", content = "DE"),
      tags$meta(name = "google-site-verification", content = "SCaDZ-eWJCu14j6urMNGER1iqoqwf_1imzwnm5PjMeo"),

      tags$meta(property = "og:title", content = og_title),
      tags$meta(property = "og:description", content = og_description),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = app_base_url),
      tags$meta(property = "og:locale", content = og_locale),
      tags$link(rel = "canonical", href = app_base_url),
      tags$link(rel = "alternate", hreflang = "de", href = paste0(app_base_url, "?lang=de")),
      tags$link(rel = "alternate", hreflang = "en", href = paste0(app_base_url, "?lang=en")),
      tags$link(rel = "alternate", hreflang = "x-default", href = app_base_url),
      tags$script(type = "application/ld+json", HTML(schema_json)),
      tags$link(rel = "icon", type = "image/svg+xml", href = "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🎲</text></svg>"),

      if (nzchar(ga4_measurement_id)) tags$script(
        async = NA,
        src = paste0("https://www.googletagmanager.com/gtag/js?id=", URLencode(ga4_measurement_id, reserved = TRUE))
      ),
      if (nzchar(ga4_measurement_id)) tags$script(HTML(
        "window.dataLayer = window.dataLayer || [];\nwindow.gtag = window.gtag || function(){ window.dataLayer.push(arguments); };\nwindow.gtag('js', new Date());\nwindow.gtag('consent', 'default', { analytics_storage: 'denied' });"
      )),

      tags$script(HTML(sprintf(
        "window.liAppConfig = {lang: '%s', ga4MeasurementId: '%s'};",
        gsub("'", "", LANG, fixed = TRUE),
        gsub("'", "", ga4_measurement_id, fixed = TRUE)
      ))),
      tags$link(rel = "stylesheet", type = "text/css", href = "Home.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js"),
      
      useShinyjs(),
      use_waiter()
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
                       span(class = "testing-badge", t("trust_badge", LANG))),
                    p(t("subtitle", LANG))
                )
            ),
            div(class = "header-nav", role = "navigation",
              a(href = "#home", t("nav_home", LANG)),
                a(href = "#analyzer", t("nav_analyzer", LANG)),
                a(href = "#educational", t("nav_educational", LANG)),
                a(href = "#disclaimer", t("nav_disclaimer", LANG))
            )
        )
    ),
    
    # Main Content
    div(class = "main-content",
        div(id = "home"),

        div(
          class = "growth-hero",
          div(class = "growth-hero-content",
              div(class = "growth-hero-copy",
                  div(class = "growth-badge", t(growth_badge_key, LANG)),
                  h2(t("growth_title", LANG)),
                  p(t(growth_subtitle_key, LANG)),
                  tags$ul(
                    class = "growth-points",
                    tags$li(t("growth_point_1", LANG)),
                    tags$li(t("growth_point_2", LANG)),
                    tags$li(t("growth_point_3", LANG))
                  ),
                  div(
                    class = "growth-actions",
                    a(
                      href = lead_capture_href,
                      target = lead_capture_target,
                      rel = lead_capture_rel,
                      class = "btn btn-primary growth-cta-primary",
                      `data-analytics-event` = "lead_capture_click",
                      t(growth_cta_primary_key, LANG)
                    ),
                    a(
                      href = "#analyzer",
                      class = "btn btn-outline-light growth-cta-secondary",
                      `data-analytics-event` = "hero_explore_click",
                      t("growth_cta_secondary", LANG)
                    )
                  )
              ),
              div(class = "growth-hero-trust",
                  h3(t("growth_trust_title", LANG)),
                  tags$ul(
                    tags$li(t("growth_trust_1", LANG)),
                    tags$li(t("growth_trust_2", LANG)),
                    tags$li(t("growth_trust_3", LANG))
                  )
              )
          )
        ),
        
        # Main Analyzer Section
        div(id = "analyzer", role = "region", `aria-label` = if(LANG == "de") "Analyse-Dashboard" else "Analysis Dashboard",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                class = "control-panel",
                open = "desktop",
                position = "left",
                max_height_mobile = NULL,
                h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #3a2e24;"),
                lotteryInputUI("inputs1", lang = LANG)
              ),
              # Main content
              div(
                style = "padding: 0; min-height: 100vh;",
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
            p(style = "margin-top: 15px; font-style: italic; color: rgba(78, 63, 50, 0.75);",
              t("notice_purpose", LANG))
        ),
        
        # Additional Educational Section
        div(id = "educational", role = "region", `aria-label` = if(LANG == "de") "Bildungsinformationen" else "Educational Information",
            style = "margin-top: 40px; padding: 30px; border-radius: 18px; border: 1px solid rgba(126, 95, 66, 0.14); background: linear-gradient(145deg, rgba(255, 255, 255, 0.78), rgba(251, 243, 231, 0.9)); box-shadow: 0 18px 30px rgba(82, 56, 34, 0.08);",
            h2(t("edu_title", LANG), style = "color: #33271f;"),
            p(style = "color: rgba(73, 59, 47, 0.86); line-height: 1.8;",
              t("edu_intro", LANG)
            ),
            h3(t("edu_objectives", LANG), style = "color: #33271f; margin-top: 20px;"),
            tags$ul(
              style = "color: rgba(73, 59, 47, 0.86); line-height: 1.8;",
              tags$li(t("edu_obj_1", LANG)),
              tags$li(t("edu_obj_2", LANG)),
              tags$li(t("edu_obj_3", LANG)),
              tags$li(t("edu_obj_4", LANG))
            )
        ),
        
        # FAQ + Trust Section
        div(id = "disclaimer", class = "faq-section", role = "region", `aria-label` = if(LANG == "de") "Häufige Fragen" else "Frequently asked questions",
            h2(if(LANG == "de") "Häufige Fragen" else "Frequently Asked Questions"),
            p(
              class = "faq-intro",
              if(LANG == "de") {
                "Diese Plattform ist ein Bildungsprojekt und hilft, Datenmuster besser zu verstehen."
              } else {
                "This platform is an educational project designed to help users understand data patterns."
              }
            ),
            div(class = "faq-list",
                tags$details(
                  class = "faq-item",
                  tags$summary(if(LANG == "de") "Kann dieses Tool Lottozahlen vorhersagen?" else "Can this tool predict lottery numbers?"),
                  p(if(LANG == "de") "Nein. Das Tool zeigt nur historische Muster und statistische Verteilungen. Jede Ziehung bleibt zufällig." else "No. This tool only shows historical patterns and statistical distributions. Every draw remains random.")
                ),
                tags$details(
                  class = "faq-item",
                  tags$summary(if(LANG == "de") "Wie oft werden die Daten aktualisiert?" else "How often is the data updated?"),
                  p(if(LANG == "de") "Die Datenbasis wird regelmäßig aktualisiert. Prüfen Sie die neuesten Ziehungen im Dashboard." else "The dataset is updated regularly. Check the dashboard for the latest available draws.")
                ),
                tags$details(
                  class = "faq-item",
                  tags$summary(if(LANG == "de") "Wie kann ich das Projekt unterstützen?" else "How can I support this project?"),
                  p(if(LANG == "de") "Teilen Sie den Link mit Freunden, in Foren oder in Lern-Communities. So helfen Sie, mehr Menschen zu erreichen." else "Share the link with friends, forums, or learning communities. That helps the project reach more people.")
                )
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
                    p(t("footer_desc", LANG))
                ),
                # Quick Links
                div(class = "footer-section",
                    h3(t("footer_quick", LANG)),
                    tags$ul(
                      tags$li(a(href = "#home", t("nav_home", LANG))),
                      tags$li(a(href = "#analyzer", t("nav_analyzer", LANG))),
                      tags$li(a(href = "#educational", t("nav_educational", LANG))),
                      tags$li(a(href = "#disclaimer", t("nav_disclaimer", LANG))),
                      tags$li(
                        class = "footer-feedback-item",
                        a(
                          href = feedback_href,
                          target = feedback_target,
                          rel = feedback_rel,
                          class = "footer-feedback-link",
                          `data-analytics-event` = "feedback_click",
                          span(class = "feedback-link-icon", "🐞"),
                          span(t("footer_report_bug", LANG))
                        )
                      )
                    )
                ),
                # Legal & Disclaimer
                div(class = "footer-section",
                    h3(t("footer_legal", LANG)),
                    tags$ul(
                      tags$li(a(href = disclaimer_href, target = "_blank", rel = "noopener noreferrer", t("footer_full_disclaimer", LANG))),
                      tags$li(a(href = privacy_href, target = "_blank", rel = "noopener noreferrer", t("footer_privacy", LANG))),
                      tags$li(a(href = terms_href, target = "_blank", rel = "noopener noreferrer", t("footer_terms", LANG))),
                      tags$li(a(href = methodology_href, target = "_blank", rel = "noopener noreferrer", t("footer_edu_statement", LANG)))
                    ),
                    p(style = "color: #df8b72; font-size: 0.85em; margin-top: 10px;",
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
                         t("footer_no_services", LANG)))
            )
        )
    ),
    div(
      id = "consentBanner",
      class = "consent-banner",
      `aria-live` = "polite",
      div(
        class = "consent-content",
        p(t("consent_text", LANG)),
        div(
          class = "consent-actions",
          tags$button(id = "consentReject", class = "btn btn-outline-light btn-sm", t("consent_reject", LANG)),
          tags$button(id = "consentAccept", class = "btn btn-primary btn-sm", t("consent_accept", LANG))
        )
      )
    ),
    a(
      href = lead_capture_href,
      target = lead_capture_target,
      rel = lead_capture_rel,
      class = "sticky-lead-cta",
      `data-analytics-event` = "sticky_lead_click",
      t(growth_sticky_cta_key, LANG)
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
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server, enableBookmarking = "url")