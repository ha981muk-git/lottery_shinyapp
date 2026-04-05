library(readr)
library(vroom)
library(shiny)
library(dplyr)
library(janitor)
library(bslib)
library(shinyjs)
library(plotly)
library(waiter)
library(tidyr)
library(purrr)
library(DT)
library(cachem)
library(digest)

# ✅ Load translations
source("translations.R")

# ---------- UI helper theme ----------
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#061423",
  fg = "#f4f8fb",
  primary = "#0ea5e9",
  secondary = "#f59e0b",
  success = "#10b981",
  warning = "#f97316",
  danger = "#ef4444",
  base_font = font_google("Manrope"),
  heading_font = font_google("Space Grotesk")
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

# Anonymous visitor counter.
# Uses CountAPI (remote, durable across deployments) with local RDS fallback.
create_visitor_counter <- function(
  store_path = file.path(script_folder, "data", "visitor_daily_counts.rds"),
  keep_days = 120,
  namespace = Sys.getenv("VISITOR_COUNTER_NAMESPACE", unset = "lottery-insights-v2"),
  api_base = Sys.getenv("VISITOR_COUNTER_API_BASE", unset = "https://api.countapi.xyz")
) {
  counter <- new.env(parent = emptyenv())
  counter$store_path <- normalizePath(store_path, mustWork = FALSE)
  counter$memory_store <- NULL
  counter$use_file <- FALSE
  counter$remote_failures <- 0L
  counter$remote_disabled <- FALSE
  counter$remote_warned <- FALSE

  normalize_namespace <- function(x) {
    ns <- tolower(trimws(as.character(x %||% "")))
    ns <- gsub("[^a-z0-9_-]", "-", ns)
    ns <- gsub("-{2,}", "-", ns)
    ns <- gsub("(^-|-$)", "", ns)
    if (!nzchar(ns)) ns <- "lottery-insights-v2"
    substr(ns, 1, 64)
  }

  counter$namespace <- normalize_namespace(namespace)
  counter$api_base <- sub("/+$", "", as.character(api_base))
  counter$id_salt <- Sys.getenv("VISITOR_COUNTER_SALT", unset = "lottery-insights-v2")
  counter$use_remote <- nzchar(counter$namespace) && grepl("^https?://", counter$api_base)

  if (isTRUE(counter$use_remote)) {
    message("Visitor counter backend: remote namespace '", counter$namespace, "'")
  }

  as_store_v2 <- function(raw_store) {
    empty_store <- list(version = 2L, total_count = 0L, daily_ids = list())

    if (is.null(raw_store) || length(raw_store) == 0) {
      return(empty_store)
    }

    if (is.list(raw_store) && !is.null(raw_store$daily_ids)) {
      daily_ids <- raw_store$daily_ids
      if (!is.list(daily_ids)) daily_ids <- list()
      total_count <- suppressWarnings(as.integer(raw_store$total_count[[1]]))
      if (is.na(total_count) || total_count < 0L) total_count <- 0L
      return(list(version = 2L, total_count = total_count, daily_ids = daily_ids))
    }

    if (is.list(raw_store)) {
      daily_ids <- raw_store
      daily_counts <- vapply(
        daily_ids,
        function(ids) length(unique(as.character(ids))),
        integer(1)
      )
      total_count <- as.integer(sum(daily_counts, na.rm = TRUE))
      return(list(version = 2L, total_count = total_count, daily_ids = daily_ids))
    }

    empty_store
  }
  
  store_dir <- dirname(counter$store_path)
  if (!dir.exists(store_dir)) {
    dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (dir.exists(store_dir) && file.access(store_dir, 2) == 0) {
    if (!file.exists(counter$store_path)) {
      try(saveRDS(list(), counter$store_path), silent = TRUE)
    }
    counter$use_file <- file.exists(counter$store_path)
  }
  
  prune_store <- function(store) {
    if (!is.list(store$daily_ids) || length(store$daily_ids) == 0) return(store)
    day_names <- names(store$daily_ids)
    if (is.null(day_names)) {
      store$daily_ids <- list()
      return(store)
    }
    days <- as.Date(day_names, format = "%Y-%m-%d")
    keep_from <- Sys.Date() - as.difftime(keep_days, units = "days")
    valid <- !is.na(days) & days >= keep_from
    store$daily_ids <- store$daily_ids[valid]
    store
  }

  read_store <- function() {
    if (isTRUE(counter$use_file)) {
      loaded <- try(readRDS(counter$store_path), silent = TRUE)
      if (!inherits(loaded, "try-error")) {
        return(prune_store(as_store_v2(loaded)))
      }
      counter$use_file <- FALSE
    }

    if (is.null(counter$memory_store)) {
      counter$memory_store <- as_store_v2(list())
    }
    prune_store(counter$memory_store)
  }
  
  write_store <- function(store) {
    store <- prune_store(as_store_v2(store))
    if (isTRUE(counter$use_file)) {
      saved <- try(saveRDS(store, counter$store_path), silent = TRUE)
      if (!inherits(saved, "try-error")) {
        return(invisible(TRUE))
      }
      counter$use_file <- FALSE
    }
    counter$memory_store <- store
    invisible(TRUE)
  }

  remote_enabled <- function() {
    isTRUE(counter$use_remote) && !isTRUE(counter$remote_disabled)
  }

  mark_remote_failure <- function() {
    counter$remote_failures <- counter$remote_failures + 1L
    if (counter$remote_failures >= 3L) {
      counter$remote_disabled <- TRUE
      if (!isTRUE(counter$remote_warned)) {
        message("Visitor counter remote backend unavailable; using local fallback.")
        counter$remote_warned <- TRUE
      }
    }
  }

  mark_remote_success <- function() {
    counter$remote_failures <- 0L
  }

  build_remote_url <- function(action, key) {
    paste0(
      counter$api_base, "/", action, "/",
      URLencode(counter$namespace, reserved = TRUE), "/",
      URLencode(key, reserved = TRUE)
    )
  }

  parse_remote_value <- function(response) {
    if (is.null(response) || inherits(response, "try-error") || httr::http_error(response)) {
      return(NA_integer_)
    }
    payload <- try(httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8"), silent = TRUE)
    if (inherits(payload, "try-error") || is.null(payload$value)) {
      return(NA_integer_)
    }
    value <- suppressWarnings(as.integer(payload$value))
    if (is.na(value) || value < 0L) return(NA_integer_)
    value
  }

  remote_get <- function(key) {
    if (!remote_enabled()) return(NA_integer_)
    response <- try(httr::GET(build_remote_url("get", key), httr::timeout(1.2)), silent = TRUE)
    value <- parse_remote_value(response)
    if (is.na(value)) {
      mark_remote_failure()
    } else {
      mark_remote_success()
    }
    value
  }

  remote_hit <- function(key) {
    if (!remote_enabled()) return(NA_integer_)
    response <- try(httr::GET(build_remote_url("hit", key), httr::timeout(1.2)), silent = TRUE)
    value <- parse_remote_value(response)
    if (is.na(value)) {
      mark_remote_failure()
    } else {
      mark_remote_success()
    }
    value
  }
  
  sanitize_id <- function(visitor_id) {
    if (is.null(visitor_id) || length(visitor_id) == 0) return(NULL)

    raw_id <- NULL
    if (is.list(visitor_id) && !is.null(visitor_id$id)) {
      raw_id <- visitor_id$id
    } else if (is.atomic(visitor_id) && !is.null(names(visitor_id)) && "id" %in% names(visitor_id)) {
      raw_id <- visitor_id[["id"]]
    } else {
      raw_id <- visitor_id[[1]]
    }

    if (is.null(raw_id) || length(raw_id) == 0) return(NULL)

    id <- as.character(raw_id[[1]])
    id <- gsub("[^A-Za-z0-9_-]", "", id)
    if (!nzchar(id) || nchar(id) < 8) return(NULL)
    substr(id, 1, 64)
  }

  day_key <- function(day) {
    gsub("[^0-9]", "", as.character(day))
  }

  local_count <- function(day = as.character(Sys.Date())) {
    store <- read_store()
    ids <- store$daily_ids[[day]]
    if (is.null(ids)) return(0L)
    as.integer(length(unique(as.character(ids))))
  }

  local_total <- function() {
    store <- read_store()
    total_count <- suppressWarnings(as.integer(store$total_count[[1]]))
    if (is.na(total_count) || total_count < 0L) return(0L)
    total_count
  }
  
  count <- function(day = as.character(Sys.Date())) {
    remote_daily <- remote_get(paste0("daily_", day_key(day)))
    if (!is.na(remote_daily)) return(remote_daily)
    local_count(day)
  }

  total <- function() {
    remote_total <- remote_get("total_all_days")
    if (!is.na(remote_total)) return(remote_total)
    local_total()
  }
  
  register <- function(visitor_id, day = as.character(Sys.Date())) {
    id <- sanitize_id(visitor_id)
    if (is.null(id)) {
      return(list(daily = count(day), total = total()))
    }

    if (remote_enabled()) {
      seen_hash <- substr(digest::digest(paste(day, id, counter$id_salt, sep = "|"), algo = "sha256"), 1, 24)
      seen_key <- paste0("seen_", day_key(day), "_", seen_hash)
      seen_value <- remote_hit(seen_key)

      if (!is.na(seen_value)) {
        if (seen_value == 1L) {
          daily_value <- remote_hit(paste0("daily_", day_key(day)))
          total_value <- remote_hit("total_all_days")
          if (is.na(daily_value)) daily_value <- local_count(day)
          if (is.na(total_value)) total_value <- local_total()
          return(list(daily = daily_value, total = total_value))
        }

        return(list(
          daily = count(day),
          total = total()
        ))
      }
    }

    store <- read_store()
    if (is.null(store$daily_ids[[day]])) {
      store$daily_ids[[day]] <- character()
    }
    if (!(id %in% store$daily_ids[[day]])) {
      store$daily_ids[[day]] <- unique(c(store$daily_ids[[day]], id))
      store$total_count <- as.integer(store$total_count + 1L)
      write_store(store)
    }
    list(
      daily = as.integer(length(store$daily_ids[[day]])),
      total = as.integer(store$total_count)
    )
  }
  
  list(register = register, count = count, total = total)
}

visitor_counter <- create_visitor_counter()

# ============================================================================
# UI - SEPARATE, with language parameter
ui <- function(request) {
  # ✅ Get language from URL or default to German
  query <- parseQueryString(request$QUERY_STRING)
  LANG <- query$lang %||% "de"
  
  fluidPage(
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
                       span(class = "testing-badge", t("testing_badge", LANG))),
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
        # Conversion Hero
        div(id = "home", class = "conversion-hero", role = "region", `aria-label` = if(LANG == "de") "Startbereich" else "Home section",
            div(class = "conversion-hero-main",
                h2(if(LANG == "de") "Erkenne Lotto-Muster in Sekunden" else "Spot Lottery Patterns in Seconds"),
                p(
                  if(LANG == "de") {
                    "Interaktive, schnelle und transparente Datenanalyse für 6/49. Kein Hype, keine Versprechen - nur Statistik, die man verstehen kann."
                  } else {
                    "Interactive, fast, and transparent 6/49 data analysis. No hype, no promises - only statistics you can understand."
                  }
                ),
                div(class = "hero-action-row",
                    a(
                      href = "#analyzer",
                      class = "hero-cta hero-cta-primary",
                      if(LANG == "de") "Jetzt analysieren" else "Start Analyzing"
                    ),
                    tags$button(
                      type = "button",
                      class = "hero-cta hero-cta-secondary share-app-btn",
                      `data-share-label` = if(LANG == "de") "Analyse teilen" else "Share Analysis",
                      `data-copied-label` = if(LANG == "de") "Link kopiert" else "Link Copied",
                      `data-fallback-label` = if(LANG == "de") "Link manuell kopieren" else "Copy Link Manually",
                      if(LANG == "de") "Analyse teilen" else "Share Analysis"
                    )
                ),
                p(
                  class = "hero-micro-note",
                  if(LANG == "de") {
                    "Tipp: Wenn dir das Tool hilft, teile den Link in deiner Community."
                  } else {
                    "Tip: If this tool helps, share the link with your community."
                  }
                )
            ),
            div(class = "hero-proof-grid",
                div(class = "hero-proof-card",
                    div(class = "hero-proof-value", "13,983,816"),
                    div(class = "hero-proof-label", if(LANG == "de") "Mögliche 6/49 Kombinationen" else "Possible 6/49 combinations")
                ),
                div(class = "hero-proof-card",
                    div(class = "hero-proof-value", textOutput("heroLatestDate")),
                    div(class = "hero-proof-label", if(LANG == "de") "Letzte verfügbare Ziehung" else "Latest available draw")
                ),
                div(class = "hero-proof-card",
                  div(class = "hero-proof-value", textOutput("heroTotalVisitors")),
                  div(class = "hero-proof-label", if(LANG == "de") "Anonyme Besucher gesamt" else "Anonymous visitors total")
                ),
                div(class = "hero-proof-card",
                  div(class = "hero-proof-value", textOutput("heroTodayVisitors")),
                  div(class = "hero-proof-label", if(LANG == "de") "Anonyme Besucher heute" else "Anonymous visitors today")
                )
            )
        ),
        
        # Live Insight Strip
        div(class = "insight-strip", role = "region", `aria-label` = if(LANG == "de") "Tages-Insights" else "Daily insights",
            div(class = "insight-card",
                div(class = "insight-title", if(LANG == "de") "Ø Summe (letzte 100)" else "Avg Sum (last 100)"),
                div(class = "insight-value", textOutput("heroAvgSum"))
            ),
            div(class = "insight-card",
                div(class = "insight-title", if(LANG == "de") "Häufigste Spannweite" else "Most common range"),
                div(class = "insight-value", textOutput("heroMostCommonRange"))
            ),
            div(class = "insight-card",
                div(class = "insight-title", if(LANG == "de") "Datenzeitraum" else "Data timespan"),
                div(class = "insight-value", textOutput("heroSpanYears"))
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
                h3(t("analysis_settings", LANG), style = "margin-top: 0; color: #e8eaed;"),
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
                    p(t("footer_desc", LANG)),
                    p(style = "color: #ffc107; font-weight: 600;", 
                      t("footer_construction", LANG))
                ),
                # Quick Links
                div(class = "footer-section",
                    h3(t("footer_quick", LANG)),
                    tags$ul(
                      tags$li(a(href = "#home", t("nav_home", LANG))),
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
  today_visitors_current <- reactiveVal(visitor_counter$count())
  total_visitors_current <- reactiveVal(visitor_counter$total())

  observeEvent(input$visitor_token, {
    counts <- visitor_counter$register(input$visitor_token)
    today_visitors_current(counts$daily)
    total_visitors_current(counts$total)
  }, ignoreInit = FALSE)
  
  visitor_counts <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      paste(visitor_counter$count(), visitor_counter$total(), sep = ":")
    },
    valueFunc = function() {
      list(
        daily = visitor_counter$count(),
        total = visitor_counter$total()
      )
    }
  )

  observeEvent(visitor_counts(), {
    counts <- visitor_counts()
    today_visitors_current(counts$daily)
    total_visitors_current(counts$total)
  }, ignoreInit = TRUE)
  
  growth_stats <- tryCatch({
    data <- generate_metrics()
    if (is.null(data) || nrow(data) == 0) stop("No data")
    
    sums <- rowSums(data[, paste0("ball_", 1:6)], na.rm = TRUE)
    recent_sums <- tail(sums, min(100, length(sums)))
    ranges <- data$ball_6 - data$ball_1
    range_mode <- as.numeric(names(sort(table(ranges), decreasing = TRUE))[1])
    
    latest_date <- suppressWarnings(max(data$datum, na.rm = TRUE))
    earliest_date <- suppressWarnings(min(data$datum, na.rm = TRUE))
    has_valid_dates <- inherits(latest_date, "Date") && !is.na(latest_date) &&
      inherits(earliest_date, "Date") && !is.na(earliest_date)
    
    list(
      latest_date = if (has_valid_dates) format(latest_date, "%d %b %Y") else "-",
      avg_sum = round(mean(recent_sums, na.rm = TRUE), 1),
      most_common_range = if (!is.na(range_mode)) range_mode else "-",
      span_years = if (has_valid_dates) {
        paste0(format(earliest_date, "%Y"), " - ", format(latest_date, "%Y"))
      } else {
        "-"
      }
    )
  }, error = function(e) {
    list(
      latest_date = "-",
      avg_sum = "-",
      most_common_range = "-",
      span_years = "-"
    )
  })
  
  output$heroLatestDate <- renderText(growth_stats$latest_date)
  output$heroAvgSum <- renderText(growth_stats$avg_sum)
  output$heroMostCommonRange <- renderText(growth_stats$most_common_range)
  output$heroSpanYears <- renderText(growth_stats$span_years)
  output$heroTotalVisitors <- renderText(format(total_visitors_current(), big.mark = ","))
  output$heroTodayVisitors <- renderText(format(today_visitors_current(), big.mark = ","))
  
  # Call input module
  input_controls <- lotteryInputServer("inputs1")
  
  # Call modules
  dashboardServer("dashboard1", input_controls = input_controls)
}

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server, enableBookmarking = "url")