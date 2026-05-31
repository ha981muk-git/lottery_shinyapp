# Responsive and accessibility regression checks.
# Run from repo root:
#   NOT_CRAN=true Rscript -e "source('tests/testthat/test_responsive_regression.R')"

suppressPackageStartupMessages(library(shinytest2))

breakpoints <- list(
  list(name = "desktop", width = 1366, height = 900),
  list(name = "laptop", width = 1024, height = 768),
  list(name = "tablet", width = 768, height = 1024),
  list(name = "mobile", width = 430, height = 932),
  list(name = "small_mobile", width = 360, height = 800)
)

collect_layout_metrics <- function(app) {
  app$get_js("(() => {
    const html = document.documentElement;
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;
    const isVisible = (el) => {
      if (!el) return false;
      const styles = getComputedStyle(el);
      return el.getClientRects().length > 0 && styles.display !== 'none' && styles.visibility !== 'hidden';
    };

    const nav = document.querySelector('.header-nav');
    const lang = document.querySelector('.lang-switcher');
    let navTouchesLang = false;

    if (isVisible(nav) && isVisible(lang)) {
      const navRect = nav.getBoundingClientRect();
      const langRect = lang.getBoundingClientRect();
      navTouchesLang = !(
        navRect.right <= langRect.left - 4 ||
        navRect.left >= langRect.right + 4 ||
        navRect.bottom <= langRect.top - 4 ||
        navRect.top >= langRect.bottom + 4
      );
    }

    const toggle = document.querySelector('.sidebar-toggle-btn, .bslib-sidebar-layout > .collapse-toggle');
    const toggleRect = toggle ? toggle.getBoundingClientRect() : null;

    return {
      viewportWidth,
      viewportHeight,
      scrollWidth: html.scrollWidth,
      overflowX: Math.max(0, html.scrollWidth - viewportWidth),
      navTouchesLang,
      toggleOffscreenLeft: !!toggleRect && toggleRect.left < -1
    };
  })()")
}

collect_accessibility_metrics <- function(app) {
  app$get_js("(() => {
    const selectorRules = [];
    const collectRules = (rules) => {
      if (!rules) return;
      for (const rule of Array.from(rules)) {
        let nestedRules = null;
        try {
          nestedRules = rule.cssRules || null;
        } catch (err) {
          nestedRules = null;
        }

        if (nestedRules && nestedRules.length > 0) {
          collectRules(nestedRules);
        } else if (rule.selectorText) {
          selectorRules.push(rule.selectorText);
        }
      }
    };

    for (const sheet of Array.from(document.styleSheets)) {
      try {
        collectRules(sheet.cssRules);
      } catch (err) {
        // Ignore cross-origin stylesheets.
      }
    }

    const hasFocusRule = (needle) =>
      selectorRules.some(
        (selector) =>
          (selector.includes(':focus-visible') || selector.includes(':focus')) &&
          selector.includes(needle)
      );

    const inspectTargets = (selector) =>
      Array.from(document.querySelectorAll(selector)).map((el) => {
        const rect = el.getBoundingClientRect();
        const styles = getComputedStyle(el);
        const minWidth = parseFloat(styles.minWidth) || 0;
        const minHeight = parseFloat(styles.minHeight) || 0;
        return {
          selector,
          label: (el.textContent || '').trim().slice(0, 24),
          width: Math.round(rect.width),
          height: Math.round(rect.height),
          minWidth,
          minHeight,
          touchOk: Math.max(rect.width, minWidth) >= 44 && Math.max(rect.height, minHeight) >= 44
        };
      });

    return {
      langFocusRule: hasFocusRule('.lang-btn'),
      consentAcceptFocusRule: hasFocusRule('#consentAccept'),
      consentRejectFocusRule: hasFocusRule('#consentReject'),
      langTargets: inspectTargets('.lang-btn'),
      consentTargets: inspectTargets('#consentAccept, #consentReject')
    };
  })()")
}

check_breakpoint <- function(app, bp) {
  app$set_window_size(bp$width, bp$height, wait = TRUE)
  app$wait_for_idle(timeout = 10000)
  Sys.sleep(0.35)
  collect_layout_metrics(app)
}

is_closed_session_error <- function(err) {
  grepl("Session and underlying target have been closed", conditionMessage(err), fixed = TRUE)
}

create_app_driver <- function(max_attempts = 4, load_timeouts = c(70000, 90000, 120000, 120000)) {
  if (length(load_timeouts) == 0) {
    load_timeouts <- 70000
  }

  for (attempt in seq_len(max_attempts)) {
    load_timeout <- load_timeouts[[min(attempt, length(load_timeouts))]]

    candidate <- tryCatch(
      {
        driver <- AppDriver$new(app_dir = ".", load_timeout = load_timeout, view = FALSE)
        driver$wait_for_idle(timeout = 12000)
        driver$get_js("1 + 1")
        driver
      },
      error = function(err) err
    )

    if (!inherits(candidate, "error")) {
      return(candidate)
    }

    if (attempt < max_attempts) {
      first_line <- strsplit(conditionMessage(candidate), "\n", fixed = TRUE)[[1]][1]
      cat(
        sprintf(
          "  AppDriver startup failed (attempt %d/%d, timeout=%dms): %s\n",
          attempt,
          max_attempts,
          load_timeout,
          first_line
        )
      )
      Sys.sleep(min(2, 0.5 * attempt))
    } else {
      stop(candidate)
    }
  }
}

reset_app_driver <- function(app) {
  try(app$stop(), silent = TRUE)
  create_app_driver(max_attempts = 3, load_timeouts = c(70000, 90000, 120000))
}

format_target <- function(target) {
  label <- target$label
  if (is.null(label) || identical(label, "")) {
    label <- target$selector
  }
  sprintf(
    "%s (%dx%d, min %.0fx%.0f)",
    label,
    as.integer(target$width),
    as.integer(target$height),
    as.numeric(target$minWidth),
    as.numeric(target$minHeight)
  )
}

cat("Running responsive regression checks...\n")

restore_env_var <- function(name, value) {
  if (is.na(value)) {
    Sys.unsetenv(name)
  } else {
    do.call(Sys.setenv, setNames(list(value), name))
  }
}

old_refresh_enabled <- Sys.getenv("LOTTO_AUTO_REFRESH_ENABLED", unset = NA_character_)
old_force_refresh <- Sys.getenv("LOTTO_FORCE_REFRESH_ON_STARTUP", unset = NA_character_)

# Keep test startup deterministic by skipping network-driven data refresh paths.
Sys.setenv(
  LOTTO_AUTO_REFRESH_ENABLED = "false",
  LOTTO_FORCE_REFRESH_ON_STARTUP = "false"
)

on.exit({
  restore_env_var("LOTTO_AUTO_REFRESH_ENABLED", old_refresh_enabled)
  restore_env_var("LOTTO_FORCE_REFRESH_ON_STARTUP", old_force_refresh)
}, add = TRUE)

failures <- character(0)
app <- create_app_driver(max_attempts = 4)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)

for (bp in breakpoints) {
  attempt <- 1
  repeat {
    metrics_or_error <- tryCatch(check_breakpoint(app, bp), error = function(err) err)

    if (!inherits(metrics_or_error, "error")) {
      metrics <- metrics_or_error
      break
    }

    if (is_closed_session_error(metrics_or_error) && attempt < 3) {
      cat(sprintf("  Browser session closed during '%s'; restarting and retrying...\n", bp$name))
      app <- reset_app_driver(app)
      attempt <- attempt + 1
      next
    }

    stop(metrics_or_error)
  }

  cat(
    sprintf(
      "- %s (%dx%d -> %dx%d): overflowX=%d, navTouchesLang=%s, toggleOffscreenLeft=%s\n",
      bp$name,
      bp$width,
      bp$height,
      as.integer(metrics$viewportWidth),
      as.integer(metrics$viewportHeight),
      as.integer(metrics$overflowX),
      ifelse(isTRUE(metrics$navTouchesLang), "true", "false"),
      ifelse(isTRUE(metrics$toggleOffscreenLeft), "true", "false")
    )
  )

  if (metrics$overflowX > 0) {
    failures <- c(
      failures,
      sprintf("%s: horizontal overflow detected (%dpx)", bp$name, as.integer(metrics$overflowX))
    )
  }

  if (isTRUE(metrics$navTouchesLang)) {
    failures <- c(failures, sprintf("%s: header navigation overlaps language switcher", bp$name))
  }

  if (isTRUE(metrics$toggleOffscreenLeft)) {
    failures <- c(failures, sprintf("%s: sidebar toggle is off-screen", bp$name))
  }
}

cat("Running accessibility sweep (focus + touch targets)...\n")
attempt <- 1
repeat {
  a11y_or_error <- tryCatch(
    {
      app$set_window_size(430, 932, wait = TRUE)
      app$wait_for_idle(timeout = 10000)
      Sys.sleep(0.35)
      collect_accessibility_metrics(app)
    },
    error = function(err) err
  )

  if (!inherits(a11y_or_error, "error")) {
    a11y <- a11y_or_error
    break
  }

  if (is_closed_session_error(a11y_or_error) && attempt < 3) {
    cat("  Browser session closed during accessibility sweep; restarting and retrying...\n")
    app <- reset_app_driver(app)
    attempt <- attempt + 1
    next
  }

  stop(a11y_or_error)
}

cat(
  sprintf(
    "- focus indicator rules: lang=%s, consentAccept=%s, consentReject=%s\n",
    ifelse(isTRUE(a11y$langFocusRule), "yes", "no"),
    ifelse(isTRUE(a11y$consentAcceptFocusRule), "yes", "no"),
    ifelse(isTRUE(a11y$consentRejectFocusRule), "yes", "no")
  )
)

if (!isTRUE(a11y$langFocusRule)) {
  failures <- c(failures, "Accessibility: missing focus indicator rule for .lang-btn")
}
if (!isTRUE(a11y$consentAcceptFocusRule)) {
  failures <- c(failures, "Accessibility: missing focus indicator rule for #consentAccept")
}
if (!isTRUE(a11y$consentRejectFocusRule)) {
  failures <- c(failures, "Accessibility: missing focus indicator rule for #consentReject")
}

if (length(a11y$langTargets) == 0) {
  failures <- c(failures, "Accessibility: no .lang-btn elements found")
} else {
  bad_lang <- Filter(function(target) !isTRUE(target$touchOk), a11y$langTargets)
  if (length(bad_lang) > 0) {
    failures <- c(
      failures,
      paste(
        "Accessibility: language switcher touch targets smaller than 44x44:",
        paste(vapply(bad_lang, format_target, character(1)), collapse = "; ")
      )
    )
  }
}

if (length(a11y$consentTargets) == 0) {
  failures <- c(failures, "Accessibility: no consent action buttons found")
} else {
  bad_consent <- Filter(function(target) !isTRUE(target$touchOk), a11y$consentTargets)
  if (length(bad_consent) > 0) {
    failures <- c(
      failures,
      paste(
        "Accessibility: consent action touch targets smaller than 44x44:",
        paste(vapply(bad_consent, format_target, character(1)), collapse = "; ")
      )
    )
  }
}

if (length(failures) > 0) {
  cat("\nResponsive pre-deploy check FAILED:\n")
  for (failure in failures) {
    cat("- ", failure, "\n", sep = "")
  }
  stop("Responsive regression check failed.")
}

cat("\nResponsive pre-deploy check PASSED.\n")
