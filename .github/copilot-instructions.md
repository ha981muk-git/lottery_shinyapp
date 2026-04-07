# Project Guidelines

## Code Style
- Keep code modular with the existing Shiny module pattern: `nameUI(id)` and `nameServer(id, ...)`.
- Reuse shared UI helpers from `DashboardModule.R` (for example `create_chart_card`, `create_table_card`, `render_title`) instead of duplicating card/header logic.
- Keep translation usage consistent: all user-facing strings should go through `t(key, lang)` and new keys must be added to both `en` and `de` in `translations.R`.
- Prefer explicit namespaces for HTTP calls (`httr::GET`, `httr::timeout`, etc.) instead of attaching `httr` with `library(httr)` to avoid masking conflicts.
- Preserve the existing reactive performance style: debounce/throttle expensive inputs and gate heavy outputs with `req()`.

## Architecture
- Entry point is `App.R`, which sources `PrepareData.R`, `DashboardModule.R`, and `dashboard/*.R`.
- Data loading and refresh logic lives in `PrepareData.R` (`create_data_loader()`, `generate_metrics()`).
- Input controls and app-level filtering live in `DashboardModule.R` (`lotteryInputServer()`, `dashboardServer()`).
- Metric modules under `dashboard/` should focus on rendering/analysis for `filtered_data()` and avoid cross-module side effects.
- Keep date filtering inclusive (`datum >= from & datum <= to`) when changing filter logic.

## Build and Test
- Restore dependencies with renv before running or testing:
  - `Rscript -e "renv::restore(prompt = FALSE)"`
- Run locally from repo root:
  - `Rscript -e "shiny::runApp('.', launch.browser = FALSE)"`
- Optional performance checks:
  - `Rscript test.R`
  - `Rscript -e "source('tests/testthat/test_performance.R')"`

## Conventions
- Keep the primary date input id as `inputs1-dateRange` (not `inputs1-timeRange`).
- For output suspension behavior, prefer calling `outputOptions(...)` inside `session$onFlushed(..., once = TRUE)` and guard with `try(..., silent = TRUE)`.
- Do not commit or rely on ephemeral runtime artifacts (`data/visitor_daily_counts.rds`, `data/LOTTO_refresh_meta.rds`, `.RData`, `.Rhistory`, `.Rproj.user/`).
- Respect environment-driven behavior for deployment (`R_CONFIG_ACTIVE`, `VISITOR_COUNTER_*`, `LOTTO_AUTO_REFRESH_*`, `APP_SUPPORT_EMAIL`, `APP_FEEDBACK_FORM_URL`) and do not hardcode secrets.

## Documentation
- Use `README.md` as the source of truth for detailed setup, feature descriptions, deployment notes, localization, visitor counter behavior, and refresh strategy.