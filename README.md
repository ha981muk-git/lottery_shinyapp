# Lottery Insights Dashboard

A modular R Shiny application for educational analysis of LOTTO 6aus49 historical draw data.
It focuses on fast filtering, interactive metric views, bilingual UX, and production-friendly refresh and telemetry behavior.

## Highlights

- Six analysis modules: balls, sums, odd/even, table, difference, and lag.
- Bright, responsive interface with a sidebar-driven workflow, skeleton loading, and fullscreen chart/table cards.
- Fast interactions through debounced filters and lazy metric server initialization.
- Shareable deep links that preserve language, metric, date range, and number range.
- Bilingual content (`de` and `en`) via the shared translation helper `t(key, lang)`.
- Automatic data refresh from WestLotto with safe fallback to local data.
- Consent-aware frontend analytics events (GA4).

## Project Layout

```text
lottery_shinyapp_v2/
├── App.R                         # Entry point, theme, top-level UI, env-driven links
├── PrepareData.R                 # Data loader, refresh scheduler, CSV/RDS pipeline
├── DashboardModule.R             # Input controls, share-link flow, dashboard orchestration
├── translations.R                # Text dictionary for de/en and translation helper
├── dashboard/
│   ├── ballsMetric.R             # Ball frequency and trend analysis
│   ├── sumsMetric.R              # Sum distribution analysis
│   ├── oddsEvensMetric.R         # Odd/even analysis
│   ├── tableMetric.R             # Detailed table view
│   ├── differenceMetric.R        # Ball_6 - Ball_1 spread analysis
│   └── lagMetric.R               # Sequential lag/jump analysis
├── www/
│   ├── custom.css                # Primary style overrides
│   ├── custom.js                 # Consent, analytics events, client UX helpers
│   ├── Home.css                  # Base page styling
│   ├── privacy.html              # Static legal page
│   ├── terms.html                # Static legal page
│   ├── disclaimer.html           # Static legal page
│   └── methodology.html          # Static educational methodology page
└── tests/testthat/test_performance.R
```

## Local Setup

### 1) Prerequisites

- R 4.0+ (R 4.3+ recommended)
- Internet access for optional package restore and optional remote data refresh

### 2) Restore dependencies

Preferred (repo-managed):

```bash
Rscript -e "renv::restore(prompt = FALSE)"
```

If you are not using `renv`, install required packages manually:

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "plotly", "DT", "dplyr", "tidyr",
  "purrr", "readr", "vroom", "janitor", "waiter", "httr"
))
```

### 3) Run the app

```bash
Rscript -e "shiny::runApp('.', host = '127.0.0.1', port = 4242, launch.browser = FALSE)"
```

Or from an interactive R session:

```r
shiny::runApp('.')
```

## How the App Works

### Data loading and refresh

`PrepareData.R` builds a loader that:

- Reads and cleans `data/LOTTO_ab_2018.csv`.
- Reuses `data/LOTTO_clean.rds` when possible.
- Checks refresh timing via `data/LOTTO_refresh_meta.rds`.
- Downloads fresh data from `https://www.westlotto.de/wlinfo/WL_InfoService` when due.
- Falls back safely to existing local data if remote refresh fails.

### Dashboard interaction model

`DashboardModule.R` handles:

- Debounced slider and date inputs.
- Quick date presets (3m, 6m, 1y, all).
- Inclusive date filtering (`datum >= from & datum <= to`).
- Share-link generation from current view state.
- Lazy metric initialization to speed first paint.

### Localization

- Language is driven by URL parameter (`?lang=de` or `?lang=en`).
- All user-facing text should come from `t(key, lang)`.
- Add new keys to both `en` and `de` dictionaries in `translations.R`.

## Environment Variables

### Data refresh controls

- `R_CONFIG_ACTIVE`: deployment profile (`shinyapps` toggles startup defaults).
- `LOTTO_AUTO_REFRESH_ENABLED`: enable periodic refresh (`true` default).
- `LOTTO_FORCE_REFRESH_ON_STARTUP`: on shinyapps, defaults to `true`; elsewhere `false`.
- `LOTTO_AUTO_REFRESH_DAYS`: target refresh cadence in days (`14` default).
- `LOTTO_AUTO_REFRESH_TOLERANCE_DAYS`: day-window jitter around cadence (`5` default).
- `LOTTO_DATA_YEAR_FROM`: download lower year bound (`2018` default).
- `LOTTO_DATA_YEAR_TO`: download upper year bound (defaults to current year).

### Feedback, support, and lead capture

- `APP_FEEDBACK_FORM_URL`: when set, footer feedback opens this URL in a new tab.
- `APP_SUPPORT_EMAIL`: used for footer feedback mailto fallback when no form URL is set.
- `APP_NEWSLETTER_URL`: when set, hero/sticky "Get updates" CTA opens this external URL.

Current CTA behavior:

- If `APP_NEWSLETTER_URL` exists, the updates CTA opens it (new tab).
- Otherwise, if `APP_FEEDBACK_FORM_URL` exists, the updates CTA opens that form (new tab).
- If neither is set, the hero/sticky CTA switches to free-analyzer copy and routes to `#analyzer`.

### Analytics and consent

- `APP_GA4_MEASUREMENT_ID`: GA4 property ID.
- If `APP_GA4_MEASUREMENT_ID` is empty, the app currently falls back to built-in ID `G-46CYMW6T38`.
- Events are sent only after consent is accepted in the banner.
- Main tracked events include metric switch, filter change, refresh click, scroll depth, outbound click, and share-link copy.

### Short `.env.example`

Use safe placeholders for local setup and keep real production values out of version control.

```env
# App profile
R_CONFIG_ACTIVE=local

# Data refresh
LOTTO_AUTO_REFRESH_ENABLED=true
LOTTO_FORCE_REFRESH_ON_STARTUP=false
LOTTO_AUTO_REFRESH_DAYS=14
LOTTO_AUTO_REFRESH_TOLERANCE_DAYS=5
LOTTO_DATA_YEAR_FROM=2018
LOTTO_DATA_YEAR_TO=2026

# Feedback and growth
APP_FEEDBACK_FORM_URL=https://example.com/feedback
APP_SUPPORT_EMAIL=support@example.com
APP_NEWSLETTER_URL=https://example.com/newsletter

# Analytics
APP_GA4_MEASUREMENT_ID=G-XXXXXXXXXX
```

## Testing and Performance Checks

Run quick script benchmarks:

```bash
Rscript test.R
```

Run the shinytest2 performance script:

```bash
Rscript -e "source('tests/testthat/test_performance.R')"
```

Run the responsive pre-deploy regression check (5 breakpoints + focus/touch-target sweep):

```bash
NOT_CRAN=true Rscript -e "source('tests/testthat/test_responsive_regression.R')"
```

This check fails fast when it detects horizontal overflow, header/language overlap,
off-screen sidebar toggle placement, missing focus indicator styles, or touch targets
smaller than 44x44 for language and consent actions.

## Deployment Notes

- Avoid committing runtime artifacts such as:
  - `data/LOTTO_refresh_meta.rds`
  - `.RData`, `.Rhistory`, `.Rproj.user/`
- Keep legal static pages in sync with app links:
  - `www/privacy.html`
  - `www/terms.html`
  - `www/disclaimer.html`
  - `www/methodology.html`

## Contributing

1. Fork the repository.
2. Create a branch for your change.
3. Commit with a clear message.
4. Open a pull request with a concise summary and testing notes.

## License

MIT License.