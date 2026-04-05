# Lottery Analytics Dashboard 🎰

A high-performance, interactive R Shiny application designed for advanced statistical analysis of lottery draw history. This dashboard provides deep insights into number patterns, frequencies, and statistical anomalies using modern visualization techniques.

## 🚀 Features

### 📊 Advanced Analytics Modules
*   **Ball Frequency & Trends:** Analyze individual ball occurrences, coverage, and probability using Violin plots, Raincloud plots, and Box plots.
*   **Difference (Range) Analysis:** Statistical breakdown of the spread between the first and last drawn numbers (Ball 6 - Ball 1), including heatmaps and density distributions.
*   **Lag Analysis:** Study the "jump" or difference between consecutive draws to identify sequential patterns.
*   **Sums & Parity:** Analysis of sum totals and Odd/Even distributions.

### ⚡ Performance & Architecture
*   **Global Caching:** Implements `cachem` for server-side memory caching, ensuring instant response times for shared filters across concurrent users.
*   **Optimized Rendering:** Uses WebGL for heavy scatter plots and debounced inputs to minimize server load.
*   **Modular Design:** Built using Shiny Modules for scalability and maintainability.
*   **Lazy Loading:** Metrics are initialized on-demand to speed up the initial application load.

### 🎨 UI/UX Experience
*   **Dark Mode Theme:** A sleek, modern dark interface designed for long analysis sessions.
*   **Interactive Charts:** Fully interactive Plotly graphs with zoom, pan, and hover details.
*   **Fullscreen Mode:** Toggle any chart or table to fullscreen for detailed inspection.
*   **Responsive Layout:** Adapts to different screen sizes using `bslib` and custom CSS.
*   **Skeleton Loaders:** Visual feedback during data processing.
*   **Visitor Trust Metrics:** Shows both total anonymous visitors (all-time) and anonymous visitors today.

## 🛠️ Installation

### Prerequisites
Ensure you have R installed (version 4.0+ recommended).

### Required Packages
Run the following R command to install necessary dependencies:

```r
install.packages(c(
  "shiny",
  "plotly",
  "dplyr",
  "tidyr",
  "DT",
  "shinyjs",
  "bslib",
  "cachem",
  "zoo",
  "stringr",
  "httr",
  "digest"
))
```

## 📂 Project Structure

```text
lottery_shinyapp_v2/
├── DashboardModule.R     # Core UI/Server logic, Helpers & Global Cache
├── dashboard/
│   ├── ballsMetric.R     # Ball frequency analysis module
│   ├── differenceMetric.R# Range/Difference analysis module
│   ├── lagMetric.R       # Lag/Jump analysis module
│   ├── sumsMetric.R      # Sums analysis module
│   ├── oddsEvensMetric.R # Parity analysis module
│   └── tableMetric.R     # Raw data table module
└── README.md             # Project documentation
```

## 🖥️ Usage

1.  **Clone the repository** or download the source code.
2.  **Open the project in RStudio.**
3.  **Run the App:**
    ```r
    shiny::runApp()
    ```

## ⚙️ Technical Details

### Caching Strategy
The application uses a global memory cache (`global_filter_cache`) defined in `DashboardModule.R`. This allows filtered datasets (e.g., "Last 30 Weeks") to be shared across all active user sessions, significantly reducing RAM usage and CPU load on the server.

### Localization
The app supports dynamic localization (defaulting to German `de`). Text elements are rendered using a helper function `t(key, lang)` which looks up strings based on the user's selected language or URL parameters.

### Automatic Lottery Data Refresh
The app can automatically refresh LOTTO 6aus49 draw data from the same backend used by the Sachsenlotto download page.

How it works:
*   On app startup / first data load, it checks whether the local file is older than the refresh window.
*   If stale, it downloads the latest ZIP archive from `https://www.westlotto.de/wlinfo/WL_InfoService`.
*   It extracts the CSV, replaces `data/LOTTO_ab_2018.csv`, deletes stale `data/LOTTO_clean.rds`, and rebuilds clean data automatically.
*   If download fails, the app keeps using existing local data (safe fallback).

Environment variables:
*   `LOTTO_AUTO_REFRESH_ENABLED`: Enable/disable auto-refresh (`true` by default).
*   `LOTTO_AUTO_REFRESH_DAYS`: Refresh interval in days (`14` by default).
*   `LOTTO_AUTO_REFRESH_TOLERANCE_DAYS`: Allowed timing tolerance in days (`5` by default).
*   `LOTTO_DATA_YEAR_FROM`: Start year for download query (`2018` by default).
*   `LOTTO_DATA_YEAR_TO`: End year for download query (defaults to current year).

Recommended production setup:
*   Keep `LOTTO_AUTO_REFRESH_ENABLED=true`.
*   Keep `LOTTO_AUTO_REFRESH_DAYS=14` for two-week updates.
*   Set `LOTTO_AUTO_REFRESH_TOLERANCE_DAYS` to `3` to `5` to allow natural timing drift.
*   If you need exact clock-time scheduling (for example every second Monday at 02:00), trigger a small external scheduled job that starts the app or runs a refresh script.

### Persistent Visitor Counter
The app tracks anonymous visitors in two ways:
*   **Anonymous visitors today**: Daily unique visitors, deduplicated by browser token per day.
*   **Anonymous visitors total**: Cumulative all-time counter.

Persistence strategy:
*   Primary backend is CountAPI (remote), which survives app restarts and new deployments.
*   If remote backend is unreachable, the app falls back to local RDS storage.

Recommended deployment settings (keep these stable across all redeployments):
*   `VISITOR_COUNTER_NAMESPACE`: Unique namespace for this app (for example: `lottery-insights-prod-v1`).
*   `VISITOR_COUNTER_SALT`: A private random string used to hash daily visitor IDs.
*   `VISITOR_COUNTER_API_BASE`: Optional override, defaults to `https://api.countapi.xyz`.
*   `VISITOR_COUNTER_BASELINE_TOTAL`: Optional real historical baseline added to total visitors (default is `523` from analytics snapshot).
*   `VISITOR_COUNTER_REMOTE_TIMEOUT_SEC`: Remote request timeout in seconds (default `4`).
*   `VISITOR_COUNTER_REMOTE_FAILURE_THRESHOLD`: Consecutive failures before short cooldown (default `8`).
*   `VISITOR_COUNTER_REMOTE_COOLDOWN_SEC`: Cooldown duration before retrying remote backend (default `45`).
*   `VISITOR_COUNTER_ALLOW_FILE_FALLBACK`: Enable local file fallback (`false` on shinyapps, `true` elsewhere by default).

Important:
*   Do not change `VISITOR_COUNTER_NAMESPACE` between deployments if you want the same historical total.
*   If remote backend is blocked by network policy, counts continue locally but may not persist across redeployments.

### Custom UI Components
*   **`create_chart_card`**: A wrapper function that standardizes chart containers, adding titles, descriptions, and the fullscreen toggle functionality.
*   **`create_stat_card`**: Standardized KPI cards for the top of metric views.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1.  Fork the project
2.  Create your feature branch (`git checkout -b feature/AmazingFeature`)
3.  Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4.  Push to the branch (`git push origin feature/AmazingFeature`)
5.  Open a Pull Request

## 📄 License

Distributed under the MIT License.