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
  "stringr"
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