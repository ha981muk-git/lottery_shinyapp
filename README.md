# 🎲 Lottery Insights - Statistical Visualization Platform

[![Status](https://img.shields.io/badge/status-testing-yellow)](https://github.com)
[![R
Version](https://img.shields.io/badge/R-%E2%89%A54.0-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-Educational-green)](LICENSE)

> **⚠️ EDUCATIONAL PURPOSE ONLY** - This is a testing and educational
> platform for demonstrating statistical analysis methods. No real
> lottery services, betting, or gambling features are provided.

## 📋 Overview

A professional Shiny application for analyzing German 6/49 lottery data
using advanced statistical methods. This platform demonstrates:

-   📊 Interactive data visualization
-   🔢 Probability theory and statistical distributions
-   📈 Time series analysis and pattern recognition
-   🎓 Educational approach to data science

## ✨ Features

### Core Analytics Modules

1.  **Balls Metric** - Comprehensive ball distribution analysis
2.  **Sums Analysis** - Sum patterns and trends over time
3.  **Odds/Evens** - Even/odd number distribution patterns
4.  **Frequency Table** - Hot/cold number tracking
5.  **Difference Analysis** - Ball range and spread patterns
6.  **Lag Analysis** - Number jump patterns between draws

### Authentication System

-   ✅ User registration with email verification
-   🔐 Secure password hashing (sodium)
-   🔄 Password reset functionality
-   📧 Email notifications system

### Subscription Management

-   💳 Stripe payment integration
-   📦 Three-tier plans (Free, Basic, Premium)
-   🔄 Subscription upgrades/downgrades
-   💾 SQLite database for user management

### Multi-language Support

-   🇩🇪 German (Deutsch)
-   🇬🇧 English
-   Dynamic UI translation system

## 🚀 Quick Start

### Prerequisites

-   R ≥ 4.0
-   RStudio (recommended)
-   SQLite
-   Stripe account (for payment features)

### Installation

1.  **Clone the repository**

``` bash
git clone <repository-url>
cd lottery_shinyapp_v2
```

2.  **Install R dependencies**

``` r
# The project uses renv for package management
renv::restore()
```

3.  **Configure environment variables**

``` bash
# Create .Renviron file
STRIPE_SECRET_KEY=your_stripe_secret_key
SENDER_EMAIL=your_email@example.com
SENDER_PASSWORD=your_email_password
SMTP_SERVER=smtp.gmail.com
SMTP_PORT=587
APP_URL=http://localhost:3838
```

4.  **Initialize database**

``` r
source("DatabaseManager.R")
init_database()
```

5.  **Run the application**

``` r
shiny::runApp()
```

## 📁 Project Structure

```         
lottery_shinyapp_v2/
├── app.R                          # Main application entry point
├── translations.R                 # Multi-language support
├── PrepareData.R                  # Data loading with caching
├── DashboardModule.R              # Main dashboard logic
│
├── Authentication System/
│   ├── AuthManager.R              # User authentication logic
│   ├── AuthenticationUI.R         # Auth UI components
│   ├── ShinyAuthManager.R         # Shiny auth modules
│   ├── EmailVerificationSystem.R  # Email verification
│   └── EnhancedAuthenticationSystem.R
│
├── Subscription System/
│   ├── SubscriptionManager.R      # Subscription logic
│   ├── ShinySubscriptionModules.R # Subscription UI
│   └── StripeManager.R            # Stripe integration
│
├── Database/
│   ├── DatabaseManager.R          # SQLite operations
│   └── lottery_users.db           # User database
│
├── Metrics Modules/
│   ├── dashboard/
│   │   ├── ballsMetric.R          # Ball distribution analysis
│   │   ├── sumsMetric.R           # Sum analysis
│   │   ├── oddsEvensMetric.R      # Odd/even patterns
│   │   ├── tableMetric.R          # Frequency analysis
│   │   ├── differenceMetric.R     # Range analysis
│   │   └── lagMetric.R            # Lag patterns
│
├── data/
│   └── LOTTO_ab_2018.csv          # Historical lottery data
│
├── www/
│   ├── Home.css                   # Application styles
│   ├── robots.txt
│   └── sitemap.xml
│
├── Deployment/
│   ├── Dockerfile                 # Docker configuration
│   ├── digitalocean.yaml          # DigitalOcean config
│   ├── shiny-server.conf          # Shiny server config
│   └── config.yaml                # App configuration
│
└── Documentation/
    ├── README.md                  # This file
    ├── SETUP_GUIDE.md             # Detailed setup guide
    └── requirements.R             # Package requirements
```

## 🔧 Configuration

### Database Schema

The app uses SQLite with three main tables:

1.  **users** - User accounts and authentication
2.  **subscriptions** - User subscription details
3.  **usage_logs** - Activity tracking

### Subscription Plans

``` r
Free Plan:
- Basic number analysis
- Limited historical data
- €0/month

Basic Plan:
- Advanced pattern detection
- Full historical data
- Export functionality
- €9.99/month

Premium Plan:
- Everything in Basic
- AI-powered insights
- API access
- €24.99/month
```

## 🎨 Key Technologies

-   **Backend**: R, Shiny
-   **Database**: SQLite (RSQLite)
-   **Authentication**: sodium (password hashing)
-   **Payments**: Stripe API
-   **Email**: mailR / SMTP
-   **UI Framework**: bslib (Bootstrap 5)
-   **Data Viz**: plotly, ggplot2
-   **Data Processing**: dplyr, vroom, tidyr

## 📊 Performance Optimizations

### L1 Cache - Filtered Data

-   LRU cache for filtered datasets
-   Keyed by: weeks + ball range
-   Max size: 15 entries

### L2 Cache - Computed Metrics

-   Global plot caching across sessions
-   Keyed by: data version + language + chart type
-   Max size: 60 entries per metric

### Features

-   ✅ Debounced user inputs (300ms)
-   ✅ Throttled refresh button (300ms)
-   ✅ Lazy module initialization
-   ✅ Empty state handling
-   ✅ Session cleanup on disconnect

## 🔐 Security Features

-   ✅ Password hashing with sodium
-   ✅ CSRF token validation
-   ✅ Rate limiting for auth attempts
-   ✅ SQL injection prevention (parameterized queries)
-   ✅ XSS protection (Shiny built-in)
-   ✅ Secure session management

## 🌐 Deployment

### DigitalOcean App Platform

``` yaml
# digitalocean.yaml
name: lottery-insights
services:
  - name: web
    dockerfile_path: Dockerfile
    http_port: 3838
```

### Docker

``` bash
# Build image
docker build -t lottery-insights .

# Run container
docker run -p 3838:3838 \
  -e STRIPE_SECRET_KEY=your_key \
  -e SENDER_EMAIL=your_email \
  lottery-insights
```

## 🧪 Testing

The project includes performance profiling:

``` r
# Run profiling
Rprof("profile.out")
# ... run app operations ...
Rprof(NULL)

# Analyze results
summaryRprof("profile.out")
```

## 📝 License

This project is for **educational purposes only**. No commercial
gambling services are provided.

## ⚠️ Disclaimer

**IMPORTANT NOTICE:** - This is a TESTING and EDUCATIONAL platform -
Demonstrates statistical analysis methods only - No real lottery
services provided - No gambling or betting features - Gambling can be
addictive - play responsibly - This site does NOT encourage gambling

## 🤝 Contributing

This is an educational project. Contributions focused on: - Statistical
methodology improvements - Code optimization - Documentation
enhancements - Bug fixes

## 📧 Contact

For educational or technical inquiries about this project's statistical
methods.

## 🔗 Resources

-   [R Shiny Documentation](https://shiny.rstudio.com/)
-   [Stripe API Documentation](https://stripe.com/docs/api)
-   [German Lottery Official Site](https://www.lotto.de/)

------------------------------------------------------------------------

**Built with R & Shiny** \| **For Educational Purposes** \| **© 2024**
\_\_\_

app.R file changed by Github to App.R by default : Need to know it for
Docker Step 1: Edit renv/settings.json Open the file renv/settings.json
in your text editor and change it to: json{ "use.cache": false,
"snapshot.type": "implicit"

# 1. Edit renv/settings.json (already done)

# 2. Clean up lockfile

R -e "renv::snapshot(type = 'implicit')" in Terminal

Ab January 2026 add prediction part, i) Steiner Triple System
<https://de.wikipedia.org/wiki/Fano-Ebene>
<https://marwahaha.github.io/steinersystems/>
<https://www.dmgordon.org/cover/> ii) from book archive ,,
<https://archive.org/details/combinatoriallot0000iliy> iii) set prepared
data previously, 25-sets, 15-sets 10-sets personally developed iv) add
more models

---
editor_options: 
  markdown: 
    wrap: 72
---

app.R file changed by Github to App.R by default : Need to know it for
Docker Step 1: Edit renv/settings.json Open the file renv/settings.json
in your text editor and change it to: json{ "use.cache": false,
"snapshot.type": "implicit"

# 1. Edit renv/settings.json (already done)

# 2. Clean up lockfile

R -e "renv::snapshot(type = 'implicit')" in Terminal

Ab January 2026 add prediction part, i) Steiner Triple System
<https://de.wikipedia.org/wiki/Fano-Ebene>
<https://marwahaha.github.io/steinersystems/>
<https://www.dmgordon.org/cover/> ii) from book archive
<https://archive.org/details/combinatoriallot0000iliy> iii) set prepared
data previously, 25-sets, 15-sets 10-sets personally developed iv) add
more models

Can be added later:

Interactive draw simulator: let visitors pick hypothetical number sets
and then simulate thousands of future draws (Monte Carlo) to show hit
probabilities, expected payout tiers, etc. People like experimenting
with “what if” scenarios, and it reinforces the educational message that
outcomes stay random.

Live results ingestion: integrate an automated fetcher (cron + API or
CSV scrape) so the dashboard updates within minutes of actual German
Lotto draws. Pair that with a “recent draws” timeline highlighting how
today’s numbers compare with historical averages.

Pattern playlists: curate preset “stories” (e.g., “Cold streak
breakers”, “Odd-heavy weeks”, “Top sums of 2024”) that jump users
directly to interesting metric combinations. Each playlist is a guided
walkthrough with annotations, almost like mini blog posts embedded in
the UI.

Localized insights: add a small bilingual blog section where you
summarize weekly insights (e.g., “Week 12: longest run without a 40+
number in two years”). These can be static markdown files, but previewed
in the app sidebar. Great for SEO and encourages return visits.

Exportable reports: let users export the current view as a PDF/PNG or
shareable link (with lang, metric, and range parameters). Sharing on
social channels can drive organic traffic and positions the tool as a
lightweight “data studio” for lotto enthusiasts.

Accessibility badges & performance metrics: surface Lighthouse scores or
“Green Hosting” badges to emphasize quality and trustworthiness. For
some users, that’s as compelling as features.

```         
###############################################################################
# HYBRID METAHEURISTIC OPTIMIZER (PARALLEL VERSION)
# GRASP + GA + Tabu + SA + Hill-Climbing + Beam Search (Parallel)
###############################################################################

library(future.apply)
library(dplyr)

plan(multicore)   # parallel on all cores

N <- 49
K <- 15

# --------------------------
# Load historical draws
# --------------------------
load_draws <- function(file = "lottery_draws.csv") {
  df <- read.csv(file)
  draws <- as.matrix(df[,2:7])
  draws
}

# --------------------------
# Frequency computation
# --------------------------
compute_frequencies <- function(draws) {
  freq <- rep(0, N)
  for (i in 1:nrow(draws)) {
    freq[ draws[i,] ] <- freq[ draws[i,] ] + 1
  }
  freq
}

# --------------------------
# Score subset
# --------------------------
score_subset <- function(subset, freq) sum(freq[subset])

# --------------------------
# GRASP
# --------------------------
generate_grasp <- function(freq, k = 5) {
  greedy <- order(freq, decreasing = TRUE)[1:k]
  remaining <- setdiff(1:N, greedy)
  probs <- freq[remaining] / sum(freq[remaining])
  rest <- sample(remaining, K-k, prob = probs)
  sort(c(greedy, rest))
}

# --------------------------
# Mutation
# --------------------------
mutate_subset <- function(sub) {
  m <- sample(1:3, 1)
  remove <- sample(sub, m)
  add <- sample(setdiff(1:N, sub), m)
  sort(c(setdiff(sub, remove), add))
}

# --------------------------
# Crossover
# --------------------------
crossover <- function(p1, p2, freq) {
  pool <- union(p1, p2)
  probs <- freq[pool] / sum(freq[pool])
  sort(sample(pool, K, replace = FALSE, prob = probs))
}

# --------------------------
# Hill-Climb
# --------------------------
hill_climb <- function(sub, freq) {
  improved <- TRUE
  best <- sub
  best_score <- score_subset(sub, freq)
  
  while (improved) {
    improved <- FALSE
    
    for (x in sub) {
      for (y in setdiff(1:N, sub)) {
        cand <- sort(c(setdiff(best, x), y))
        s <- score_subset(cand, freq)
        
        if (s > best_score) {
          best <- cand
          best_score <- s
          improved <- TRUE
        }
      }
    }
  }
  best
}

# --------------------------
# Initial Population
# --------------------------
init_population <- function(freq) {
  pop <- list()
  pop[[1]] <- sort(order(freq, decreasing = TRUE)[1:K])
  
  for (i in 1:10) pop[[length(pop)+1]] <- generate_grasp(freq)
  for (i in 1:20) pop[[length(pop)+1]] <- sort(sample(1:N, K))
  
  pop
}

# --------------------------
# MAIN PARALLEL OPTIMIZER
# --------------------------
hybrid_optimize_parallel <- function(file = "lottery_draws.csv",
                                     generations = 300) {
  
  draws <- load_draws(file)
  freq <- compute_frequencies(draws)
  
  population <- init_population(freq)
  tabu <- new.env()
  best_list <- list()
  
  T <- 1.0   # SA temperature
  
  for (gen in 1:generations) {
    
    # Parallel scoring
    scores <- future_sapply(population, score_subset, freq = freq)
    ranked <- order(scores, decreasing = TRUE)
    population <- population[ranked]
    
    # Beam store
    best_list <- c(best_list, population[1:5])
    best_list <- best_list[
      order(sapply(best_list, score_subset, freq), decreasing = TRUE)
    ]
    if (length(best_list) > 20) best_list <- best_list[1:20]
    
    elite <- population[1:5]
    new_pop <- elite
    
    # GA
    while (length(new_pop) < 31) {
      parents <- sample(elite, 2)
      child <- crossover(parents[[1]], parents[[2]], freq)
      
      if (runif(1) < 0.25) child <- mutate_subset(child)
      
      key <- paste(child, collapse = "-")
      if (is.null(tabu[[key]])) {
        tabu[[key]] <- TRUE
        new_pop[[length(new_pop)+1]] <- child
      }
    }
    
    # Simulated annealing
    if (gen %% 20 == 0) {
      worst <- population[[length(population)]]
      mutated <- mutate_subset(worst)
      delta <- score_subset(mutated, freq) - score_subset(worst, freq)
      
      if (delta < 0 && runif(1) < exp(delta/T)) {
        new_pop[[length(new_pop)+1]] <- mutated
      }
      T <- T * 0.95
    }
    
    # Parallel hill climbing
    elite <- future_lapply(elite, hill_climb, freq = freq)
    
    population <- c(elite, new_pop)
    
    cat("Gen", gen, " | Best:", score_subset(population[[1]], freq), "\n")
  }
  
  best_list[1:20]
}

# Example run
# result <- hybrid_optimize_parallel("lottery_draws.csv", generations = 200)
# result
```

```         
###############################################################################
# HYBRID METAHEURISTIC OPTIMIZER (PARALLEL VERSION)
# Bayesian + Markov + Limit-Theorem Integration (Product-Model Scoring)
###############################################################################

library(future.apply)
library(dplyr)

plan(multicore)

N <- 49
K <- 15

# --------------------------
# Load draws
# --------------------------
load_draws <- function(file = "lottery_draws.csv") {
  df <- read.csv(file)
  as.matrix(df[,2:7])
}

# --------------------------
# Raw frequency
# --------------------------
compute_frequencies <- function(draws) {
  freq <- rep(0, N)
  for (i in 1:nrow(draws)) freq[draws[i,]] <- freq[draws[i,]] + 1
  freq
}

# --------------------------
# Bayesian Posterior (Beta-Binomial)
# --------------------------
compute_bayesian <- function(draws, alpha=1, beta=1) {
  freq <- compute_frequencies(draws)
  total <- sum(freq)
  posterior <- (freq + alpha) / (total + alpha + beta)
  posterior
}

# --------------------------
# Markov Transition Matrix
# --------------------------
compute_markov <- function(draws) {
  M <- matrix(1, nrow=N, ncol=N) # Laplace smoothing

  for (t in 2:nrow(draws)) {
    prev <- draws[t-1,]
    curr <- draws[t,]
    for (i in prev) for (j in curr) M[i,j] <- M[i,j] + 1
  }
  M / rowSums(M)  # normalize
}

# --------------------------
# Limit-Theorem Z-score
# --------------------------
compute_zscore <- function(draws) {
  freq <- compute_frequencies(draws)
  z <- (freq - mean(freq)) / sd(freq)
  # normalize 0–1
  z_norm <- (z - min(z)) / (max(z) - min(z) + 1e-9)
  z_norm
}

# --------------------------
# Combined Probabilistic Score Vector
# --------------------------
compute_score_vector <- function(draws, alpha=1, beta=1, gamma=1) {
  posterior <- compute_bayesian(draws)
  M <- compute_markov(draws)

  # last draw for Markov prediction
  last_draw <- draws[nrow(draws),]

  markov_score <- rep(0, N)
  for (j in 1:N) {
    prob_sum <- 0
    for (i in last_draw) prob_sum <- prob_sum + M[i,j]
    markov_score[j] <- prob_sum / length(last_draw)
  }

  # Normalize Markov 0–1
  markov_score <- (markov_score - min(markov_score)) /
                  (max(markov_score) - min(markov_score) + 1e-9)

  z_norm <- compute_zscore(draws)

  # product model
  final <- (posterior^alpha) * (markov_score^beta) * (z_norm^gamma)
  final
}

# --------------------------
# Subset scoring
# --------------------------
score_subset <- function(subset, score_vector) sum(score_vector[subset])

# --------------------------
# GRASP
# --------------------------
generate_grasp <- function(score_vec, k = 5) {
  greedy <- order(score_vec, decreasing = TRUE)[1:k]
  remaining <- setdiff(1:N, greedy)
  probs <- score_vec[remaining] / sum(score_vec[remaining])
  rest <- sample(remaining, K-k, prob = probs)
  sort(c(greedy, rest))
}

# --------------------------
# Mutation
# --------------------------
mutate_subset <- function(sub) {
  m <- sample(1:3, 1)
  remove <- sample(sub, m)
  add <- sample(setdiff(1:N, sub), m)
  sort(c(setdiff(sub, remove), add))
}

# --------------------------
# Crossover
# --------------------------
crossover <- function(p1, p2, score_vec) {
  pool <- union(p1, p2)
  probs <- score_vec[pool] / sum(score_vec[pool])
  sort(sample(pool, K, replace = FALSE, prob = probs))
}

# --------------------------
# Hill-Climb
# --------------------------
hill_climb <- function(sub, score_vec) {
  improved <- TRUE
  best <- sub
  best_score <- score_subset(sub, score_vec)
  
  while (improved) {
    improved <- FALSE
    
    for (x in sub) {
      for (y in setdiff(1:N, sub)) {
        cand <- sort(c(setdiff(best, x), y))
        s <- score_subset(cand, score_vec)
        
        if (s > best_score) {
          best <- cand
          best_score <- s
          improved <- TRUE
        }
      }
    }
  }
  best
}

# --------------------------
# Initial population
# --------------------------
init_population <- function(score_vec) {
  pop <- list()
  pop[[1]] <- sort(order(score_vec, decreasing = TRUE)[1:K])
  
  for (i in 1:10) pop[[length(pop)+1]] <- generate_grasp(score_vec)
  for (i in 1:20) pop[[length(pop)+1]] <- sort(sample(1:N, K))
  
  pop
}

# --------------------------
# Main Hybrid Optimizer
# --------------------------
hybrid_optimize_parallel <- function(file="lottery_draws.csv",
                                     generations=300,
                                     alpha=1, beta=1, gamma=1) {
  
  draws <- load_draws(file)
  score_vec <- compute_score_vector(draws, alpha, beta, gamma)
  
  population <- init_population(score_vec)
  tabu <- new.env()
  best_list <- list()
  
  T <- 1.0
  
  for (gen in 1:generations) {
    
    scores <- future_sapply(population, score_subset, score_vector=score_vec)
    ranked <- order(scores, decreasing = TRUE)
    population <- population[ranked]
    
    best_list <- c(best_list, population[1:5])
    best_list <- best_list[
      order(sapply(best_list, score_subset, score_vector=score_vec), decreasing=TRUE)
    ]
    if (length(best_list) > 20) best_list <- best_list[1:20]
    
    elite <- population[1:5]
    new_pop <- elite
    
    while (length(new_pop) < 31) {
      parents <- sample(elite, 2)
      child <- crossover(parents[[1]], parents[[2]], score_vec)
      
      if (runif(1) < 0.25) child <- mutate_subset(child)
      
      key <- paste(child, collapse = "-")
      if (is.null(tabu[[key]])) {
        tabu[[key]] <- TRUE
        new_pop[[length(new_pop)+1]] <- child
      }
    }
    
    if (gen %% 20 == 0) {
      worst <- population[[length(population)]]
      mutated <- mutate_subset(worst)
      delta <- score_subset(mutated, score_vec) - score_subset(worst, score_vec)
      
      if (delta < 0 && runif(1) < exp(delta/T)) {
        new_pop[[length(new_pop)+1]] <- mutated
      }
      T <- T * 0.95
    }
    
    elite <- future_lapply(elite, hill_climb, score_vec = score_vec)
    population <- c(elite, new_pop)
    
    cat("Gen", gen, " | Best:", score_subset(population[[1]], score_vec), "\n")
  }
  
  best_list[1:20]
}

# Example run
# result <- hybrid_optimize_parallel("lottery_draws.csv", generations = 200)
# result
# Running the hybrid optimizer and predicting next draw

result <- hybrid_optimize_parallel("lottery_draws.csv", generations = 250)
best_prediction <- result[[1]]
best_prediction

predict_next_6 <- function(result_list, score_vec) {
  best15 <- result_list[[1]]
  # pick top 6 numbers with highest individual probabilistic scores
  sort(best15[order(score_vec[best15], decreasing = TRUE)][1:6])
}
next_draw_prediction <- predict_next_6(result, 
                                        compute_score_vector(
                                          load_draws("lottery_draws.csv")))
next_draw_prediction
```
