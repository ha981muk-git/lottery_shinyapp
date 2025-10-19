# ============================================================================
# PERFORMANCE TESTING - SIMPLIFIED (renv compatible)
# ============================================================================
# Save this as: test_performance_simple.R
# Run from project root with: source("test_performance_simple.R")

# IMPORTANT: If asked about renv activation, choose option 1

# Step 1: Set working directory
setwd("~/drive/workspace/global/code/R/lottery_shinyapp_v2")

# Step 2: Check what we have
cat("\n📍 Working Directory:", getwd(), "\n")
cat("📁 Checking files...\n")
cat("   ✓ app.R exists?", file.exists("app.R"), "\n")
cat("   ✓ renv.lock exists?", file.exists("renv.lock"), "\n")
cat("   ✓ renv/ folder exists?", dir.exists("renv"), "\n\n")

# Step 3: Load required libraries (don't touch renv)
library(shinytest2)

cat("✅ shinytest2 loaded successfully\n\n")

# ============================================================================
# TEST 1: APP INITIALIZATION
# ============================================================================
test_app_initialization <- function() {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║ TEST 1: APP INITIALIZATION TIME                            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  tryCatch({
    load_start <- Sys.time()
    
    cat("⏳ Starting app driver...\n")
    cat("   (This may take 30-40 seconds on first run)\n\n")
    
    app <- AppDriver$new(
      app_dir = ".",
      load_timeout = 50000,
      view = FALSE
    )
    
    cat("⏳ App started, waiting for render...\n")
    Sys.sleep(3)
    
    load_end <- Sys.time()
    load_time <- as.numeric(difftime(load_end, load_start, units = "secs"))
    
    cat("✅ App loaded successfully!\n")
    cat("⏱️  Load time: ", round(load_time, 2), " seconds\n", sep = "")
    
    status <- if(load_time < 5) "✓ EXCELLENT" else if(load_time < 10) "⚠️  ACCEPTABLE" else "❌ SLOW"
    cat("📊 Status: ", status, "\n\n", sep = "")
    
    app$stop()
    
    return(list(
      success = TRUE,
      load_time = load_time,
      status = status
    ))
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# TEST 2: SLIDER INTERACTION
# ============================================================================
test_slider_performance <- function() {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║ TEST 2: SLIDER INTERACTION PERFORMANCE                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  tryCatch({
    app <- AppDriver$new(".", load_timeout = 50000, view = FALSE)
    Sys.sleep(3)
    
    slider_times <- vector("numeric", 5)
    
    cat("📊 Testing 5 slider position changes...\n\n")
    
    for (i in 1:5) {
      slider_start <- Sys.time()
      
      min_val <- (i - 1) * 7 + 1
      max_val <- min(min_val + 12, 49)
      
      cat("  [", i, "] Setting range to (", min_val, "-", max_val, ")...", sep = "")
      
      app$set_inputs(`inputs1-range` = c(min_val, max_val))
      Sys.sleep(0.7)
      
      slider_end <- Sys.time()
      slider_times[i] <- as.numeric(difftime(slider_end, slider_start, units = "secs"))
      
      cat(" ", round(slider_times[i], 3), "s\n", sep = "")
    }
    
    avg_time <- mean(slider_times)
    max_time <- max(slider_times)
    min_time <- min(slider_times)
    
    cat("\n📈 RESULTS:\n")
    cat("   Average: ", round(avg_time, 3), " seconds\n", sep = "")
    cat("   Maximum: ", round(max_time, 3), " seconds\n", sep = "")
    cat("   Minimum: ", round(min_time, 3), " seconds\n", sep = "")
    
    status <- if(max_time < 2) "✓ EXCELLENT" else if(max_time < 3) "⚠️  ACCEPTABLE" else "❌ SLOW"
    cat("   Status:  ", status, "\n\n", sep = "")
    
    app$stop()
    
    return(list(
      success = TRUE,
      times = slider_times,
      average = avg_time,
      maximum = max_time,
      status = status
    ))
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# TEST 3: METRIC SWITCHING
# ============================================================================
test_metric_switching <- function() {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║ TEST 3: METRIC SWITCHING PERFORMANCE                       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  tryCatch({
    app <- AppDriver$new(".", load_timeout = 50000, view = FALSE)
    Sys.sleep(3)
    
    metrics <- c("balls", "sums", "odds_evens", "table", "difference", "lag")
    switch_times <- vector("numeric", length(metrics))
    
    cat("🔄 Testing metric switches...\n\n")
    
    for (i in seq_along(metrics)) {
      switch_start <- Sys.time()
      
      cat("  [", i, "] Switching to '", metrics[i], "'...", sep = "")
      
      app$set_inputs(`inputs1-metric` = metrics[i])
      Sys.sleep(0.8)
      
      switch_end <- Sys.time()
      switch_times[i] <- as.numeric(difftime(switch_end, switch_start, units = "secs"))
      
      cat(" ", round(switch_times[i], 3), "s\n", sep = "")
    }
    
    avg_time <- mean(switch_times)
    max_time <- max(switch_times)
    min_time <- min(switch_times)
    
    cat("\n📈 RESULTS:\n")
    cat("   Average: ", round(avg_time, 3), " seconds\n", sep = "")
    cat("   Maximum: ", round(max_time, 3), " seconds\n", sep = "")
    cat("   Minimum: ", round(min_time, 3), " seconds\n", sep = "")
    
    status <- if(max_time < 2) "✓ EXCELLENT" else if(max_time < 3) "⚠️  ACCEPTABLE" else "❌ SLOW"
    cat("   Status:  ", status, "\n\n", sep = "")
    
    app$stop()
    
    return(list(
      success = TRUE,
      times = switch_times,
      average = avg_time,
      maximum = max_time,
      status = status
    ))
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# TEST 4: TIME RANGE CHANGES
# ============================================================================
test_time_range_performance <- function() {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║ TEST 4: TIME RANGE SELECTION PERFORMANCE                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  tryCatch({
    app <- AppDriver$new(".", load_timeout = 50000, wait_ = TRUE, view = FALSE)
    Sys.sleep(3)
    
    time_ranges <- c(7, 30, 60, 90, 120)
    time_results <- vector("numeric", length(time_ranges))
    
    cat("📅 Testing time range changes...\n\n")
    
    for (i in seq_along(time_ranges)) {
      time_start <- Sys.time()
      
      cat("  [", time_ranges[i], " days]: Updating...", sep = "")
      
      app$set_inputs(`inputs1-timeRange` = time_ranges[i])
      Sys.sleep(0.8)
      
      time_end <- Sys.time()
      time_results[i] <- as.numeric(difftime(time_end, time_start, units = "secs"))
      
      cat(" ", round(time_results[i], 3), "s\n", sep = "")
    }
    
    avg_time <- mean(time_results)
    max_time <- max(time_results)
    min_time <- min(time_results)
    
    cat("\n📈 RESULTS:\n")
    cat("   Average: ", round(avg_time, 3), " seconds\n", sep = "")
    cat("   Maximum: ", round(max_time, 3), " seconds\n", sep = "")
    cat("   Minimum: ", round(min_time, 3), " seconds\n", sep = "")
    
    status <- if(max_time < 2.5) "✓ EXCELLENT" else if(max_time < 4) "⚠️  ACCEPTABLE" else "❌ SLOW"
    cat("   Status:  ", status, "\n\n", sep = "")
    
    app$stop()
    
    return(list(
      success = TRUE,
      times = time_results,
      average = avg_time,
      maximum = max_time,
      status = status
    ))
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# TEST 5: STRESS TEST (All interactions together)
# ============================================================================
test_stress_interactions <- function() {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║ TEST 5: STRESS TEST (RAPID INTERACTIONS)                   ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  tryCatch({
    app <- AppDriver$new(".", load_timeout = 50000, wait_ = TRUE, view = FALSE)
    Sys.sleep(3)
    
    stress_start <- Sys.time()
    
    cat("⚡ Performing rapid sequential interactions...\n\n")
    
    cat("  1. Changing slider range...\n")
    app$set_inputs(`inputs1-range` = c(10, 40))
    Sys.sleep(0.5)
    
    cat("  2. Switching to 'sums' metric...\n")
    app$set_inputs(`inputs1-metric` = "sums")
    Sys.sleep(0.5)
    
    cat("  3. Changing time range to 60 days...\n")
    app$set_inputs(`inputs1-timeRange` = 60)
    Sys.sleep(0.5)
    
    cat("  4. Clicking refresh button...\n")
    app$click("inputs1-refresh")
    Sys.sleep(1)
    
    cat("  5. Switching to 'table' metric...\n")
    app$set_inputs(`inputs1-metric` = "table")
    Sys.sleep(0.5)
    
    cat("  6. Fine-tuning slider range...\n")
    app$set_inputs(`inputs1-range` = c(15, 35))
    Sys.sleep(0.5)
    
    stress_end <- Sys.time()
    stress_time <- as.numeric(difftime(stress_end, stress_start, units = "secs"))
    
    cat("\n✅ Stress test completed\n")
    cat("⏱️  Total time: ", round(stress_time, 2), " seconds\n", sep = "")
    cat("   (6 interactions)\n")
    
    status <- if(stress_time < 8) "✓ EXCELLENT" else if(stress_time < 12) "⚠️  ACCEPTABLE" else "❌ SLOW"
    cat("   Status: ", status, "\n\n", sep = "")
    
    app$stop()
    
    return(list(
      success = TRUE,
      total_time = stress_time,
      interactions = 6,
      status = status
    ))
    
  }, error = function(e) {
    cat("❌ ERROR:", e$message, "\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# MAIN TEST RUNNER
# ============================================================================
run_all_tests <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  🚀 SHINY LOTTERY APP - PERFORMANCE TEST SUITE\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  results <- list()
  
  # Test 1
  cat("\n⏳ TEST 1 of 5: App Initialization\n")
  results$init <- test_app_initialization()
  
  if (!results$init$success) {
    cat("\n❌ Cannot proceed - app initialization failed.\n")
    cat("   Make sure app.R exists and all dependencies are installed.\n")
    return(results)
  }
  
  # Test 2
  cat("⏳ TEST 2 of 5: Slider Performance\n")
  results$slider <- test_slider_performance()
  
  # Test 3
  cat("⏳ TEST 3 of 5: Metric Switching\n")
  results$metrics <- test_metric_switching()
  
  # Test 4
  cat("⏳ TEST 4 of 5: Time Range Performance\n")
  results$time_range <- test_time_range_performance()
  
  # Test 5
  cat("⏳ TEST 5 of 5: Stress Test\n")
  results$stress <- test_stress_interactions()
  
  # Final Summary Report
  print_summary_report(results)
  
  return(results)
}

# ============================================================================
# SUMMARY REPORT
# ============================================================================
print_summary_report <- function(results) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  📊 PERFORMANCE TEST SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Test 1
  if (results$init$success) {
    cat("1️⃣  APP INITIALIZATION\n")
    cat("   Load Time: ", round(results$init$load_time, 2), "s  ", results$init$status, "\n\n", sep = "")
  }
  
  # Test 2
  if (results$slider$success) {
    cat("2️⃣  SLIDER INTERACTION (5 changes)\n")
    cat("   Average:  ", round(results$slider$average, 3), "s\n", sep = "")
    cat("   Maximum:  ", round(results$slider$maximum, 3), "s  ", results$slider$status, "\n\n", sep = "")
  }
  
  # Test 3
  if (results$metrics$success) {
    cat("3️⃣  METRIC SWITCHING (6 metrics)\n")
    cat("   Average:  ", round(results$metrics$average, 3), "s\n", sep = "")
    cat("   Maximum:  ", round(results$metrics$maximum, 3), "s  ", results$metrics$status, "\n\n", sep = "")
  }
  
  # Test 4
  if (results$time_range$success) {
    cat("4️⃣  TIME RANGE CHANGES (5 ranges)\n")
    cat("   Average:  ", round(results$time_range$average, 3), "s\n", sep = "")
    cat("   Maximum:  ", round(results$time_range$maximum, 3), "s  ", results$time_range$status, "\n\n", sep = "")
  }
  
  # Test 5
  if (results$stress$success) {
    cat("5️⃣  STRESS TEST (6 interactions)\n")
    cat("   Total:    ", round(results$stress$total_time, 2), "s  ", results$stress$status, "\n\n", sep = "")
  }
  
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  cat("💡 NEXT STEPS:\n\n")
  cat("✓ If all show ✓ EXCELLENT: Your app is well-optimized!\n\n")
  cat("⚠️  If showing ⚠️ ACCEPTABLE or ❌ SLOW:\n")
  cat("   1. Check generate_metrics() function - is it slow?\n")
  cat("   2. Are debounce settings too high? Try reducing by 50ms\n")
  cat("   3. Is data filtering expensive? Add memoise() caching\n")
  cat("   4. Share results below for specific recommendations\n\n")
}

# ============================================================================
# RUN TESTS
# ============================================================================

cat("\n🔄 Initializing tests...\n")
cat("⏳ This will take 3-5 minutes total\n")
cat("⏰ Waiting 3 seconds before start...\n\n")

Sys.sleep(3)

performance_results <- run_all_tests()

cat("\n✅ ALL TESTS COMPLETED!\n\n")
cat("📌 Save your results to share with Claude for optimization advice.\n\n")