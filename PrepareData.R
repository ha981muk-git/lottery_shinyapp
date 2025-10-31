# ============================================================================
# PrepareData.R - Robust Data Loading with Checksum-based Caching
# ============================================================================

create_data_loader <- function(file_path = NULL) {
  
  # ========================================================================
  # STEP 1: ROBUST FILE PATH RESOLUTION
  # ========================================================================
  if (is.null(file_path)) {
    # Try multiple strategies to find the data file
    
    # Strategy 1: Script directory (best for deployed apps)
    script_dir <- tryCatch({
      # Get the directory of the currently executing script
      if (!is.null(sys.frames()[[1]]$ofile)) {
        dirname(sys.frames()[[1]]$ofile)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (!is.null(script_dir)) {
      candidate <- file.path(script_dir, "data", "LOTTO_ab_2018.csv")
      if (file.exists(candidate)) {
        file_path <- candidate
        message("✓ Found data file via script directory: ", file_path)
      }
    }
    
    # Strategy 2: Current working directory (fallback for interactive)
    if (is.null(file_path)) {
      candidate <- file.path(getwd(), "data", "LOTTO_ab_2018.csv")
      if (file.exists(candidate)) {
        file_path <- candidate
        message("✓ Found data file via working directory: ", file_path)
      }
    }
    
    # Strategy 3: Shiny app directory (for Shiny Server deployments)
    if (is.null(file_path)) {
      # Check if we're in a Shiny context
      if (exists(".shiny_app_dir", envir = .GlobalEnv)) {
        candidate <- file.path(.GlobalEnv$.shiny_app_dir, "data", "LOTTO_ab_2018.csv")
        if (file.exists(candidate)) {
          file_path <- candidate
          message("✓ Found data file via Shiny app directory: ", file_path)
        }
      }
    }
    
    # Strategy 4: Look in parent directories (in case running from subdirectory)
    if (is.null(file_path)) {
      parent_dir <- dirname(getwd())
      candidate <- file.path(parent_dir, "data", "LOTTO_ab_2018.csv")
      if (file.exists(candidate)) {
        file_path <- candidate
        message("✓ Found data file via parent directory: ", file_path)
      }
    }
    
    # Final check: fail with helpful message
    if (is.null(file_path)) {
      stop("❌ Data file not found. Searched in:\n",
           "  1. Script directory: ", ifelse(is.null(script_dir), "(unknown)", script_dir), "\n",
           "  2. Working directory: ", getwd(), "\n",
           "  3. Parent directory: ", dirname(getwd()), "\n",
           "Please ensure 'data/LOTTO_ab_2018.csv' exists relative to your app.")
    }
  }
  
  # Verify file exists
  if (!file.exists(file_path)) {
    stop("❌ Data file does not exist: ", file_path)
  }
  
  message("📁 Data file path: ", file_path)
  
  # ========================================================================
  # STEP 2: INITIALIZE CACHE WITH CHECKSUM
  # ========================================================================
  cache <- new.env(parent = emptyenv())
  cache$data <- NULL
  cache$last_modified <- NULL
  cache$checksum <- NULL  # ✅ NEW: Track file content changes
  
  # ========================================================================
  # STEP 3: DATA LOADING FUNCTION WITH CHECKSUM VALIDATION
  # ========================================================================
  load_data <- function(force = FALSE) {
    
    # Verify file still exists (might be deleted during runtime)
    if (!file.exists(file_path)) {
      stop("❌ Data file does not exist: ", file_path)
    }
    
    # Get file metadata
    current_modified <- file.info(file_path)$mtime
    
    # ✅ IMPROVED: Use MD5 checksum for more reliable cache validation
    # (timestamp can be unreliable across systems/deployments)
    current_checksum <- tryCatch({
      tools::md5sum(file_path)
    }, error = function(e) {
      warning("⚠️ Could not compute checksum, falling back to timestamp")
      as.character(current_modified)
    })
    
    # Check if we can use cached data
    if (!force && 
        !is.null(cache$data) && 
        !is.null(cache$checksum) &&
        identical(current_checksum, cache$checksum)) {
      message("⚡ Using cached data (checksum match)")
      return(cache$data)
    }
    
    message("🔄 Loading data from file...")
    
    # ========================================================================
    # STEP 4: LOAD AND CLEAN DATA
    # ========================================================================
    
    # Load ALL columns as character to avoid parsing issues
    data <- tryCatch({
      vroom::vroom(
        file_path,
        delim = ";",
        col_types = vroom::cols(.default = vroom::col_character()),
        trim_ws = TRUE,
        locale = vroom::locale(encoding = "ISO-8859-1"),
        show_col_types = FALSE  # ✅ NEW: Suppress column type messages
      ) %>%
        janitor::clean_names()
    }, error = function(e) {
      stop("❌ Failed to load data file: ", e$message)
    })
    
    # Validate data loaded
    if (nrow(data) == 0) {
      stop("❌ Data file is empty")
    }
    
    # Remove empty rows and junk columns (x2 / ...2)
    data <- data %>%
      dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.) | . == "")) %>%
      dplyr::select(-dplyr::matches("^x\\d+$"), -dplyr::matches("^\\.\\.\\d+$"))
    
    # ========================================================================
    # STEP 5: VALIDATE AND CONVERT DATES
    # ========================================================================
    
    # Trim whitespace from date column
    data <- data %>%
      dplyr::mutate(datum = trimws(datum))
    
    # Validate date format (DD.MM.YYYY)
    valid_date_pattern <- "^\\d{2}\\.\\d{2}\\.\\d{4}$"
    invalid_dates <- data %>% 
      dplyr::filter(!grepl(valid_date_pattern, datum))
    
    if (nrow(invalid_dates) > 0) {
      warning("⚠️ Removing ", nrow(invalid_dates), " rows with invalid dates")
    }
    
    # Filter valid dates and convert
    data <- data %>%
      dplyr::filter(grepl(valid_date_pattern, datum)) %>%
      dplyr::mutate(datum = as.Date(datum, format = "%d.%m.%Y"))
    
    # ========================================================================
    # STEP 6: CONVERT NUMERIC COLUMNS
    # ========================================================================
    
    num_cols <- c("gewinnzahlen", "zz", "s", "spiel77", "super6", "spieleinsatz", "anz_kl_2")
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(num_cols), ~ suppressWarnings(as.numeric(.))))
    
    # ========================================================================
    # STEP 7: TRANSFORM TO LOTTERY FORMAT
    # ========================================================================
    
    lotto_clean <- data %>%
      dplyr::select(
        datum,
        ball_1    = gewinnzahlen,
        ball_2    = zz,
        ball_3    = s,
        ball_4    = spiel77,
        ball_5    = super6,
        ball_6    = spieleinsatz,
        superzahl = anz_kl_2
      ) %>%
      # Remove rows with any NA values
      dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
      # Sort balls 1-6 in ascending order
      dplyr::mutate(
        sorted = purrr::pmap(
          list(ball_1, ball_2, ball_3, ball_4, ball_5, ball_6), 
          ~ sort(c(...))
        ),
        ball_1 = purrr::map_dbl(sorted, 1),
        ball_2 = purrr::map_dbl(sorted, 2),
        ball_3 = purrr::map_dbl(sorted, 3),
        ball_4 = purrr::map_dbl(sorted, 4),
        ball_5 = purrr::map_dbl(sorted, 5),
        ball_6 = purrr::map_dbl(sorted, 6)
      ) %>%
      dplyr::select(-sorted)
    
    # ========================================================================
    # STEP 8: VALIDATE FINAL DATA
    # ========================================================================
    
    if (nrow(lotto_clean) == 0) {
      stop("❌ No valid lottery data found after cleaning")
    }
    
    # Validate ball ranges (should be 1-49 for German Lotto)
    invalid_balls <- lotto_clean %>%
      dplyr::filter(
        ball_1 < 1 | ball_1 > 49 |
          ball_2 < 1 | ball_2 > 49 |
          ball_3 < 1 | ball_3 > 49 |
          ball_4 < 1 | ball_4 > 49 |
          ball_5 < 1 | ball_5 > 49 |
          ball_6 < 1 | ball_6 > 49
      )
    
    if (nrow(invalid_balls) > 0) {
      warning("⚠️ Found ", nrow(invalid_balls), " rows with invalid ball numbers (not in 1-49)")
      lotto_clean <- lotto_clean %>%
        dplyr::filter(
          ball_1 >= 1 & ball_1 <= 49,
          ball_2 >= 1 & ball_2 <= 49,
          ball_3 >= 1 & ball_3 <= 49,
          ball_4 >= 1 & ball_4 <= 49,
          ball_5 >= 1 & ball_5 <= 49,
          ball_6 >= 1 & ball_6 <= 49
        )
    }
    
    # ========================================================================
    # STEP 9: CACHE RESULTS
    # ========================================================================
    
    cache$data <- lotto_clean
    cache$last_modified <- current_modified
    cache$checksum <- current_checksum  # ✅ NEW: Store checksum
    
    message("✅ Data loaded successfully: ", nrow(lotto_clean), " draws")
    message("📅 Date range: ", min(lotto_clean$datum), " to ", max(lotto_clean$datum))
    
    return(lotto_clean)
  }
  
  # ========================================================================
  # STEP 10: RETURN DATA LOADER INTERFACE
  # ========================================================================
  
  list(
    load = load_data,
    # ✅ NEW: Add utility functions
    get_cache_info = function() {
      list(
        cached = !is.null(cache$data),
        rows = if (!is.null(cache$data)) nrow(cache$data) else 0,
        last_modified = cache$last_modified,
        checksum = cache$checksum,
        file_path = file_path
      )
    },
    clear_cache = function() {
      cache$data <- NULL
      cache$last_modified <- NULL
      cache$checksum <- NULL
      message("🗑️ Cache cleared")
      invisible(TRUE)
    }
  )
}

# ============================================================================
# USAGE
# ============================================================================

# Create global data loader instance
data_loader <- create_data_loader()

# Optional: Test loading on startup (commented out for production)
# tryCatch({
#   test_data <- data_loader$load()
#   message("✅ Data loader test successful: ", nrow(test_data), " rows loaded")
# }, error = function(e) {
#   warning("⚠️ Data loader test failed: ", e$message)
# })