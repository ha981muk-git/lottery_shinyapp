# Source main files
script_folder <- "."
data_path <- file.path(script_folder,'data',"LOTTO_ab_2018.csv")

library(vroom)
library(janitor)
library(dplyr)
library(readr)
library(purrr)

# ==== Loading data
# data <- read_delim(
#   "~/drive/workspace/global/code/R/lottery_bayesian/LOTTO_ab_2018.csv",
#   delim = ";",
#   trim_ws = TRUE
# )
# ==== Data Loader with Caching and Robust Column Handling
create_data_loader <- function(file_path = file.path(getwd(), "data", "LOTTO_ab_2018.csv")) {
  
  cache <- new.env(parent = emptyenv())
  cache$data <- NULL
  cache$last_modified <- NULL
  cache$rds_message_shown <- FALSE
  cache$refresh_checked <- FALSE
  
  rds_path <- file.path(getwd(), "data", "LOTTO_clean.rds")
  refresh_meta_path <- file.path(getwd(), "data", "LOTTO_refresh_meta.rds")

  as_bool <- function(value, default = TRUE) {
    if (is.null(value) || !nzchar(value)) return(default)
    tolower(trimws(value)) %in% c("1", "true", "yes", "y", "on")
  }

  as_int <- function(value, default_value) {
    parsed <- suppressWarnings(as.integer(value))
    if (is.na(parsed)) return(default_value)
    parsed
  }

  refresh_enabled <- as_bool(Sys.getenv("LOTTO_AUTO_REFRESH_ENABLED", unset = "true"), TRUE)
  refresh_days <- max(1L, as_int(Sys.getenv("LOTTO_AUTO_REFRESH_DAYS", unset = "14"), 14L))
  refresh_tolerance_days <- max(0L, as_int(Sys.getenv("LOTTO_AUTO_REFRESH_TOLERANCE_DAYS", unset = "5"), 5L))
  refresh_min_days <- max(1L, refresh_days - refresh_tolerance_days)
  refresh_max_days <- refresh_days + refresh_tolerance_days
  year_from <- max(2000L, as_int(Sys.getenv("LOTTO_DATA_YEAR_FROM", unset = "2018"), 2018L))
  current_year <- as_int(format(Sys.Date(), "%Y"), 2018L)
  year_to <- as_int(Sys.getenv("LOTTO_DATA_YEAR_TO", unset = as.character(current_year)), current_year)
  year_to <- max(year_from, year_to)

  read_refresh_meta <- function() {
    if (!file.exists(refresh_meta_path)) return(NULL)
    meta <- try(readRDS(refresh_meta_path), silent = TRUE)
    if (inherits(meta, "try-error") || !is.list(meta)) return(NULL)
    meta
  }

  write_refresh_meta <- function(success, error_message = NULL, next_due_at = NULL) {
    meta <- list(
      updated_at = as.character(Sys.time()),
      success = isTRUE(success),
      error_message = if (is.null(error_message)) "" else as.character(error_message),
      next_due_at = if (is.null(next_due_at)) "" else as.character(next_due_at)
    )
    try(saveRDS(meta, refresh_meta_path), silent = TRUE)
    invisible(TRUE)
  }

  schedule_next_refresh <- function(from_time = Sys.time()) {
    target_days <- refresh_days
    if (refresh_tolerance_days > 0L) {
      target_days <- sample(seq.int(refresh_min_days, refresh_max_days), size = 1)
    }
    from_time + as.difftime(target_days, units = "days")
  }

  refresh_due <- function() {
    if (!file.exists(file_path)) return(TRUE)

    now <- Sys.time()
    meta <- read_refresh_meta()

    if (!is.null(meta$next_due_at) && nzchar(as.character(meta$next_due_at))) {
      next_due <- suppressWarnings(as.POSIXct(meta$next_due_at, tz = "UTC"))
      if (!is.na(next_due)) {
        return(now >= next_due)
      }
    }

    if (!is.null(meta$updated_at)) {
      last_refresh <- suppressWarnings(as.POSIXct(meta$updated_at, tz = "UTC"))
      if (!is.na(last_refresh)) {
        return((now - last_refresh) >= as.difftime(refresh_days, units = "days"))
      }
    }

    mtime <- file.info(file_path)$mtime
    if (is.na(mtime)) return(TRUE)
    (now - mtime) >= as.difftime(refresh_min_days, units = "days")
  }

  download_latest_csv <- function() {
    url <- "https://www.westlotto.de/wlinfo/WL_InfoService"
    tmp_zip <- tempfile(pattern = "lotto_download_", fileext = ".zip")
    extract_dir <- tempfile(pattern = "lotto_extract_")
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

    on.exit(unlink(tmp_zip, force = TRUE), add = TRUE)
    on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)

    response <- httr::GET(
      url = url,
      query = list(
        gruppe = "ErgebnisDownload",
        client = "wldl",
        jahr_von = as.character(year_from),
        jahr_bis = as.character(year_to),
        spielart = "LOTTO",
        format = "csv"
      ),
      httr::write_disk(tmp_zip, overwrite = TRUE),
      httr::timeout(30)
    )

    if (httr::http_error(response)) {
      stop("Download failed with HTTP status ", httr::status_code(response))
    }

    zip_index <- try(utils::unzip(tmp_zip, list = TRUE), silent = TRUE)
    if (inherits(zip_index, "try-error") || is.null(zip_index) || nrow(zip_index) == 0) {
      stop("Downloaded file is not a valid ZIP archive")
    }

    csv_candidates <- zip_index$Name[grepl("\\.csv$", zip_index$Name, ignore.case = TRUE)]
    if (length(csv_candidates) == 0) {
      stop("ZIP archive did not contain a CSV file")
    }

    csv_name <- csv_candidates[[1]]
    utils::unzip(tmp_zip, files = csv_name, exdir = extract_dir, overwrite = TRUE)
    extracted_file <- file.path(extract_dir, csv_name)

    if (!file.exists(extracted_file)) {
      stop("CSV extraction failed")
    }

    first_line <- readLines(extracted_file, n = 1, warn = FALSE, encoding = "latin1")
    if (length(first_line) == 0 || !grepl("Datum", first_line[[1]], fixed = TRUE)) {
      stop("Downloaded CSV header is not recognized")
    }

    if (!file.copy(extracted_file, file_path, overwrite = TRUE)) {
      stop("Could not replace local CSV file")
    }

    if (file.exists(rds_path)) {
      unlink(rds_path, force = TRUE)
    }

    invisible(TRUE)
  }

  maybe_auto_refresh <- function(force = FALSE) {
    if (!isTRUE(refresh_enabled)) return(invisible(FALSE))
    if (!force && isTRUE(cache$refresh_checked)) return(invisible(FALSE))

    due <- isTRUE(force) || refresh_due()
    cache$refresh_checked <- TRUE

    if (!due) return(invisible(FALSE))

    message("🌐 Checking lottery archive for updates...")

    tryCatch({
      download_latest_csv()
      cache$data <- NULL
      cache$last_modified <- NULL
      cache$rds_message_shown <- FALSE
      next_due <- schedule_next_refresh()
      write_refresh_meta(success = TRUE, next_due_at = next_due)
      message("✅ Lottery archive refreshed from remote source")
      invisible(TRUE)
    }, error = function(e) {
      retry_at <- Sys.time() + as.difftime(1, units = "days")
      write_refresh_meta(success = FALSE, error_message = e$message, next_due_at = retry_at)
      warning("⚠️ Automatic lottery data refresh failed; using existing local data. Details: ", e$message)
      invisible(FALSE)
    })
  }
  
  load_data <- function(force = FALSE) {
    maybe_auto_refresh(force = force)

    if (!force && file.exists(rds_path)) {
      if (!isTRUE(cache$rds_message_shown)) {
        message("⚡ Using precomputed RDS data: ", rds_path)
        cache$rds_message_shown <- TRUE
      }
      return(readRDS(rds_path))
    }
    
    if (!file.exists(file_path)) {
      stop("❌ Data file does not exist: ", file_path)
    }
    
    current_modified <- file.info(file_path)$mtime
    
    if (!force && !is.null(cache$data) && identical(current_modified, cache$last_modified)) {
      message("⚡ Using cached data")
      return(cache$data)
    }
    
    message("🔄 Loading data from file...")
    
    # Load ALL columns as character to avoid parsing issues
    data <- vroom(
      file_path,
      delim = ";",
      col_types = cols(.default = col_character()),
      trim_ws = TRUE,
      locale = locale(encoding = "ISO-8859-1")   # 👈 Fix UTF-8 issue
    ) %>%
      clean_names()
    
    # Remove empty rows and junk column (x2 / ...2)
    data <- data %>%
      filter(!if_all(everything(), ~ is.na(.) | . == "")) %>%
      select(-matches("^x\\d+$"), -matches("^\\.\\.\\d+$"))
    
    # Validate and convert date
    data <- data %>%
      mutate(datum = trimws(datum))
    
    valid_date_pattern <- "^\\d{2}\\.\\d{2}\\.\\d{4}$"
    invalid_dates <- data %>% filter(!grepl(valid_date_pattern, datum))
    
    if (nrow(invalid_dates) > 0) {
      warning("⚠️ Removing ", nrow(invalid_dates), " invalid date rows")
    }
    
    data <- data %>%
      filter(grepl(valid_date_pattern, datum)) %>%
      mutate(datum = as.Date(datum, format = "%d.%m.%Y"))
    
    # Convert relevant numeric columns
    num_cols <- c("gewinnzahlen", "zz", "s", "spiel77", "super6", "spieleinsatz", "anz_kl_2")
    data <- data %>%
      mutate(across(any_of(num_cols), ~ suppressWarnings(as.numeric(.))))
    
    lotto_clean <- data %>%
      select(
        datum,
        ball_1    = gewinnzahlen,
        ball_2    = zz,
        ball_3    = s,
        ball_4    = spiel77,
        ball_5    = super6,
        ball_6    = spieleinsatz,
        superzahl = anz_kl_2
      ) %>%
      filter(if_all(everything(), ~ !is.na(.)))
    
    # Fast vectorized sorting instead of purrr::pmap
    cols <- paste0("ball_", 1:6)
    m <- as.matrix(lotto_clean[, cols])
    m_sorted <- base::t(apply(m, 1, sort))
    
    lotto_clean[, cols] <- m_sorted
    
    options(li_base_row_count = nrow(lotto_clean))
    cache$data <- lotto_clean
    cache$last_modified <- current_modified
    
    # Save the processed data to bypass this slow logic next time
    saveRDS(lotto_clean, rds_path)
    
    message("✅ Data loaded and serialized to RDS: ", nrow(lotto_clean), " rows")
    return(lotto_clean)
  }
  
  list(load = load_data)
}

# Initialize Loader
data_loader <- create_data_loader(data_path)

# Wrapper for compatibility
generate_metrics <- function() {
  data_loader$load()
}
