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
  
  rds_path <- file.path(getwd(), "data", "LOTTO_clean.rds")
  
  load_data <- function(force = FALSE) {
    if (!force && file.exists(rds_path)) {
      message("⚡ Using precomputed RDS data: ", rds_path)
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
