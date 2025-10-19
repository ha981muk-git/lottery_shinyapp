tryCatch({
  # Source main files
  data_path <- file.path(getwd(), "data", "LOTTO_ab_2018.csv")
  
  # ==== Loading data
  data <- vroom(data_path, delim = ";",
                col_types = cols(
                  Datum = col_character(),
                  Gewinnzahlen = col_double(),
                  ZZ = col_double(),
                  S = col_double(),
                  Spiel77 = col_double(),
                  Super6 = col_double(),
                  Spieleinsatz = col_double(),
                  `Anz. Kl. 2` = col_double()
                ))
  data <- clean_names(data)  # make column names safe
  
  # ==== Cleaning and Preparation
  lotto_clean <- data %>%
    filter(datum != "Datum") %>%  # remove repeated header rows
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
    filter(if_all(everything(), ~ !is.na(.)))  # remove rows with any NA
  
  lotto_clean_sorted <- lotto_clean %>%
    # sort balls row-wise
    rowwise() %>%
    mutate(
      sorted_balls = list(sort(c(ball_1, ball_2, ball_3, ball_4, ball_5, ball_6)))
    ) %>%
    mutate(
      ball_1 = sorted_balls[1],
      ball_2 = sorted_balls[2],
      ball_3 = sorted_balls[3],
      ball_4 = sorted_balls[4],
      ball_5 = sorted_balls[5],
      ball_6 = sorted_balls[6]
    ) %>%
    ungroup() %>%
    select(datum, ball_1, ball_2, ball_3, ball_4, ball_5, ball_6, superzahl)
  
  cat("Data loaded successfully:", nrow(lotto_clean_sorted), "rows\n")
  
}, error = function(e) {
  cat("ERROR LOADING DATA:", conditionMessage(e), "\n")
  stop(e)
})

# Gets Lottery data to work with
generate_metrics <- function() {
  return(lotto_clean_sorted)
}