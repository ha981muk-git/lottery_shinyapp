# ==== Loading data
# data <- read_delim(
#   "~/drive/workspace/global/code/R/lottery_bayesian/LOTTO_ab_2018.csv",
#   delim = ";",
#   trim_ws = TRUE
# )

# ==== Cleaning and Preparation
data <- vroom("~/drive/workspace/global/code/R/lottery_bayesian/LOTTO_ab_2018.csv", delim = ";",
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

# Quick check
nrow(data)

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
  select(datum, ball_1, ball_2, ball_3, ball_4, ball_5, ball_6, superzahl) #%>% # keep date
  # mutate(draw_number = row_number()) # sequential numbering



