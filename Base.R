pkgs <- c("vroom","dplyr","janitor","shiny","bslib","shinyjs","plotly",
          "waiter", "readr", "tidyr", "purrr", "ggplot2", "qgraph", "brms")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# ---------- UI helper theme ----------
app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#0a0e27",
  fg = "#e8eaed",
  primary = "#8b5cf6",
  secondary = "#ec4899",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

# Gets Lottery data to work with
generate_metrics <- function() {
  return(lotto_clean_sorted)
}





