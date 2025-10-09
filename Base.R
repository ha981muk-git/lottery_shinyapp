pkgs <- c("vroom","dplyr","janitor","shiny","bslib","shinyjs","plotly",
          "waiter", "readr", "tidyr", "purrr")
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



# Define consistent colors for each ball (using hex codes for transparency support)

# Define consistent colors for each ball (hex codes)
ball_colors <- c(
  "Ball 1" = "#4169E1",  # royal blue
  "Ball 2" = "#DC143C",  # crimson red
  "Ball 3" = "#32CD32",  # lime green
  "Ball 4" = "#FFD700",  # gold/yellow
  "Ball 5" = "#9370DB",  # medium purple
  "Ball 6" = "#00CED1"   # dark cyan
)




