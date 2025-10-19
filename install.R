pkgs <- c("shiny","vroom","dplyr","janitor","bslib","shinyjs","plotly",
          "waiter","tidyr","purrr","DT")

to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]

if(length(to_install)) install.packages(to_install)

lapply(pkgs, library, character.only = TRUE)
