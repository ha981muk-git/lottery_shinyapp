# -------------------------
# Top-level Server
# -------------------------
server <- function(input, output, session) {
  # Call input module
  input_controls <- lotteryInputServer("inputs1")
  
  # call modules
  dashboardServer("dashboard1", input_controls = input_controls)
  #  ballsMetricServer("dashboard1", input_controls = input_controls)
  #  gen_out <- generatorServer("gen1")  # returns list with selected_numbers reactive if needed
  #  statsServer("stats1")
  #  hotcoldServer("hc1")
}
