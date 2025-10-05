# -------------------------
# Module: hotcoldModule
# -------------------------
hotcoldUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      div(class = "content-card",
          div(class = "card-title", "🔥 Hot Numbers"),
          div(class = "info-text", "Most frequently drawn numbers in last 100 draws"),
          uiOutput(ns("hot_numbers"))
      ),
      div(class = "content-card",
          div(class = "card-title", "❄️ Cold Numbers"),
          div(class = "info-text", "Least frequently drawn numbers in last 100 draws"),
          uiOutput(ns("cold_numbers"))
      )
    ),
    div(class = "content-card",
        div(class = "card-title", "📉 Hot vs Cold Trend"),
        plotlyOutput(ns("trend_chart"), height = "450px")
    )
  )
}

hotcoldServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    historical_data <- reactive({
      if (!exists("lotto_clean_sorted")) {
        data.frame(number = 1:49, frequency = 0)
      } else {
        numbers <- unlist(lotto_clean_sorted %>% select(ball_1:ball_6))
        freq_table <- as.data.frame(table(numbers))
        colnames(freq_table) <- c("number", "frequency")
        freq_table$number <- as.numeric(as.character(freq_table$number))
        freq_table
      }
    })
    
    output$hot_numbers <- renderUI({
      data <- historical_data()
      hot <- head(data[order(-data$frequency), ], 10)
      div(class = "number-container", lapply(hot$number, function(n) div(class = "hot-number", n)))
    })
    
    output$cold_numbers <- renderUI({
      data <- historical_data()
      cold <- head(data[order(data$frequency), ], 10)
      div(class = "number-container", lapply(cold$number, function(n) div(class = "cold-number", n)))
    })
    
    output$trend_chart <- renderPlotly({
      data <- historical_data()
      hot <- head(data[order(-data$frequency), ], 6)
      cold <- head(data[order(data$frequency), ], 6)
      plot_ly() %>%
        add_trace(x = hot$number, y = hot$frequency, type = 'scatter', mode = 'lines+markers', name = 'Hot Numbers') %>%
        add_trace(x = cold$number, y = cold$frequency, type = 'scatter', mode = 'lines+markers', name = 'Cold Numbers') %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
        config(displayModeBar = FALSE)
    })
  })
}
# -------------------------
# Module: hotcoldModule
# -------------------------
hotcoldUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      div(class = "content-card",
          div(class = "card-title", "🔥 Hot Numbers"),
          div(class = "info-text", "Most frequently drawn numbers in last 100 draws"),
          uiOutput(ns("hot_numbers"))
      ),
      div(class = "content-card",
          div(class = "card-title", "❄️ Cold Numbers"),
          div(class = "info-text", "Least frequently drawn numbers in last 100 draws"),
          uiOutput(ns("cold_numbers"))
      )
    ),
    div(class = "content-card",
        div(class = "card-title", "📉 Hot vs Cold Trend"),
        plotlyOutput(ns("trend_chart"), height = "450px")
    )
  )
}

hotcoldServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    historical_data <- reactive({
      if (!exists("lotto_clean_sorted")) {
        data.frame(number = 1:49, frequency = 0)
      } else {
        numbers <- unlist(lotto_clean_sorted %>% select(ball_1:ball_6))
        freq_table <- as.data.frame(table(numbers))
        colnames(freq_table) <- c("number", "frequency")
        freq_table$number <- as.numeric(as.character(freq_table$number))
        freq_table
      }
    })
    
    output$hot_numbers <- renderUI({
      data <- historical_data()
      hot <- head(data[order(-data$frequency), ], 10)
      div(class = "number-container", lapply(hot$number, function(n) div(class = "hot-number", n)))
    })
    
    output$cold_numbers <- renderUI({
      data <- historical_data()
      cold <- head(data[order(data$frequency), ], 10)
      div(class = "number-container", lapply(cold$number, function(n) div(class = "cold-number", n)))
    })
    
    output$trend_chart <- renderPlotly({
      data <- historical_data()
      hot <- head(data[order(-data$frequency), ], 6)
      cold <- head(data[order(data$frequency), ], 6)
      plot_ly() %>%
        add_trace(x = hot$number, y = hot$frequency, type = 'scatter', mode = 'lines+markers', name = 'Hot Numbers') %>%
        add_trace(x = cold$number, y = cold$frequency, type = 'scatter', mode = 'lines+markers', name = 'Cold Numbers') %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
        config(displayModeBar = FALSE)
    })
  })
}