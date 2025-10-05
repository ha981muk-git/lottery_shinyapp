# -------------------------
# Module: statsModule
# -------------------------
# depends on data frame lotto_clean_sorted to exist in global env
statsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/3,
      heights_equal = "row",
      uiOutput(ns("value_box_1")),
      uiOutput(ns("value_box_2")),
      uiOutput(ns("value_box_3"))
    ),
    div(class = "content-card",
        div(class = "card-title", "📈 Number Frequency Analysis"),
        plotlyOutput(ns("frequency_chart"), height = "450px")
    ),
    layout_column_wrap(
      width = 1/2,
      div(class = "content-card",
          div(class = "card-title", "⚖️ Even vs Odd Distribution"),
          plotlyOutput(ns("even_odd_chart"), height = "350px")
      ),
      div(class = "content-card",
          div(class = "card-title", "📊 Number Range Distribution"),
          plotlyOutput(ns("range_chart"), height = "350px")
      )
    )
  )
}

statsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # historical_data reactive — uses lotto_clean_sorted from global env
    historical_data <- reactive({
      # The app expects a data.frame 'lotto_clean_sorted' available in global env
      if (!exists("lotto_clean_sorted")) {
        # return a zero-filled table if data missing to avoid errors
        data.frame(number = 1:49, frequency = 0)
      } else {
        numbers <- unlist(lotto_clean_sorted %>% select(ball_1:ball_6))
        freq_table <- as.data.frame(table(numbers))
        colnames(freq_table) <- c("number", "frequency")
        freq_table$number <- as.numeric(as.character(freq_table$number))
        freq_table
      }
    })
    
    output$value_box_1 <- renderUI({
      div(class = "value-box-custom",
          div(class = "value-box-icon", "💾"),
          div(class = "value-box-value", "1,000"),
          div(class = "value-box-label", "Total Draws Analyzed")
      )
    })
    
    output$value_box_2 <- renderUI({
      data <- historical_data()
      most_common <- if (nrow(data) > 0) data$number[which.max(data$frequency)] else NA
      div(class = "value-box-custom",
          div(class = "value-box-icon", "⭐"),
          div(class = "value-box-value", most_common),
          div(class = "value-box-label", "Most Common Number")
      )
    })
    
    output$value_box_3 <- renderUI({
      data <- historical_data()
      least_common <- if (nrow(data) > 0) data$number[which.min(data$frequency)] else NA
      div(class = "value-box-custom",
          div(class = "value-box-icon", "❄️"),
          div(class = "value-box-value", least_common),
          div(class = "value-box-label", "Least Common Number")
      )
    })
    
    output$frequency_chart <- renderPlotly({
      data <- historical_data()
      plot_ly(data, x = ~number, y = ~frequency, type = 'bar',
              marker = list(
                color = ~frequency,
                colorscale = list(c(0, '#8b5cf6'), c(0.5, '#ec4899'), c(1, '#ffd700')),
                showscale = TRUE
              ),
              hovertemplate = paste("<b>Number:</b> %{x}<br><b>Frequency:</b> %{y}<extra></extra>")
      ) %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
        config(displayModeBar = FALSE)
    })
    
    output$even_odd_chart <- renderPlotly({
      data <- historical_data()
      even_sum <- sum(data$frequency[data$number %% 2 == 0])
      odd_sum <- sum(data$frequency[data$number %% 2 == 1])
      plot_ly(labels = c("Even", "Odd"), values = c(even_sum, odd_sum), type = 'pie',
              marker = list(colors = c('#8b5cf6', '#ec4899')),
              textfont = list(size = 16, color = 'white'),
              textinfo = 'label+percent',
              hovertemplate = paste("<b>%{label}</b><br>Count: %{value}<extra></extra>")) %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
        config(displayModeBar = FALSE)
    })
    
    output$range_chart <- renderPlotly({
      data <- historical_data()
      data$range <- cut(data$number, breaks = c(0, 10, 20, 30, 40, 49),
                        labels = c("1-10", "11-20", "21-30", "31-40", "41-49"))
      range_data <- aggregate(frequency ~ range, data, sum)
      plot_ly(range_data, x = ~range, y = ~frequency, type = 'bar') %>%
        layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
        config(displayModeBar = FALSE)
    })
  })
}