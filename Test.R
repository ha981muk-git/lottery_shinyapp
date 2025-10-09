library(shiny)
library(jsonlite)

# Load translations JSON
translations <- fromJSON("~/drive/workspace/global/code/R/lottery_shinyapp_v1/translations/translation.json")

# Function to get translation by key
get_translation <- function(key, lang = "en") {
  if (!lang %in% names(translations)) stop("Language not found")
  if (!key %in% names(translations[[lang]])) return(key)
  translations[[lang]][[key]]
}

ui <- fluidPage(
  # Language selector
  selectInput("lang", "Choose language", choices = names(translations)),
  
  # UI elements
  h2(textOutput("greeting")),
  h3(textOutput("farewell")),
  p(textOutput("file_label"))
)

server <- function(input, output, session) {
  
  # Reactive translations
  output$greeting <- renderText({
    get_translation("greeting", input$lang)
  })
  
  output$farewell <- renderText({
    get_translation("farewell", input$lang)
  })
  
  output$file_label <- renderText({
    get_translation("file_select", input$lang)
  })
}

shinyApp(ui, server)
