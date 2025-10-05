# -------------------------
# Module: generatorModule
# -------------------------
generatorUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "content-card",
      div(class = "card-title", "✨ Quick Pick Generator"),
      div(
        style = "text-align: center;",
        actionButton(ns("quick_pick"), "🎲 Generate Lucky Numbers", class = "btn-generate"),
        div(style = "margin-top: 40px;", uiOutput(ns("quick_pick_results")))
      )
    ),
    div(
      class = "content-card",
      div(class = "card-title", "🎯 Manual Selection"),
      div(class = "section-header", "Select Your 6 Numbers (1-49)"),
      div(
        class = "number-grid",
        # Use ns in onclick target to properly namespace toggle_num
        lapply(1:49, function(i) {
          tags$button(
            id = ns(paste0("num_", i)),
            class = "number-btn",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("toggle_num"), i),
            i
          )
        })
      ),
      div(id = ns("count"), class = "selection-count", "Selected: 0/6"),
      div(
        style = "text-align: center; margin-top: 30px; display: flex; gap: 15px; justify-content: center; flex-wrap: wrap;",
        actionButton(ns("submit"), "✓ Submit Numbers", class = "btn-action btn-success"),
        actionButton(ns("clear"), "✕ Clear All", class = "btn-action btn-warning")
      )
    )
  )
}

generatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_numbers <- reactiveVal(numeric(0))
    
    # Quick pick with waiter
    output$quick_pick_results <- renderUI({ NULL })
    observeEvent(input$quick_pick, {
      w <- Waiter$new(
        html = tagList(spin_fading_circles(), h4("Generating Lucky Numbers...", style = "color: white; margin-top: 20px;")),
        color = "rgba(10, 14, 39, 0.9)"
      )
      w$show()
      Sys.sleep(1)
      lucky_nums <- sort(sample(1:49, 6))
      output$quick_pick_results <- renderUI({
        div(class = "lucky-numbers", lapply(lucky_nums, function(n) div(class = "lucky-ball", n)))
      })
      w$hide()
      showNotification("Lucky numbers generated! Good luck! 🍀", type = "message", duration = 3)
    })
    
    # toggle selection via JS-set input (namespaced toggle_num)
    observeEvent(input$toggle_num, {
      num <- input$toggle_num
      current <- selected_numbers()
      btn_id <- ns(paste0("num_", num))
      
      if (num %in% current) {
        selected_numbers(current[current != num])
        shinyjs::removeClass(id = btn_id, class = "selected")
      } else if (length(current) < 6) {
        selected_numbers(c(current, num))
        shinyjs::addClass(id = btn_id, class = "selected")
      } else {
        showNotification("Maximum 6 numbers allowed!", type = "warning", duration = 2)
      }
      
      shinyjs::html(ns("count"), sprintf("Selected: %d/6", length(selected_numbers())))
    })
    
    observeEvent(input$clear, {
      current <- selected_numbers()
      lapply(current, function(n) {
        shinyjs::removeClass(id = ns(paste0("num_", n)), class = "selected")
      })
      selected_numbers(numeric(0))
      shinyjs::html(ns("count"), "Selected: 0/6")
    })
    
    observeEvent(input$submit, {
      nums <- selected_numbers()
      if (length(nums) < 6) {
        showNotification("Please select exactly 6 numbers!", type = "error", duration = 3)
      } else {
        showNotification(paste("Numbers submitted:", paste(sort(nums), collapse = ", "), "🎉"), type = "message", duration = 5)
      }
    })
    
    # expose selected numbers getter for potential use by other modules
    list(
      selected_numbers = reactive(selected_numbers())
    )
  })
}