library(shiny)
library(ggplot2)
library(dplyr)

#' Run the Two Events Visualization Shiny App
#'
#' This function launches a Shiny app that visualizes the outcomes of two events (Coin Toss or Dice Roll).
#' Users can select event types, choose outcomes, and interactively visualize probabilities and regions.
#'
#' Features:
#' - Select between "Coin Toss" or "Dice Roll" for two events.
#' - Choose specific outcomes for each event or explore an interactive mode.
#' - Visualize the probability and outcomes on a 2D plot.
#'
#' @return A Shiny app instance that runs locally in your browser.
#'
#' @import ggplot2
#' @import shiny
#' @import dplyr
#' @examples
#' if (interactive()) {
#'   runTwoEventsApp()
#' }
#'
#' @export
runTwoEventsApp <- function() {

  ui <- fluidPage(
  titlePanel("Two Events Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Event Type"),
      checkboxInput("interactive", "Interactive Mode", FALSE),
      selectInput("eventType1", "First Event Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("eventType2", "Second Event Type:", choices = c("Coin Toss", "Dice Roll")),

      uiOutput("event1Choices"),  # Outcome selection for Event 1
      uiOutput("event2Choices"),  # Outcome selection for Event 2

      actionButton("reset", "Reset Selection")
    ),
    mainPanel(
      plotOutput("eventPlot", hover = hoverOpts(id = "plot_hover"), click = "plot_click"),
      br(),
      h4("Probability Calculations"),
      textOutput("totalOutcomes"),
      textOutput("favorableOutcomes"),
      textOutput("probability"),
      br(),
      textOutput("hoverInfo")  # Hover info display
    )
  )
)


server <- function(input, output, session) {


  selected_outcomes <- reactiveVal(data.frame(First_Event = character(), Second_Event = character(), stringsAsFactors = FALSE))


  output$event1Choices <- renderUI({
    if (!input$interactive) {
      if (input$eventType1 == "Coin Toss") {
        checkboxGroupInput("event1", "First Event Outcomes:", choices = c("Head", "Tail"))
      } else {
        checkboxGroupInput("event1", "First Event Numbers:", choices = as.character(1:6))
      }
    }
  })

  output$event2Choices <- renderUI({
    if (!input$interactive) {
      if (input$eventType2 == "Coin Toss") {
        checkboxGroupInput("event2", "Second Event Outcomes:", choices = c("Head", "Tail"))
      } else {
        checkboxGroupInput("event2", "Second Event Numbers:", choices = as.character(1:6))
      }
    }
  })

  observeEvent(list(input$eventType1, input$eventType2, input$reset), {
    selected_outcomes(data.frame(First_Event = character(), Second_Event = character(), stringsAsFactors = FALSE))
  })

  observeEvent(input$plot_click, {
    if (input$interactive) {
      click_data <- input$plot_click

      outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)


      clicked_event1 <- round(click_data$x)
      clicked_event2 <- round(click_data$y)


      if (clicked_event1 %in% 1:length(outcomes1) && clicked_event2 %in% 1:length(outcomes2)) {
        clicked_outcome1 <- outcomes1[clicked_event1]
        clicked_outcome2 <- outcomes2[clicked_event2]

        selected <- selected_outcomes()
        new_selection <- data.frame(First_Event = clicked_outcome1, Second_Event = clicked_outcome2, stringsAsFactors = FALSE)


        if (any(selected$First_Event == clicked_outcome1 & selected$Second_Event == clicked_outcome2)) {
          selected <- selected %>%
            filter(!(First_Event == clicked_outcome1 & Second_Event == clicked_outcome2))
        } else {
          selected <- rbind(selected, new_selection)
        }

        selected_outcomes(selected)
      }
    }
  })

  output$totalOutcomes <- renderText({
    total_outcomes <- (if (input$eventType1 == "Coin Toss") 2 else 6) * (if (input$eventType2 == "Coin Toss") 2 else 6)
    paste("Total Number of Regions: ", total_outcomes)
  })


  output$favorableOutcomes <- renderText({
    if (input$interactive) {
      favorable_outcomes <- nrow(selected_outcomes())
    } else {
      outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes <- expand.grid(First_Event = outcomes1, Second_Event = outcomes2)
      selected_event <- with(outcomes, First_Event %in% input$event1 & Second_Event %in% input$event2)
      favorable_outcomes <- sum(selected_event)
    }
    paste("Selected Number of Regions: ", favorable_outcomes)
  })

  output$probability <- renderText({
    total_outcomes <- (if (input$eventType1 == "Coin Toss") 2 else 6) * (if (input$eventType2 == "Coin Toss") 2 else 6)
    if (input$interactive) {
      favorable_outcomes <- nrow(selected_outcomes())
    } else {
      outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes <- expand.grid(First_Event = outcomes1, Second_Event = outcomes2)
      selected_event <- with(outcomes, First_Event %in% input$event1 & Second_Event %in% input$event2)
      favorable_outcomes <- sum(selected_event)
    }
    probability <- favorable_outcomes / total_outcomes
    paste("Probability: ", round(probability, 4))
  })

  output$hoverInfo <- renderText({
    hover_data <- input$plot_hover

    if (!is.null(hover_data)) {
      outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

      # Get the hovered point
      hovered_event1 <- round(hover_data$x)
      hovered_event2 <- round(hover_data$y)

      if (hovered_event1 %in% 1:length(outcomes1) && hovered_event2 %in% 1:length(outcomes2)) {
        paste0("Hovered Outcome: (", outcomes1[hovered_event1], ", ", outcomes2[hovered_event2], ")")
      }
    }
  })


  output$eventPlot <- renderPlot({
    outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

    outcomes <- expand.grid(First_Event = outcomes1, Second_Event = outcomes2)

    outcomes$Color <- "#D0D0D0"

    if (input$interactive) {
      selected <- selected_outcomes()
      for (i in 1:nrow(selected)) {
        outcomes$Color[outcomes$First_Event == selected$First_Event[i] & outcomes$Second_Event == selected$Second_Event[i]] <- "#FF5733"
      }
    } else {
      selected_event <- with(outcomes, First_Event %in% input$event1 & Second_Event %in% input$event2)
      outcomes$Color[selected_event] <- "#FF5733"
    }

    ggplot(outcomes, aes(x = as.factor(First_Event), y = as.factor(Second_Event))) +
      geom_tile(aes(fill = Color), color = "black", width = 1, height = 1) +
      scale_fill_identity() +
      theme_minimal() +
      labs(x = paste(input$eventType1, "(First Event)"),
           y = paste(input$eventType2, "(Second Event)"),
           title = "Outcome Visualization for Two Events") +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            panel.grid = element_blank())
  })
}

  shinyApp(ui = ui, server = server)
}

