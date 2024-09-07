library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Two Trials Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Trial Type"),
      selectInput("trialType1", "First Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("trialType2", "Second Trial Type:", choices = c("Coin Toss", "Dice Roll")),

      uiOutput("trial1Choices"),
      uiOutput("trial2Choices"),

      actionButton("refresh", "Refresh Visualization")
    ),
    mainPanel(
      plotOutput("trialPlot"),
      br(),
      h4("Probability Calculations"),
      textOutput("totalOutcomes"),
      textOutput("favorableOutcomes"),
      textOutput("probability")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {

  # Dynamically generate choices based on selected trial type
  output$trial1Choices <- renderUI({
    if (input$trialType1 == "Coin Toss") {
      checkboxGroupInput("trial1", "First Trial Outcomes:", choices = c("Head", "Tail"))
    } else {
      checkboxGroupInput("trial1", "First Trial Numbers:", choices = as.character(1:6))
    }
  })

  output$trial2Choices <- renderUI({
    if (input$trialType2 == "Coin Toss") {
      checkboxGroupInput("trial2", "Second Trial Outcomes:", choices = c("Head", "Tail"))
    } else {
      checkboxGroupInput("trial2", "Second Trial Numbers:", choices = as.character(1:6))
    }
  })

  # Calculate and display the probability
  output$totalOutcomes <- renderText({
    total_outcomes <- (if (input$trialType1 == "Coin Toss") 2 else 6) * (if (input$trialType2 == "Coin Toss") 2 else 6)
    paste("Total Number of Outcomes: ", total_outcomes)
  })

  output$favorableOutcomes <- renderText({
    outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes <- expand.grid(First_Trial = outcomes1, Second_Trial = outcomes2)
    selected_event <- with(outcomes, First_Trial %in% input$trial1 & Second_Trial %in% input$trial2)
    favorable_outcomes <- sum(selected_event)
    paste("Favorable Number of Outcomes: ", favorable_outcomes)
  })

  output$probability <- renderText({
    total_outcomes <- (if (input$trialType1 == "Coin Toss") 2 else 6) * (if (input$trialType2 == "Coin Toss") 2 else 6)
    outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes <- expand.grid(First_Trial = outcomes1, Second_Trial = outcomes2)
    selected_event <- with(outcomes, First_Trial %in% input$trial1 & Second_Trial %in% input$trial2)
    favorable_outcomes <- sum(selected_event)
    probability <- favorable_outcomes / total_outcomes
    paste("Probability: ", round(probability, 4))
  })

  # Render the plot based on user selection
  output$trialPlot <- renderPlot({

    # Define the outcomes based on selected trial types
    outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

    # Create a grid of outcomes for both trials
    outcomes <- expand.grid(
      First_Trial = outcomes1,
      Second_Trial = outcomes2
    )

    # Determine which outcomes are selected by the user
    selected_event <- with(outcomes,
                           First_Trial %in% input$trial1 & Second_Trial %in% input$trial2)
    outcomes$Color <- ifelse(selected_event, "#FF5733", "#D0D0D0")  # Vibrant orange for selected, light gray for others

    # Create the plot
    ggplot(outcomes, aes(x = First_Trial, y = Second_Trial)) +
      geom_tile(aes(fill = Color), color = "black", width = 1, height = 1) +  # Black border for better contrast
      scale_fill_identity() +
      theme_minimal() +
      labs(x = paste(input$trialType1, "(First Trial)"),
           y = paste(input$trialType2, "(Second Trial)"),
           title = "Outcome Visualization for Two Trials") +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            panel.grid = element_blank(),  # Remove grid lines
            axis.ticks = element_blank(),  # Remove axis ticks
            plot.margin = margin(0, 0, 0, 0))  # Remove plot margins
  })

  # Refresh the plot and recalculate probabilities when the button is clicked
  observeEvent(input$refresh, {
    output$trialPlot <- renderPlot({

      # Define the outcomes based on selected trial types
      outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
      outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

      # Create a grid of outcomes for both trials
      outcomes <- expand.grid(
        First_Trial = outcomes1,
        Second_Trial = outcomes2
      )

      # Determine which outcomes are selected by the user
      selected_event <- with(outcomes,
                             First_Trial %in% input$trial1 & Second_Trial %in% input$trial2)
      outcomes$Color <- ifelse(selected_event, "#FF5733", "#D0D0D0")  # Vibrant orange for selected, light gray for others

      # Create the plot
      ggplot(outcomes, aes(x = First_Trial, y = Second_Trial)) +
        geom_tile(aes(fill = Color), color = "black", width = 1, height = 1) +  # Black border for better contrast
        scale_fill_identity() +
        theme_minimal() +
        labs(x = paste(input$trialType1, "(First Trial)"),
             y = paste(input$trialType2, "(Second Trial)"),
             title = "Outcome Visualization for Two Trials") +
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              panel.grid = element_blank(),  # Remove grid lines
              axis.ticks = element_blank(),  # Remove axis ticks
              plot.margin = margin(0, 0, 0, 0))  # Remove plot margins
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
