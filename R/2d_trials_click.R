library(shiny)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Two Trials Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Trial Type"),
      selectInput("trialType1", "First Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("trialType2", "Second Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      actionButton("reset", "Reset Selection")
    ),
    mainPanel(
      plotOutput("trialPlot", click = "plot_click"),
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

  # Reactive value to store the selected outcomes
  selected_outcomes <- reactiveVal(data.frame(First_Trial = character(), Second_Trial = character(), stringsAsFactors = FALSE))

  # Reset selections when trial types change
  observeEvent(list(input$trialType1, input$trialType2), {
    selected_outcomes(data.frame(First_Trial = character(), Second_Trial = character(), stringsAsFactors = FALSE))
  })

  # Update the selections based on clicks
  observeEvent(input$plot_click, {
    click_data <- input$plot_click

    # Determine the trial types
    outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

    # Get the clicked point
    clicked_trial1 <- round(click_data$x)
    clicked_trial2 <- round(click_data$y)

    # Check if the click is within the valid region
    if (clicked_trial1 %in% 1:length(outcomes1) && clicked_trial2 %in% 1:length(outcomes2)) {
      clicked_outcome1 <- outcomes1[clicked_trial1]
      clicked_outcome2 <- outcomes2[clicked_trial2]

      selected <- selected_outcomes()
      new_selection <- data.frame(First_Trial = clicked_outcome1, Second_Trial = clicked_outcome2, stringsAsFactors = FALSE)

      # Toggle selection (add if not selected, remove if already selected)
      if (any(selected$First_Trial == clicked_outcome1 & selected$Second_Trial == clicked_outcome2)) {
        selected <- selected %>%
          filter(!(First_Trial == clicked_outcome1 & Second_Trial == clicked_outcome2))
      } else {
        selected <- rbind(selected, new_selection)
      }

      selected_outcomes(selected)
    }
  })

  # Reset selections when the reset button is clicked
  observeEvent(input$reset, {
    selected_outcomes(data.frame(First_Trial = character(), Second_Trial = character(), stringsAsFactors = FALSE))
  })

  # Calculate and display the probability
  output$totalOutcomes <- renderText({
    total_outcomes <- (if (input$trialType1 == "Coin Toss") 2 else 6) * (if (input$trialType2 == "Coin Toss") 2 else 6)
    paste("Total Number of Regions: ", total_outcomes)
  })

  output$favorableOutcomes <- renderText({
    favorable_outcomes <- nrow(selected_outcomes())
    paste("Selected Number of Regions: ", favorable_outcomes)
  })

  output$probability <- renderText({
    total_outcomes <- (if (input$trialType1 == "Coin Toss") 2 else 6) * (if (input$trialType2 == "Coin Toss") 2 else 6)
    favorable_outcomes <- nrow(selected_outcomes())
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

    # Initialize all colors to light gray
    outcomes$Color <- "#D0D0D0"

    # Update colors for selected pairs only
    selected <- selected_outcomes()
    for (i in 1:nrow(selected)) {
      outcomes$Color[outcomes$First_Trial == selected$First_Trial[i] & outcomes$Second_Trial == selected$Second_Trial[i]] <- "#FF5733"
    }

    # Create the plot
    ggplot(outcomes, aes(x = as.factor(First_Trial), y = as.factor(Second_Trial))) +
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
}

# Run the app
shinyApp(ui = ui, server = server)
