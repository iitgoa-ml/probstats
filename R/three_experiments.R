

#' Run the Three Events Visualization Shiny App
#'
#' This function launches a Shiny app that visualizes the outcomes of three experiments (Coin Toss or Dice Roll).
#' Users can select experiment types and outcomes, and visualize probabilities and selected outcomes in a 3D plot.
#'
#' Features:
#' - Select between "Coin Toss" or "Dice Roll" for three different experiments.
#' - Choose specific outcomes for each event.
#' - Visualize the probability of selected outcomes and display a 3D scatter plot of possible results.
#'
#' @return A Shiny app instance that runs locally in your browser.
#'
#' @import shiny
#' @import plotly
#' @examples
#' \dontrun{
#' run_three_experiments_app() # Run shiny app
#' }
#'
#' @export
run_three_experiments_app <- function() {

  library(shiny)
  library(plotly)


# Define the UI
ui <- fluidPage(
  titlePanel("Three Experiments Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Event Type"),
      selectInput("eventType1", "First Experiment Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("eventType2", "Second Experiment Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("eventType3", "Third Experiment Type:", choices = c("Coin Toss", "Dice Roll")),

      uiOutput("event1Choices"),
      uiOutput("event2Choices"),
      uiOutput("event3Choices"),

      actionButton("refresh", "Refresh Visualization", class = "btn btn-primary")
    ),
    mainPanel(
      plotlyOutput("eventPlot", height = "600px", width = "100%"),
      br(),
      h4("Event Statistics"),
      fluidRow(
        column(4,
               div(class = "card text-white bg-success mb-3", style = "max-width: 18rem;",
                   div(class = "card-header", "Favorable Outcomes"),
                   div(class = "card-body",
                       h5(class = "card-title", textOutput("favorableOutcomes")),
                       p("Number of selected outcomes that match your criteria.")
                   )
               )
        ),
        column(4,
               div(class = "card text-white bg-info mb-3", style = "max-width: 18rem;",
                   div(class = "card-header", "Total Outcomes Possible"),
                   div(class = "card-body",
                       h5(class = "card-title", textOutput("totalOutcomes")),
                       p("Total possible outcomes based on selected event types.")
                   )
               )
        ),
        column(4,
               div(class = "card text-white bg-warning mb-3", style = "max-width: 18rem;",
                   div(class = "card-header", "Probability of Event"),
                   div(class = "card-body",
                       h5(class = "card-title", textOutput("eventProbability")),
                       p("Probability of the event (favorable outcomes / total outcomes).")
                   )
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Dynamically generate choices based on selected event type
  output$event1Choices <- renderUI({
    if (input$eventType1 == "Coin Toss") {
      checkboxGroupInput("event1", "First Experiment Outcomes:", choices = c("Head", "Tail"))
    } else {
      checkboxGroupInput("event1", "First Experiment Outcomes:", choices = as.character(1:6))
    }
  })

  output$event2Choices <- renderUI({
    if (input$eventType2 == "Coin Toss") {
      checkboxGroupInput("event2", "Second Experiment Outcomes:", choices = c("Head", "Tail"))
    } else {
      checkboxGroupInput("event2", "Second Experiment Outcomes:", choices = as.character(1:6))
    }
  })

  output$event3Choices <- renderUI({
    if (input$eventType3 == "Coin Toss") {
      checkboxGroupInput("event3", "Third Experiment Outcomes:", choices = c("Head", "Tail"))
    } else {
      checkboxGroupInput("event3", "Third Experiment Outcomes:", choices = as.character(1:6))
    }
  })

  # Function to generate the 3D plot and calculate event statistics
  generate_plot <- function() {
    # Define the outcomes based on selected event types
    outcomes1 <- if (input$eventType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$eventType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes3 <- if (input$eventType3 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

    # Create a grid of outcomes for all events
    outcomes <- expand.grid(
      First_Event = outcomes1,
      Second_Event = outcomes2,
      Third_Event = outcomes3
    )

    # Determine which outcomes are selected by the user
    selected_event <- with(outcomes,
                           (First_Event %in% input$event1) &
                             (Second_Event %in% input$event2) &
                             (Third_Event %in% input$event3))

    # Set color based on selection
    outcomes$Color <- ifelse(selected_event, "#FF5733", "#D0D0D0")  # Vibrant orange for selected, light gray for non-selected

    # Calculate event statistics
    favorable_count <- sum(selected_event)
    total_count <- nrow(outcomes)
    event_probability <- favorable_count / total_count

    # Update text outputs for statistics
    output$favorableOutcomes <- renderText({
      favorable_count
    })

    output$totalOutcomes <- renderText({
      total_count
    })

    output$eventProbability <- renderText({
      round(event_probability, 4)
    })

    # Create the 3D plot
    plot_ly(
      data = outcomes,
      x = ~First_Event,
      y = ~Second_Event,
      z = ~Third_Event,
      color = ~Color,
      colors = c("#D0D0D0", "#FF5733"),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 15)  # Adjust marker size for better visibility
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste(input$trialType1, "(First Experiment)")),
          yaxis = list(title = paste(input$trialType2, "(Second Experiment)")),
          zaxis = list(title = paste(input$trialType3, "(Third Experiment)"))
        ),
        title = "Outcome Visualization for Three Events",
        margin = list(l = 0, r = 0, b = 0, t = 30)  # Adjust margins to give more space
      )
  }

  # Render the plot based on user selection
  output$eventPlot <- renderPlotly({
    generate_plot()
  })

  # Refresh the plot when the button is clicked
  observeEvent(input$refresh, {
    output$eventPlot <- renderPlotly({
      generate_plot()
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
}
run_three_experiments_app()
