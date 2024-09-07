library(shiny)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Three Trials Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Trial Type"),
      selectInput("trialType1", "First Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("trialType2", "Second Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      selectInput("trialType3", "Third Trial Type:", choices = c("Coin Toss", "Dice Roll")),
      actionButton("reset", "Reset Selection")
    ),
    mainPanel(
      plotlyOutput("trialPlot", height = "600px", width = "100%")
    )
  )
)

server <- function(input, output, session) {

  # Reactive value to store selected points
  selected_outcomes <- reactiveVal(NULL)

  # Function to generate the 3D plot
  generate_plot <- function() {
    # Define the outcomes based on selected trial types
    outcomes1 <- if (input$trialType1 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes2 <- if (input$trialType2 == "Coin Toss") c("Head", "Tail") else as.character(1:6)
    outcomes3 <- if (input$trialType3 == "Coin Toss") c("Head", "Tail") else as.character(1:6)

    # Create a grid of outcomes for all trials
    outcomes <- expand.grid(
      First_Trial = outcomes1,
      Second_Trial = outcomes2,
      Third_Trial = outcomes3
    )

    # Add unique ID for each outcome
    outcomes$ID <- with(outcomes, paste(First_Trial, Second_Trial, Third_Trial))

    # Create selection status
    if (is.null(selected_outcomes())) {
      outcomes$Color <- "#D0D0D0"  # Light gray for non-selected
    } else {
      selected_ids <- selected_outcomes()$ID
      outcomes$Color <- ifelse(outcomes$ID %in% selected_ids, "#FF5733", "#D0D0D0")  # Orange for selected
    }

    # Create the 3D plot
    plot_ly(
      data = outcomes,
      x = ~First_Trial,
      y = ~Second_Trial,
      z = ~Third_Trial,
      color = ~Color,
      colors = c("#D0D0D0", "#FF5733"),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 10),
      source = "plot"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste(input$trialType1, "(First Trial)")),
          yaxis = list(title = paste(input$trialType2, "(Second Trial)")),
          zaxis = list(title = paste(input$trialType3, "(Third Trial)"))
        ),
        title = "Outcome Visualization for Three Trials",
        margin = list(l = 0, r = 0, b = 0, t = 30)
      )
  }

  # Render the plot based on user selection
  output$trialPlot <- renderPlotly({
    generate_plot()
  })

  # Handle clicks on the plot to toggle selection
  observeEvent(event_data("plotly_click", source = "plot"), {
    click <- event_data("plotly_click", source = "plot")

    if (is.null(click)) return()

    clicked_outcome <- data.frame(
      First_Trial = as.character(click$x),
      Second_Trial = as.character(click$y),
      Third_Trial = as.character(click$z),
      stringsAsFactors = FALSE
    )
    clicked_outcome$ID <- paste(clicked_outcome$First_Trial, clicked_outcome$Second_Trial, clicked_outcome$Third_Trial)

    # Update the selected outcomes
    current_selected <- selected_outcomes()
    if (is.null(current_selected)) {
      selected_outcomes(clicked_outcome)
    } else {
      selected_ids <- current_selected$ID
      if (clicked_outcome$ID %in% selected_ids) {
        selected_outcomes(current_selected[!(selected_ids %in% clicked_outcome$ID), ])
      } else {
        selected_outcomes(rbind(current_selected, clicked_outcome))
      }
    }

    output$trialPlot <- renderPlotly({
      generate_plot()
    })
  })

  # Reset selections when the reset button is clicked
  observeEvent(input$reset, {
    selected_outcomes(NULL)
    output$trialPlot <- renderPlotly({
      generate_plot()
    })
  })

  # Reset selections when trial types are changed
  observe({
    selected_outcomes(NULL)
    output$trialPlot <- renderPlotly({
      generate_plot()
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
