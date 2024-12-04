# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("Visualization of Conditional and Marginal Probabilities"),

  sidebarLayout(
    sidebarPanel(
      h4("Input Parameters"),

      # Define inputs for the joint distribution
      sliderInput("p_A", "P(A)", min = 0, max = 1, value = 0.4, step = 0.01),
      sliderInput("p_B_given_A", "P(B | A)", min = 0, max = 1, value = 0.6, step = 0.01),
      sliderInput("p_B_given_not_A", "P(B | Not A)", min = 0, max = 1, value = 0.3, step = 0.01),

      # Allow the user to highlight probabilities
      checkboxInput("highlight_condition", "Highlight Conditional Probabilities", value = TRUE),
      checkboxInput("show_marginals", "Show Marginal Probabilities", value = TRUE)
    ),

    mainPanel(
      h4("Visualization"),
      plotOutput("probabilityPlot", height = "600px"),
      br(),
      h4("Calculations"),
      tableOutput("probTable"),
      p("This visualization helps understand relationships between conditional, marginal, and joint probabilities.")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive data frame to calculate probabilities
  probabilities <- reactive({
    p_A <- input$p_A
    p_B_given_A <- input$p_B_given_A
    p_B_given_not_A <- input$p_B_given_not_A

    # Joint probabilities
    p_AB <- p_A * p_B_given_A
    p_AcB <- (1 - p_A) * p_B_given_not_A

    # Marginals
    p_B <- p_AB + p_AcB
    p_not_B <- 1 - p_B

    data.frame(
      Event = c("P(A and B)", "P(A and Not B)", "P(Not A and B)", "P(Not A and Not B)", "P(B)", "P(Not B)"),
      Probability = c(
        p_AB,
        p_A - p_AB,
        p_AcB,
        1 - p_A - p_AcB,
        p_B,
        p_not_B
      )
    )
  })

  # Plot probabilities
  output$probabilityPlot <- renderPlot({
    df <- probabilities()
    bar_data <- data.frame(
      Event = c("A and B", "A and Not B", "Not A and B", "Not A and Not B"),
      Probability = df$Probability[1:4]
    )

    ggplot(bar_data, aes(x = Event, y = Probability, fill = Event)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Joint, Conditional, and Marginal Probabilities",
           x = "Events",
           y = "Probability",
           fill = "Event") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16)
      ) +
      if (input$highlight_condition) {
        geom_text(aes(label = paste0(round(Probability * 100, 1), "%")), vjust = -0.5, size = 5)
      }
  })

  # Display probabilities in table format
  output$probTable <- renderTable({
    probabilities()
  }, align = 'c')
}

# Run the application
shinyApp(ui = ui, server = server)
