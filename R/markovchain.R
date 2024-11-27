# Load necessary libraries
library(shiny)
library(shinyMatrix)
library(ggplot2)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Markov Chain Simulation with Dynamic States"),

  sidebarLayout(
    sidebarPanel(
      numericInput("num_states", "Number of States:", value = 2, min = 2, step = 1, max=10),
      matrixInput("transition_matrix",
                  value = matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2, byrow = TRUE),
                  class = "numeric",
                  label = "Transition Matrix"),
      numericInput("n_steps", "Number of Steps:", value = 10, min = 1),
      uiOutput("initial_probs_ui"),
      actionButton("run_sim", "Run Simulation")
    ),

    mainPanel(
      plotOutput("prob_plot"),
      tableOutput("prob_table"),
      tableOutput("transition_table")  # Table to display rounded transition matrix
    )
  )
)

server <- function(input, output, session) {

  # Update transition matrix dimensions dynamically based on the number of states
  observeEvent(input$num_states, {
    num_states <- input$num_states
    initial_matrix <- matrix(1 / num_states, nrow = num_states, ncol = num_states)
    updateMatrixInput(session, "transition_matrix", value = initial_matrix)
  })

  # Dynamic UI for setting initial probabilities
  output$initial_probs_ui <- renderUI({
    lapply(1:input$num_states, function(i) {
      numericInput(paste0("initial_prob_", i),
                   paste("Initial Probability for State", i, ":"),
                   value = ifelse(i == 1, 1, 0), min = 0, max = 1)
    })
  })

  # Validation function for transition matrix
  validateTransitionMatrix <- reactive({
    matrix <- input$transition_matrix
    if (any(matrix < 0 | matrix > 1)) {
      showNotification("All entries in the transition matrix must be between 0 and 1.", type = "error")
      return(FALSE)
    }
    if (!all(abs(rowSums(matrix) - 1) < 1e-6)) {
      showNotification("Each row of the transition matrix must sum to 1.", type = "error")
      return(FALSE)
    }
    return(TRUE)
  })

  # Validation function for initial probabilities
  validateInitialProbs <- reactive({
    probs <- sapply(1:input$num_states, function(i) {
      as.numeric(input[[paste0("initial_prob_", i)]])
    })
    if (any(probs < 0 | probs > 1)) {
      showNotification("All initial probabilities must be between 0 and 1.", type = "error")
      return(FALSE)
    }
    if (abs(sum(probs) - 1) > 1e-6) {
      showNotification("The sum of initial probabilities must be 1.", type = "error")
      return(FALSE)
    }
    return(TRUE)
  })

  # Create initial probability vector based on user inputs
  initial_probs <- reactive({
    probs <- sapply(1:input$num_states, function(i) {
      as.numeric(input[[paste0("initial_prob_", i)]])
    })
    probs / sum(probs)
  })

  # Calculate probabilities over time when the button is clicked
  probs <- eventReactive(input$run_sim, {
    if (!validateTransitionMatrix() || !validateInitialProbs()) {
      return(NULL)
    }

    n_steps <- input$n_steps
    num_states <- input$num_states
    transition_matrix <- input$transition_matrix

    # Initialize matrix to store probabilities at each step
    prob_matrix <- matrix(0, nrow = n_steps, ncol = num_states)
    prob_matrix[1, ] <- initial_probs()

    # Iteratively calculate probabilities for each step without rounding
    for (i in 2:n_steps) {
      prob_matrix[i, ] <- prob_matrix[i - 1, ] %*% transition_matrix
    }

    prob_matrix
  })

  # Plot probabilities over time
  output$prob_plot <- renderPlot({
    req(probs())
    n_steps <- input$n_steps
    time <- 1:n_steps
    num_states <- input$num_states

    df <- data.frame(
      Step = rep(time, each = num_states),
      State = rep(paste("State", 1:num_states), times = n_steps),
      Probability = as.vector(t(probs()))
    )

    ggplot(df, aes(x = Step, y = Probability, color = State, group = State)) +
      geom_line(size = 1.2) +
      labs(title = "Probability of Being in Each State Over Time",
           x = "Step", y = "Probability") +
      theme_minimal() +
      scale_color_brewer(palette = "Dark2")
  })

  # Display probability table with rounded values only for display
  output$prob_table <- renderTable({
    req(probs())
    prob_df <- as.data.frame(probs())
    colnames(prob_df) <- paste("State", 1:input$num_states)
    prob_df <- cbind(Step = 1:nrow(prob_df), prob_df)
    prob_df <- prob_df %>% mutate(across(where(is.numeric), ~ format(round(., 4), nsmall = 4)))
    prob_df
  })

  # Display rounded transition matrix in a separate table without affecting actual values
  output$transition_table <- renderTable({
    rounded_matrix <- apply(input$transition_matrix, c(1, 2), function(x) format(round(x, 2), nsmall = 2))
    transition_df <- as.data.frame(rounded_matrix)
    colnames(transition_df) <- paste("State", 1:ncol(transition_df))
    rownames(transition_df) <- paste("State", 1:nrow(transition_df))
    transition_df
  }, rownames = TRUE, colnames = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
