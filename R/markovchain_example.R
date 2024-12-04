#' Learn Markov Chains: State Transition Visualizer
#'
#' This Shiny app provides an interactive way to explore and understand Markov Chains
#' using a relatable example of weather states (Sunny, Cloudy, Rainy).
#' Users can configure the transition matrix, set initial probabilities,
#' and observe how the probabilities of states evolve over time.
#'
#' The app helps users grasp core Markov Chain concepts such as state transitions,
#' steady-state behavior, and the impact of initial conditions on long-term dynamics.
#'
#' Key features:
#' - Validate the transition matrix and initial probabilities.
#' - Visualize the evolution of state probabilities over time.
#' - Analyze steady-state probabilities using eigenvalues or linear equations.
#' - Dynamic visualizations of state transitions, probability plots, and distributions.
#'
#' @import shiny
#' @import ggplot2
#' @import shinyMatrix
#' @import dplyr
#' @import igraph
#' @import DT
#' @examples
#' \dontrun{
#' run_markov_chain_visualizer() # Launches the Markov Chain Visualizer Shiny app.
#' }
#'
#' @export
run_markov_chain_visualizer <- function(){
  library(shiny)
  library(ggplot2)
  library(shinyMatrix)
  library(dplyr)
  library(igraph)
  library(DT)

  # Define UI
  ui <- fluidPage(
    titlePanel("Markov Chains: State Transition Visualizer"),

    sidebarLayout(
      sidebarPanel(
        h3("Input Parameters"),
        matrixInput("transition_matrix",
                    value = matrix(c(0.7, 0.2, 0.1,
                                     0.3, 0.4, 0.3,
                                     0.2, 0.3, 0.5), nrow = 3, byrow = TRUE),
                    class = "numeric",
                    rows = list(names = TRUE, labels = c("Sunny", "Cloudy", "Rainy")),
                    cols = list(names = TRUE, labels = c("Sunny", "Cloudy", "Rainy")),
                    label = "Transition Matrix (Rows: From, Columns: To)"),

        numericInput("initial_sunny", "Initial Probability of Sunny:", value = 0.5, min = 0, max = 1, step=0.1),
        numericInput("initial_cloudy", "Initial Probability of Cloudy:", value = 0.3, min = 0, max = 1, step=0.1),
        numericInput("initial_rainy", "Initial Probability of Rainy:", value = 0.2, min = 0, max = 1, step=0.1),
        numericInput("n_steps", "Number of Steps:", value = 20, min = 1, step=1),
        actionButton("run_sim", "Run Simulation"),
        br(), br(),
        verbatimTextOutput("steady_state"),
        verbatimTextOutput("conclusion"),
        br(), br(),
        h4("Relationship between Initial and Nth Step Probability"),

        uiOutput("relationship_text")

      ),

      mainPanel(
        h3("Visualizations"),
        fluidRow(
          column(6, plotOutput("initial_pie_chart")),  # Initial state pie chart on the left
          column(6, plotOutput("final_pie_chart"))     # Final state pie chart on the right
        ),
        #plotOutput("initial_pie_chart"),
        #plotOutput("final_pie_chart"),  # New final state pie chart
        plotOutput("state_diagram"),
        plotOutput("prob_plot"),

        DTOutput("cumulative_table")
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {

    # Validate transition matrix and initial probabilities
    validateInputs <- reactive({
      matrix <- input$transition_matrix
      if (any(matrix < 0 | matrix > 1)) {
        showNotification("All transition matrix values must be between 0 and 1.", type = "error")
        return(FALSE)
      }
      if (!all(abs(rowSums(matrix) - 1) < 1e-6)) {
        showNotification("Each row of the transition matrix must sum to 1.", type = "error")
        return(FALSE)
      }
      initial_probs <- c(input$initial_sunny, input$initial_cloudy, input$initial_rainy)
      if (any(initial_probs < 0 | initial_probs > 1) || abs(sum(initial_probs) - 1) > 1e-6) {
        showNotification("Initial probabilities must sum to 1 and lie between 0 and 1.", type = "error")
        return(FALSE)
      }
      return(TRUE)
    })

    # Define initial probability vector
    initial_probs <- reactive({
      c(input$initial_sunny, input$initial_cloudy, input$initial_rainy)
    })

    # Calculate probabilities over time
    probs <- eventReactive(input$run_sim, {
      if (!validateInputs()) return(NULL)

      n_steps <- input$n_steps
      transition_matrix <- input$transition_matrix
      num_states <- 3

      prob_matrix <- matrix(0, nrow = n_steps, ncol = num_states)
      prob_matrix[1, ] <- initial_probs()

      for (i in 2:n_steps) {
        prob_matrix[i, ] <- prob_matrix[i - 1, ] %*% transition_matrix
      }

      colnames(prob_matrix) <- c("Sunny", "Cloudy", "Rainy")
      prob_matrix
    })

    # Initial Probability Pie Chart
    output$initial_pie_chart <- renderPlot({
      initial_probs <- initial_probs()
      labels <- c("Sunny", "Cloudy", "Rainy")
      pie_data <- data.frame(State = labels, Probability = initial_probs)

      ggplot(pie_data, aes(x = "", y = Probability, fill = State)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Initial State Probability Distribution") +
        theme_void()
    })
    # Final State Probability Pie Chart
    output$final_pie_chart <- renderPlot({
      req(probs())
      final_probs <- tail(probs(), 1)  # Get the probabilities from the last step
      labels <- c("Sunny", "Cloudy", "Rainy")
      pie_data <- data.frame(State = labels, Probability = as.numeric(final_probs))

      ggplot(pie_data, aes(x = "", y = Probability, fill = State)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Final State Probability Distribution") +
        theme_void()
    })

    # Render probability plot
    output$prob_plot <- renderPlot({
      req(probs())
      n_steps <- input$n_steps
      time <- 1:n_steps
      df <- data.frame(
        Step = rep(time, each = 3),
        State = rep(c("Sunny", "Cloudy", "Rainy"), times = n_steps),
        Probability = as.vector(t(probs()))
      )

      ggplot(df, aes(x = Step, y = Probability, color = State, group = State)) +
        geom_line(size = 1.2) +
        labs(title = "Weather State Probabilities Over Time",
             x = "Step", y = "Probability") +
        theme_minimal()
    })

    # Steady-State Analysis Using Eigenvalue Method
    output$steady_state <- renderText({
      transition_matrix <- input$transition_matrix

      # Calculate the eigenvalues and eigenvectors
      eig <- eigen(t(transition_matrix))
      eig_values <- eig$values
      eig_vectors <- eig$vectors

      # Find the eigenvector corresponding to the eigenvalue equal to 1
      steady_state_vector <- eig_vectors[, which(abs(eig_values - 1) < 1e-6)]

      # Normalize the eigenvector to form a probability distribution
      steady_state_probs <- steady_state_vector / sum(steady_state_vector)

      # Check if the result is a valid probability distribution
      if (all(steady_state_probs >= 0) && abs(sum(steady_state_probs) - 1) < 1e-6) {
        # Return the formatted steady-state probabilities
        paste0("Steady-State Probabilities: Sunny = ", round(steady_state_probs[1], 3),
               ", Cloudy = ", round(steady_state_probs[2], 3),
               ", Rainy = ", round(steady_state_probs[3], 3))
      } else {
        "Steady-state does not exist or cannot be computed."
      }
    })
    # Steady-State Analysis Using Linear Equations
    # Steady-State Analysis Combining Linear Equations and Eigenvalue Method
    output$steady_state <- renderText({
      req(probs())
      req(input$transition_matrix)

      # Extract the transition matrix from input
      transition_matrix <- input$transition_matrix

      # Validate the matrix dimensions
      if (nrow(transition_matrix) != ncol(transition_matrix)) {
        return("The transition matrix must be square.")
      }

      num_states <- nrow(transition_matrix)

      ### Method 1: Linear Equations
      try_linear_method <- function() {
        # Construct the linear system for steady-state
        A <- t(transition_matrix) - diag(num_states)  # Subtract identity matrix
        A[num_states, ] <- 1                          # Add the normalization equation: sum(pi) = 1
        b <- c(rep(0, num_states - 1), 1)             # Right-hand side vector

        # Solve the linear system
        steady_state_probs <- tryCatch({
          solve(A, b)
        }, error = function(e) {
          return(NULL)  # Return NULL if the system is unsolvable
        })

        # Validate the solution
        if (!is.null(steady_state_probs) &&
            all(steady_state_probs >= 0) &&
            abs(sum(steady_state_probs) - 1) < 1e-6) {
          return(steady_state_probs)
        }
        return(NULL)  # Return NULL if the solution is invalid
      }

      ### Method 2: Eigenvalue Method
      try_eigen_method <- function() {
        # Calculate the eigenvalues and eigenvectors
        eig <- eigen(t(transition_matrix))
        eig_values <- eig$values
        eig_vectors <- eig$vectors

        # Find the eigenvector corresponding to the eigenvalue 1
        steady_state_vector <- eig_vectors[, which(abs(eig_values - 1) < 1e-6)]

        # Normalize the eigenvector to form a probability distribution
        steady_state_probs <- steady_state_vector / sum(steady_state_vector)

        # Validate the solution
        if (all(steady_state_probs >= 0) && abs(sum(steady_state_probs) - 1) < 1e-6) {
          return(steady_state_probs)
        }
        return(NULL)  # Return NULL if the solution is invalid
      }

      ### Main Execution
      steady_state_probs <- try_linear_method()
      if (is.null(steady_state_probs)) {
        steady_state_probs <- try_eigen_method()
      }

      # Check if any method succeeded
      if (is.null(steady_state_probs)) {
        return("Steady-state probabilities could not be computed. Ensure the matrix is valid.")
      }

      # Format and return the steady-state probabilities
      steady_state_probs <- as.numeric(steady_state_probs)
      paste0("Steady-State Probabilities: Sunny = ", round(steady_state_probs[1], 3),
             ", Cloudy = ", round(steady_state_probs[2], 3),
             ", Rainy = ", round(steady_state_probs[3], 3))
    })


    # Detailed Conclusion
    output$conclusion <- renderText({
      req(probs())
      transition_matrix <- input$transition_matrix

      # Check if the transition matrix is valid for steady state
      if (!all(abs(rowSums(transition_matrix) - 1) < 1e-6)) {
        return("The transition matrix rows do not sum to 1, so a steady state cannot be computed correctly.")
      }

      # Calculate eigenvalues to determine if a steady state exists
      eig <- eigen(t(transition_matrix))
      eig_values <- eig$values

      if (any(abs(eig_values - 1) < 1e-6)) {
        "The transition matrix has an eigenvalue equal to 1, indicating that a steady state exists. The system will converge to these steady-state probabilities regardless of the initial distribution, provided the Markov Chain is regular (i.e., irreducible and aperiodic)."
      } else {
        "The transition matrix does not have an eigenvalue equal to 1, indicating that a steady state may not exist or the system may not converge to a steady state."
      }
    })


    # Interactive State Diagram with Enhanced Colors and Sizing
    output$state_diagram <- renderPlot({
      # req(probs())
      matrix <- input$transition_matrix
      states <- c("Sunny", "Cloudy", "Rainy")

      # Create graph object
      graph <- graph_from_adjacency_matrix(matrix, mode = "directed", weighted = TRUE)
      edge_weights <- round(E(graph)$weight, 2)

      # Define colors and node sizes
      node_colors <- c("Sunny" = "#FFD700", "Cloudy" = "#87CEEB", "Rainy" = "#1E90FF")
      vertex_colors <- node_colors[states]
      vertex_sizes <- 75  # Increased size for better visibility

      # Define edge color and size
      edge_colors <- "#555555"
      edge_widths <- E(graph)$weight * 5  # Scaled edge width based on probability

      # Prob Labels
      edge_labels <- paste0("P=", edge_weights)  # Format edge labels with probabilities

      # Plot the graph with enhanced features
      plot(graph,
           vertex.label = states,
           vertex.label.color = "black",
           vertex.label.cex = 1.5,
           vertex.color = vertex_colors,
           vertex.size = vertex_sizes,
           edge.color = edge_colors,
           edge.width = edge_widths,
           edge.arrow.size = 0.8,
           main = "State Transition Diagram")
    })

    # Step-wise Probability Table (Instead of Cumulative)
    output$cumulative_table <- renderDT({
      req(probs())
      prob_df <- as.data.frame(probs())
      colnames(prob_df) <- c("Sunny", "Cloudy", "Rainy")
      prob_df <- cbind(Step = 1:nrow(prob_df), prob_df)
      datatable(prob_df, options = list(pageLength = 10), caption = "Probability Evolution Over Time")
    })

    # Display the Relationship Between Initial, Nth Step, and Transition Matrix
    output$relationship_text <- renderUI({
      req(probs())
      initial_probs <- initial_probs()
      n_step_probs <- tail(probs(), 1)  # Get nth step probabilities
      n <- input$n_steps
      transition_matrix <- input$transition_matrix

      # Compute transition matrix raised to the power of n
      transition_matrix_n <- transition_matrix
      if (n > 1) {
        for (i in 2:n) {
          transition_matrix_n <- transition_matrix_n %*% transition_matrix
        }
      }

      # Format transition matrix for LaTeX
      matrix_text <- paste0("\\begin{bmatrix}",
                            paste(apply(transition_matrix_n, 1, function(row) {
                              paste(round(row, 3), collapse = " & ")
                            }), collapse = " \\\\ "),
                            "\\end{bmatrix}")

      # Combine everything into a MathJax-rendered equation
      withMathJax(HTML(paste0(
        "$$\\textbf{Relationship:}\\ P(", n, ") = P(0) \\cdot T^{", n, "}$$",
        "$$\\textbf{Initial State:}\\ P(0) = \\begin{bmatrix}",
        round(initial_probs[1], 2), " \\\\ ",
        round(initial_probs[2], 2), " \\\\ ",
        round(initial_probs[3], 2), "\\end{bmatrix}$$",
        "$$\\textbf{Nth Transition Matrix:}\\ T^{", n, "} = ", matrix_text, "$$",
        "$$\\textbf{Nth State:}\\ P(", n, ") = \\begin{bmatrix}",
        round(n_step_probs[1], 2), " \\\\ ",
        round(n_step_probs[2], 2), " \\\\ ",
        round(n_step_probs[3], 2), "\\end{bmatrix}$$"
      )))
    })
  }

  # Run App
  shinyApp(ui = ui, server = server)
}
# run_markov_chain_visualizer()
