

#' Probability of Repeated Outcomes in Random Events Shiny App
#'
#' This Shiny app allows users to explore the probabilities of repeated outcomes
#' in various random events such as the Birthday Paradox, Dice Rolls, Card Draws, and Coin Flips.
#' It also includes a simulation feature to visualize the outcomes and highlight repeated events.
#'
#' @param none No parameters required
#' @return A Shiny app interface to explore probabilities and simulate events
#' @import ggplot2
#' @import shiny
#' @examples
#' run_probability_app()  # Launch the app
#'
#' @export
run_birthday_paradox_app <- function() {
  library(shiny)
  library(ggplot2)

  # Probability functions for different events
  # Birthday paradox formula: P(A') = 1 * (364/365) * ... * (365-n+1)/365
  birthday_prob <- function(n) {
    if (n == 0) return(0)
    prob_no_shared <- 1
    for (i in 0:(n-1)) {
      prob_no_shared <- prob_no_shared * (365 - i) / 365
    }
    return(1 - prob_no_shared)
  }

  # Dice throws formula: P(A') = 1 * (5/6) * ... * (6-n+1)/6
  dice_prob <- function(n) {
    if (n == 0) return(0)
    prob_no_shared <- 1
    for (i in 0:(n-1)) {
      prob_no_shared <- prob_no_shared * (6 - i) / 6
    }
    return(1 - prob_no_shared)
  }

  # Card draws formula: P(A') = 1 * (51/52) * ... * (52-n+1)/52
  cards_prob <- function(n) {
    if (n == 0) return(0)
    prob_no_shared <- 1
    for (i in 0:(n-1)) {
      prob_no_shared <- prob_no_shared * (52 - i) / 52
    }
    return(1 - prob_no_shared)
  }

  # Coin flips formula: P(A') = 1 * (1/2) * ... * (2-n+1)/2
  coin_prob <- function(n) {
    if (n == 0) return(0)
    prob_no_shared <- 1
    for (i in 0:(n-1)) {
      prob_no_shared <- prob_no_shared * (2 - i) / 2
    }
    return(1 - prob_no_shared)
  }

  # Simulate events
  simulate_event <- function(n, type) {
    if (type == "Birthday Paradox") {
      outcomes <- sample(1:365, n, replace = TRUE)
    } else if (type == "Dice Rolls") {
      outcomes <- sample(1:6, n, replace = TRUE)
    } else if (type == "Card Draws") {
      outcomes <- sample(1:52, n, replace = TRUE)
    } else if (type == "Coin Flips") {
      outcomes <- sample(c("Heads", "Tails"), n, replace = TRUE)
    }
    duplicates <- duplicated(outcomes)
    return(list(outcomes = outcomes, duplicates = duplicates))
  }

  # Run multiple simulations to count repeated outcomes
  run_multiple_simulations <- function(n, iterations, type) {
    repeated_counts <- numeric(iterations)
    for (i in 1:iterations) {
      sim <- simulate_event(n, type)
      repeated_counts[i] <- sum(sim$duplicates)
    }
    return(repeated_counts)
  }

  # UI for the Shiny app
  ui <- fluidPage(
    titlePanel("Probability of Repeated Outcomes in Random Events"),
    sidebarLayout(
      sidebarPanel(
        h3("Explore Repeated Outcomes in Random Events"),
        p("This app visualizes the probability of repeated outcomes (e.g., same birthday, dice number, card value, or coin flip result) occurring at least once as the number of samples (n) changes."),

        selectInput("event_type", "Select Event:",
                    choices = c("Birthday Paradox", "Dice Rolls", "Card Draws", "Coin Flips"),
                    selected = "Birthday Paradox"),

        # Problem description based on the event type
        uiOutput("problem_description"),
        hr(),

        # Probability equation based on the event type
        h4("Probability Equation"),
        uiOutput("equation"),  # Display the probability equation dynamically
        hr(),

        sliderInput("num_samples", "Number of Samples (n):", min = 1, max = 365, value = 23),



        actionButton("simulate", "Simulate"),
        br(),

        h4("Probability Meter"),
        uiOutput("probMeter"),  # Dynamic probability meter
        helpText("Displays the probability of observing a repeated outcome in the selected event scenario based on the number of samples."),

        hr(),

        sliderInput("num_runs", "Number of Repeated Runs:", min = 1, max = 2000, value = 50),

        actionButton("runSimulation", "Run Repeated Runs"),
        hr(),

      ),

      mainPanel(
        plotOutput("probPlot"),
        textOutput("probText"),

        hr(),
        h4("Repeated Runs Simulation Results"),
        plotOutput("simulationPlot"),  # Visualizes simulation outcomes and repeats
        helpText("Visualizes the outcomes of repeated runs, highlighting where repeated outcomes occur.")
      )
    )
  )

  # Server logic
  server <- function(input, output, session) {

    # Update the slider range based on selected event
    observeEvent(input$event_type, {
      if (input$event_type == "Birthday Paradox") {
        updateSliderInput(session, "num_samples", min = 1, max = 366, value = 23)
      } else if (input$event_type == "Dice Rolls") {
        updateSliderInput(session, "num_samples", min = 1, max = 10, value = 2)
      } else if (input$event_type == "Card Draws") {
        updateSliderInput(session, "num_samples", min = 1, max = 60, value = 2)
      } else if (input$event_type == "Coin Flips") {
        updateSliderInput(session, "num_samples", min = 1, max = 6, value = 2)
      }
    })


    # Display unique problem descriptions for each event
    output$problem_description <- renderUI({
      event <- input$event_type
      description <- switch(event,
                            "Birthday Paradox" = "In the birthday paradox, we explore the probability of at least two people sharing the same birthday in a group of randomly chosen individuals.",
                            "Dice Rolls" = "In the dice rolls scenario, we examine the likelihood of getting the same number at least once when rolling a fair die multiple times.",
                            "Card Draws" = "In the card draws scenario, we consider the probability of drawing the same card value more than once in a sequence of card draws from a standard deck.",
                            "Coin Flips" = "In the coin flips scenario, we investigate the probability of getting the same result (either heads or tails) at least once when flipping a fair coin multiple times.")
      tags$p(strong("Problem Description: "), description)
    })

    # Display the probability equation dynamically
    # Display the probability equation dynamically using MathJax with proper math formatting
    output$equation <- renderUI({
      event <- input$event_type
      formula <- switch(event,
                        "Birthday Paradox" = "$$P(A') = 1 \\times \\frac{364}{365} \\times \\frac{363}{365} \\times \\cdots \\times \\frac{365 - n + 1}{365}$$",
                        "Dice Rolls" = "$$P(A') = 1 \\times \\frac{5}{6} \\times \\frac{4}{6} \\times \\cdots \\times \\frac{6 - n + 1}{6}$$",
                        "Card Draws" = "$$P(A') = 1 \\times \\frac{51}{52} \\times \\frac{50}{52} \\times \\cdots \\times \\frac{52 - n + 1}{52}$$",
                        "Coin Flips" = "$$P(A') = 1 \\times \\frac{1}{2} \\times \\cdots \\times \\frac{2 - n + 1}{2}$$")

      withMathJax(
        tags$p(strong("Probability Formula: "), formula)
      )
    })

    # Simulate based on the selected event
    observeEvent(input$simulate, {
      n <- input$num_samples
      event <- input$event_type
      prob <- 0

      if (event == "Birthday Paradox") {
        prob <- birthday_prob(n)
      } else if (event == "Dice Rolls") {
        prob <- dice_prob(n)
      } else if (event == "Card Draws") {
        prob <- cards_prob(n)
      } else if (event == "Coin Flips") {
        prob <- coin_prob(n)
      }

      # Output the probability text
      output$probText <- renderText({
        paste("With", n, "samples, the probability of observing a repeated outcome at least once in the selected scenario is approximately", round(prob * 100, 3), "%.")
      })

      # Create a sequence for plotting the probability
      max_samples <- switch(input$event_type,
                            "Birthday Paradox" = 366,
                            "Dice Rolls" = 10,
                            "Card Draws" = 60,
                            "Coin Flips" = 6)

      samples_seq <- 1:max_samples
      prob_seq <- sapply(samples_seq, switch(input$event_type,
                                             "Birthday Paradox" = birthday_prob,
                                             "Dice Rolls" = dice_prob,
                                             "Card Draws" = cards_prob,
                                             "Coin Flips" = coin_prob))

      # Plot the probability curve
      output$probPlot <- renderPlot({
        ggplot(data = data.frame(samples = samples_seq, probability = prob_seq), aes(x = samples, y = probability)) +
          geom_line(color = "blue", size = 1.2) +
          geom_vline(xintercept = n, linetype = "dashed", color = "red") +
          ggtitle(paste("Probability of Repeated Outcome for", event)) +
          xlab("Number of Samples (n)") +
          ylab("Probability") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          theme_minimal()
      })

      # Probability meter
      output$probMeter <- renderUI({
        progressBarColor <- ifelse(prob > 0.5, "bg-danger", "bg-success")
        shiny::tags$div(class = "progress",
                        shiny::tags$div(class = paste("progress-bar", progressBarColor),
                                        role = "progressbar", style = paste0("width:", round(prob * 100), "%;"),
                                        paste(round(prob * 100, 2), "%")))
      })
    })

    # Run repeated runs simulation
    observeEvent(input$runSimulation, {
      n <- input$num_samples
      iterations <- input$num_runs
      event <- input$event_type
      repeated_counts <- run_multiple_simulations(n, iterations, event)

      # Plot the repeated outcome counts
      output$simulationPlot <- renderPlot({
        ggplot(data = data.frame(repeated_counts), aes(x = repeated_counts)) +
          geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
          xlab("Number of Repeated Samples") +
          ylab("Frequency in Iterations") +
          ggtitle("Distribution of Repeated Outcomes Across Iterations") +
          theme_minimal()
      })
    })
  }

  # Run the shiny app
  shinyApp(ui, server)
}

# Run the app
# run_birthday_paradox_app()
