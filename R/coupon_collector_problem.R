#' Run the Coupon Collector Problem Visualization Shiny App
#'
#' This function launches a Shiny app that provides interactive visualizations and simulations for the Coupon Collector Problem.
#' Users can explore theoretical expectations, run Monte Carlo simulations, and view distributions (PMF, CDF) related to the problem.
#'
#' Features:
#' - Simulate the Coupon Collector Problem with customizable parameters.
#' - Visualize expected trials, simulation results, probability mass function (PMF), and cumulative distribution function (CDF).
#' - Dynamic plot themes (Light, Minimal, Dark).
#' - Includes a detailed explanation of the Coupon Collector Problem and its key formula.
#'
#' @return A Shiny app instance that runs locally in your browser.
#'
#' @import ggplot2
#' @import shiny
#' @import dplyr
#' @examples
#' \dontrun{
#' run_coupon_collector_app() # Run the Shiny app
#' }
#'
#' @export
run_coupon_collector_app <- function() {


  library(shiny)
  library(ggplot2)
  library(dplyr)

  # Define UI
  ui <- fluidPage(
    titlePanel("Visualizing the Coupon Collector Problem"),
    withMathJax(),  # Enable MathJax for equations
    sidebarLayout(
      sidebarPanel(
        h4("Customize Inputs"),
        numericInput("num_coupons", "Number of Coupons (n):", value = 10, min = 1),
        numericInput("num_simulations", "Number of Simulations (Monte Carlo Trials):", value = 1000, min = 1),
        selectInput("theme", "Plot Theme:",
                    choices = c("Light" = "theme_light",
                                "Minimal" = "theme_minimal",
                                "Dark" = "theme_dark")),
        actionButton("run", "Run Simulations", icon = icon("play")),
        hr(),
        h5("What is the Coupon Collector Problem?"),
        p("The Coupon Collector Problem explores the expected number of random draws required to collect a complete set of
           \\( n \\) unique items (e.g., cards, stickers). This problem assumes each draw is independent, with all items equally
           likely to be drawn."),
        p("Key Formula: $$E(T) = n \\times H_n$$ where $$H_n = \\sum_{k=1}^n \\frac{1}{k}$$ is the \\( n \\)-th Harmonic number."),
        strong("Explore this problem interactively!")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Expected Trials",
                   plotOutput("expected_plot"),
                   p("This plot shows the theoretical expected number of draws required to collect all unique coupons as the
                      number of coupons increases.")
          ),
          tabPanel("Simulation Results",
                   plotOutput("simulation_histogram"),
                   p("This histogram shows the distribution of the number of draws required to collect all \\( n \\) coupons across
                      the specified number of simulations."),
                   verbatimTextOutput("simulation_summary")
          ),
          tabPanel("Probability Distribution",
                   plotOutput("pmf_plot"),
                   p("The estimated probability mass function (PMF) shows the probability of requiring a specific number of draws to
                      collect all coupons, based on the simulation results.")
          ),
          tabPanel("Cumulative Distribution Function",
                   plotOutput("cdf_plot"),
                   p("The cumulative distribution function (CDF) shows the cumulative probability of collecting all coupons by a
                      specific number of draws, as estimated from the simulation results.")
          )
        )
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {
    # Simulation Data
    simulate_data <- reactive({
      input$run
      isolate({
        validate(need(input$num_coupons > 0, "Number of coupons must be positive."))
        validate(need(input$num_simulations > 0, "Number of simulations must be positive."))

        n <- input$num_coupons
        sims <- input$num_simulations
        trials <- replicate(sims, {
          collected <- rep(FALSE, n)
          count <- 0
          while (any(!collected)) {
            collected[sample(1:n, 1)] <- TRUE
            count <- count + 1
          }
          count
        })
        trials
      })
    })

    # Expected Trials Plot
    output$expected_plot <- renderPlot({
      n <- 1:input$num_coupons
      expected_trials <- n * sapply(n, function(x) sum(1 / (1:x)))
      df <- data.frame(n = n, expected = expected_trials)

      theme_function <- match.fun(input$theme)
      ggplot(df, aes(x = n, y = expected)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "red", size = 2) +
        labs(title = "Expected Number of Trials vs. Number of Coupons",
             x = "Number of Coupons (n)",
             y = "Expected Number of Trials (E(T))",
             caption = "E(T) = n × Harmonic Number") +
        theme_function()
    })

    # Simulation Histogram
    output$simulation_histogram <- renderPlot({
      trials <- simulate_data()
      df <- data.frame(trials = trials)

      theme_function <- match.fun(input$theme)
      ggplot(df, aes(x = trials)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        labs(title = "Distribution of Trials from Simulations",
             x = "Number of Trials",
             y = "Frequency",
             caption = paste("Based on", input$num_simulations, "simulations")) +
        theme_function()
    })

    output$simulation_summary <- renderText({
      trials <- simulate_data()
      summary <- summary(trials)
      paste("Simulation Summary:\n",
            "Min:", summary[1], "\n",
            "1st Quartile:", summary[2], "\n",
            "Median:", summary[3], "\n",
            "Mean:", summary[4], "\n",
            "3rd Quartile:", summary[5], "\n",
            "Max:", summary[6])
    })

    # PMF Plot
    output$pmf_plot <- renderPlot({
      trials <- simulate_data()
      pmf <- table(trials) / length(trials)
      df <- data.frame(trials = as.numeric(names(pmf)), probability = as.numeric(pmf))

      theme_function <- match.fun(input$theme)
      ggplot(df, aes(x = trials, y = probability)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(title = "Probability Mass Function (PMF)",
             x = "Number of Trials",
             y = "Probability",
             caption = "Probability of requiring specific trials to collect all coupons.") +
        theme_function()
    })

    # CDF Plot
    output$cdf_plot <- renderPlot({
      trials <- simulate_data()
      pmf <- table(trials) / length(trials)
      cdf <- cumsum(pmf)
      df <- data.frame(trials = as.numeric(names(pmf)), cumulative = as.numeric(cdf))

      theme_function <- match.fun(input$theme)
      ggplot(df, aes(x = trials, y = cumulative)) +
        geom_line(color = "green", size = 1.2) +
        labs(title = "Cumulative Distribution Function (CDF)",
             x = "Number of Trials",
             y = "Cumulative Probability",
             caption = "Cumulative probability of collecting all coupons by specific trials.") +
        theme_function()
    })
  }

  # Run App
  shinyApp(ui, server)
}
