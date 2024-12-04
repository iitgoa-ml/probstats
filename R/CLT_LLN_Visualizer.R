#' Statistical Concepts Explorer: CLT and WLLN
#'
#' This Shiny app allows users to explore two foundational statistical concepts:
#' the Central Limit Theorem (CLT) and the Weak Law of Large Numbers (WLLN).
#' Users can select either concept, choose parameters for different distributions
#' or experiments, and visualize the respective results.
#'
#' @import zoo
#' @import shiny
#' @examples
#' \dontrun{
#' run_clt_wlln_app() # This function runs the Shiny app.
#' }
#'
#' @export
run_clt_wlln_app <- function() {

  library(shiny)
  library(zoo)

  # Define UI
  ui <- fluidPage(
    titlePanel("Visualizing Central Limit Theorem and Weak Law of Large Numbers"),

    sidebarLayout(
      sidebarPanel(
        # Dropdown to select either CLT or LLN
        selectInput("appMode", "Select Mode:", choices = c("Central Limit Theorem", "Weak Law of Large Numbers")),

        # Conditional Panels for each mode
        conditionalPanel(
          condition = "input.appMode == 'Central Limit Theorem'",
          selectInput("dist", "Distribution Type:",
                      choices = c("Uniform", "Exponential", "Binomial", "Normal", "Poisson", "Gamma")),
          conditionalPanel(
            condition = "input.dist == 'Uniform'",
            numericInput("min", "Minimum (a):", value = 0),
            numericInput("max", "Maximum (b):", value = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Exponential'",
            numericInput("rate", "Rate (λ):", value = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Binomial'",
            numericInput("size", "Size (n):", value = 10),
            numericInput("prob", "Probability (p):", value = 0.5)
          ),
          conditionalPanel(
            condition = "input.dist == 'Normal'",
            numericInput("mean", "Mean (μ):", value = 0),
            numericInput("sd", "Standard Deviation (σ):", value = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Poisson'",
            numericInput("lambda", "Rate (λ):", value = 1)
          ),
          conditionalPanel(
            condition = "input.dist == 'Gamma'",
            numericInput("shape", "Shape (α):", value = 2),
            numericInput("scale", "Scale (θ):", value = 1)
          ),
          sliderInput("sampleSize", "Sample Size (n):", min = 50, max = 5000, value = 300),
          sliderInput("numSamples", "Number of Samples:", min = 100, max = 10000, value = 1000),
          sliderInput("bins", "Histogram Bins:", min = 5, max = 50, value = 20),
          checkboxInput("showNormal", "Show Theoretical Normal Curve", value = TRUE)
        ),

        conditionalPanel(
          condition = "input.appMode == 'Weak Law of Large Numbers'",
          selectInput("experiment", "Experiment Type:",
                      choices = c("Coin Flip", "Dice Roll", "Beta Distribution",
                                  "Normal Distribution", "Uniform Distribution",
                                  "Exponential Distribution", "Geometric Distribution")),

          conditionalPanel(
            condition = "input.experiment == 'Beta Distribution'",
            numericInput("alpha", "Alpha (α):", value = 2),
            numericInput("beta", "Beta (β):", value = 5)
          ),
          # Normal distribution inputs
          conditionalPanel(
            condition = "input.experiment == 'Normal Distribution'",
            numericInput("mean", "Mean (μ):", value = 0),
            numericInput("sd", "Standard Deviation (σ):", value = 1, min = 0.1)
          ),

          # Uniform distribution inputs
          conditionalPanel(
            condition = "input.experiment == 'Uniform Distribution'",
            numericInput("min", "Minimum (a):", value = 0),
            numericInput("max", "Maximum (b):", value = 1)
          ),

          # Exponential distribution inputs
          conditionalPanel(
            condition = "input.experiment == 'Exponential Distribution'",
            numericInput("rate", "Rate (λ):", value = 1, min = 0.1)
          ),

          # Geometric distribution inputs
          conditionalPanel(
            condition = "input.experiment == 'Geometric Distribution'",
            numericInput("probGeom", "Probability of Success (p):", value = 0.5, min = 0.01, max = 1)
          )
          ,
          sliderInput("numTrials", "Number of Trials:", min = 10, max = 5000, value = 100),


          sliderInput("windowSize", "Rolling Average Window:", min = 1, max = 100, value = 10),
          checkboxInput("showMean", "Show Theoretical Mean", value = TRUE)
        )
      ),

      mainPanel(
        plotOutput("mainPlot"),
        textOutput("description")
      )
    )
  )

  # Define server logic
  server <- function(input, output) {

    output$mainPlot <- renderPlot({
      # Central Limit Theorem logic
      if (input$appMode == "Central Limit Theorem") {
        # Generate random samples based on selected distribution
        dist <- switch(input$dist,
                       "Uniform" = function(n) runif(n, min = input$min, max = input$max),
                       "Exponential" = function(n) rexp(n, rate = input$rate),
                       "Binomial" = function(n) rbinom(n, size = input$size, prob = input$prob),
                       "Normal" = function(n) rnorm(n, mean = input$mean, sd = input$sd),
                       "Poisson" = function(n) rpois(n, lambda = input$lambda),
                       "Gamma" = function(n) rgamma(n, shape = input$shape, scale = input$scale))

        # Generate samples and compute sample means
        originalData <- dist(input$sampleSize)
        sampleMeans <- replicate(input$numSamples, mean(dist(input$sampleSize)))

        par(mfrow = c(1, 2)) # Two-panel layout

        ## Original Distribution Plot

        if (input$dist %in% c("Binomial", "Poisson")) {
          # Observed values from simulation
          observed_vals <- sort(unique(originalData))

          # Theoretical probabilities for observed values
          probs <- if (input$dist == "Binomial") {
            dbinom(observed_vals, size = input$size, prob = input$prob)
          } else if (input$dist == "Poisson") {
            dpois(observed_vals, lambda = input$lambda)
          }

          # Calculate empirical PMF
          counts <- table(factor(originalData, levels = observed_vals)) / length(originalData)

          # Create barplot and capture bar positions
          bar_positions <- barplot(counts, names.arg = observed_vals, col = "lightblue",
                                   main = "Original Distribution", ylab = "Probability",
                                   ylim = c(0, max(c(counts, probs)) * 1.2))

          # Overlay theoretical PMF using bar positions
          points(bar_positions, probs, col = "red", pch = 16, cex = 1.5) # Points for PMF
          lines(bar_positions, probs, col = "red", lwd = 2) # Line connecting points
        } else {
          # Continuous distribution: use histogram
          hist(originalData, main = "Original Distribution", col = "lightblue", border = "white",
               breaks = input$bins, freq = FALSE)
          if (input$dist == "Uniform") {
            curve(dunif(x, min = input$min, max = input$max), col = "red", lwd = 2, add = TRUE)
          } else if (input$dist == "Exponential") {
            curve(dexp(x, rate = input$rate), col = "red", lwd = 2, add = TRUE)
          } else if (input$dist == "Normal") {
            curve(dnorm(x, mean = input$mean, sd = input$sd), col = "red", lwd = 2, add = TRUE)
          } else if (input$dist == "Gamma") {
            curve(dgamma(x, shape = input$shape, scale = input$scale), col = "red", lwd = 2, add = TRUE)
          }
        }

        ## Distribution of Sample Means Plot
        hist(sampleMeans, main = "Distribution of Sample Means", col = "lightgreen", border = "white",
             breaks = input$bins, freq = FALSE)

        # Overlay theoretical normal curve
        if (input$showNormal) {
          curve(dnorm(x, mean = mean(sampleMeans), sd = sd(sampleMeans)), col = "red", lwd = 2, add = TRUE)
        }

        # Add theoretical mean line
        abline(v = mean(sampleMeans), col = "blue", lwd = 2, lty = 2)
      } else if (input$appMode == "Weak Law of Large Numbers") {
        # Weak Law of Large Numbers logic
        set.seed(123)

        # Generate trials based on selected experiment
        trials <- switch(input$experiment,
                         "Coin Flip" = sample(c(0, 1), input$numTrials, replace = TRUE),
                         "Dice Roll" = sample(1:6, input$numTrials, replace = TRUE),
                         "Beta Distribution" = rbeta(input$numTrials, input$alpha, input$beta),
                         "Normal Distribution" = rnorm(input$numTrials, mean = input$mean, sd = input$sd),
                         "Uniform Distribution" = runif(input$numTrials, min = input$min, max = input$max),
                         "Exponential Distribution" = rexp(input$numTrials, rate = input$rate),
                         "Geometric Distribution" = rgeom(input$numTrials, prob = input$probGeom))

        # Compute theoretical mean
        trueMean <- switch(input$experiment,
                           "Coin Flip" = 0.5,
                           "Dice Roll" = 3.5,
                           "Beta Distribution" = input$alpha / (input$alpha + input$beta),
                           "Normal Distribution" = input$mean,
                           "Uniform Distribution" = (input$min + input$max) / 2,
                           "Exponential Distribution" = 1 / input$rate,
                           "Geometric Distribution" = (1 - input$probGeom) / input$probGeom)

        # Compute cumulative and rolling averages
        cumulativeAvg <- cumsum(trials) / seq_along(trials)
        rollingAvg <- rollmean(cumulativeAvg, k = input$windowSize, fill = NA)

        # Plot cumulative and rolling averages
        plot(cumulativeAvg, type = "l", col = "blue", lwd = 2,
             ylim = range(c(cumulativeAvg, rollingAvg, trueMean), na.rm = TRUE),
             ylab = "Cumulative Average / Rolling Average", xlab = "Number of Trials",
             main = paste("Convergence with Rolling Average (Window =", input$windowSize, ")"))

        # Add rolling average
        lines(rollingAvg, col = "green", lwd = 2, lty = 2)

        # Add theoretical mean
        if (input$showMean) {
          abline(h = trueMean, col = "red", lty = 2, lwd = 2)
          legend("topright", legend = c("Cumulative Avg", "Rolling Avg", "Theoretical Mean"),
                 col = c("blue", "green", "red"), lty = c(1, 2, 2), lwd = 2)
        }
        output$description <- renderText({
          # Description of the selected experiment
          paste0("The Weak Law of Large Numbers shows that as the number of trials increases, ",
                 "the sample mean of ", input$experiment, " approaches the theoretical mean (",
                 round(trueMean, 2), ").")
        })
      }
    })

    output$description <- renderText({
      # Description for each mode
      if (input$appMode == "Central Limit Theorem") {
        paste0("The Central Limit Theorem states that as the sample size increases, ",
               "the distribution of sample means approaches a normal distribution, ",
               "regardless of the population distribution.")
      }
    })
  }

  # Run the Shiny app
  shinyApp(ui, server)
}
# run_clt_wlln_app()
