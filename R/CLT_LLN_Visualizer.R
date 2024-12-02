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
                      choices = c("Uniform", "Exponential", "Binomial")),
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
          sliderInput("sampleSize", "Sample Size (n):", min = 50, max = 5000, value = 300),
          sliderInput("numSamples", "Number of Samples:", min = 100, max = 10000, value = 1000),
          sliderInput("bins", "Histogram Bins:", min = 5, max = 50, value = 20),
          checkboxInput("showNormal", "Show Theoretical Normal Curve", value = TRUE)
        ),

        conditionalPanel(
          condition = "input.appMode == 'Weak Law of Large Numbers'",
          selectInput("experiment", "Experiment Type:",
                      choices = c("Coin Flip", "Dice Roll", "Beta Distribution")),

          conditionalPanel(
            condition = "input.experiment == 'Beta Distribution'",
            numericInput("alpha", "Alpha (α):", value = 2),
            numericInput("beta", "Beta (β):", value = 5)
          ),
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
        dist <- switch(input$dist,
                       "Uniform" = function(n) runif(n, min = input$min, max = input$max),
                       "Exponential" = function(n) rexp(n, rate = input$rate),
                       "Binomial" = function(n) rbinom(n, size = input$size, prob = input$prob))

        sampleMeans <- replicate(input$numSamples, mean(dist(input$sampleSize)))

        par(mfrow = c(1, 2)) # Two-panel layout
        # Original distribution
        hist(dist(input$sampleSize), main = "Original Distribution", col = "lightblue", border = "white", breaks = input$bins)
        # Sample means distribution
        hist(sampleMeans, main = "Distribution of Sample Means", col = "lightgreen", border = "white", breaks = input$bins)

        # Adding the theoretical normal curve if requested
        if (input$showNormal) {
          curve(dnorm(x, mean = mean(sampleMeans), sd = sd(sampleMeans)), add = TRUE, col = "red", lwd = 2)
        }

      } else if (input$appMode == "Weak Law of Large Numbers") {
        # Weak Law of Large Numbers logic
        set.seed(123)

        trials <- switch(input$experiment,
                         "Coin Flip" = sample(c(0, 1), input$numTrials, replace = TRUE),
                         "Dice Roll" = sample(1:6, input$numTrials, replace = TRUE),
                         "Beta Distribution" = rbeta(input$numTrials, input$alpha, input$beta))

        cumulativeAvg <- cumsum(trials) / seq_along(trials)
        windowSize <- input$windowSize
        rollingAvg <- rollmean(cumulativeAvg, k = windowSize, fill = NA)

        # Plot cumulative and rolling averages
        plot(cumulativeAvg, type = "l", col = "blue", lwd = 2,
             ylim = range(c(cumulativeAvg, rollingAvg), na.rm = TRUE),
             ylab = "Cumulative Average / Rolling Average", xlab = "Number of Trials",
             main = paste("Convergence with Rolling Average (Window =", windowSize, ")"))

        # Add rolling average to the plot
        lines(rollingAvg, col = "green", lwd = 2, lty = 2)

        # Show theoretical mean if checkbox is checked
        trueMean <- if (input$experiment == "Coin Flip") 0.5 else if (input$experiment == "Dice Roll") 3.5 else mean(rbeta(10000, input$alpha, input$beta))
        if (input$showMean) {
          abline(h = trueMean, col = "red", lty = 2)
        }
      }
    })

    output$description <- renderText({
      # Description for each mode
      if (input$appMode == "Central Limit Theorem") {
        paste0("The Central Limit Theorem states that as the sample size increases, ",
               "the distribution of sample means approaches a normal distribution, ",
               "regardless of the population distribution.")
      } else {
        paste("The Weak Law of Large Numbers shows that as the number of trials increases, ",
              "the sample mean converges to the expected value. ",
              "The rolling average window smooths the data over the specified window size.")
      }
    })
  }

  # Run the Shiny app
  shinyApp(ui, server)
}
# run_clt_wlln_app()
