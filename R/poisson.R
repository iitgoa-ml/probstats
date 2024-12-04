#' Launch Poisson and Exponential Distributions App
#'
#' A Shiny application to visualize and explore Poisson and Exponential distributions.
#' Adjust the rate parameter (λ) and view interactive plots with summaries.
#'
#' @details
#' - **Poisson Distribution**: Visualizes the probability of a number of events (x) given λ.
#' - **Exponential Distribution**: Shows waiting time density with λ as the rate.
#'
#' @return Launches a Shiny app in the default web browser.
#'
#' @examples
#' \dontrun{
#' run_poisson_app() # This function runs the Shiny app.
#' }
#'
#' @import shiny
#' @export
run_poisson_app <- function() {
  # Load required libraries
  library(shiny)

  # Define the UI
  ui <- fluidPage(
    titlePanel("Poisson and Waiting Time (Exponential) Distributions"),

    sidebarLayout(
      sidebarPanel(
        # Inputs for Poisson distribution
        sliderInput("lambda",
                    "Rate Parameter (λ):",
                    min = 0.5,
                    max = 10,
                    value = 2,
                    step = 0.1),

        sliderInput("poissonRange",
                    "X-Range for Poisson (Number of Events):",
                    min = 0,
                    max = 50,
                    value = c(0, 20)),

        # Inputs for Exponential distribution
        sliderInput("timeRange",
                    "Time Range for Waiting Time (T):",
                    min = 0,
                    max = 10,
                    value = c(0, 5),
                    step = 0.1)
      ),

      mainPanel(
        # Poisson distribution plot
        h3("Poisson Distribution"),
        plotOutput("poissonPlot"),
        textOutput("poissonSummary"),

        # Exponential distribution plot
        h3("Waiting Time (Exponential) Distribution"),
        plotOutput("exponentialPlot"),
        textOutput("exponentialSummary")
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {
    # Poisson distribution plot
    output$poissonPlot <- renderPlot({
      x <- seq(input$poissonRange[1], input$poissonRange[2])
      y <- dpois(x, lambda = input$lambda)

      plot(
        x, y, type = "h", lwd = 2, col = "blue",
        main = paste("Poisson Distribution (λ =", input$lambda, ")"),
        xlab = "Number of Events (x)",
        ylab = "Probability P(X = x)"
      )
    })

    # Poisson summary
    output$poissonSummary <- renderText({
      paste(
        "For λ =", input$lambda,
        "the mean is", input$lambda,
        "and the variance is", input$lambda, "."
      )
    })

    # Exponential distribution plot
    output$exponentialPlot <- renderPlot({
      t <- seq(input$timeRange[1], input$timeRange[2], by = 0.01)
      y <- input$lambda * exp(-input$lambda * t)

      plot(
        t, y, type = "l", lwd = 2, col = "purple",
        main = paste("Exponential Distribution (λ =", input$lambda, ")"),
        xlab = "Time (T)",
        ylab = "Density f(T)"
      )

      abline(v = 1 / input$lambda, col = "red", lty = 2, lwd = 2)
      legend("topright", legend = paste("Mean Waiting Time =", round(1 / input$lambda, 2)), col = "red", lty = 2, cex = 0.8)
    })

    # Exponential summary
    output$exponentialSummary <- renderText({
      paste(
        "The exponential distribution models the waiting time between events.",
        "For λ =", input$lambda,
        "the mean waiting time is", round(1 / input$lambda, 2), "."
      )
    })
  }

  # Run the app
  shinyApp(ui = ui, server = server)
}
