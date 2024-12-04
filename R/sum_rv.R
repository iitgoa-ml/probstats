#' Run the Sum of Random Variables Shiny App
#'
#' This function launches a Shiny application designed to provide an interactive
#' and educational exploration of the behavior of sums of random variables.
#' Users can select various distributions, define their parameters, simulate
#' samples, and analyze both theoretical and empirical properties of the resulting sums.
#'
#' @details
#' The application supports the following distributions:
#' - **Normal**: Defined by mean and standard deviation.
#' - **Exponential**: Defined by the rate parameter.
#' - **Gamma**: Defined by shape and rate parameters.
#' - **Beta**: Defined by two shape parameters.
#' - **Chi-Square**: Defined by degrees of freedom.
#' - **Poisson**: Defined by the lambda parameter.
#'
#' Features of the app include:
#' - Dynamic input interface for up to 10 random variables, each with adjustable parameters.
#' - Real-time visualization of individual distributions and their sum.
#' - Theoretical and empirical analysis of distribution properties, including mean and variance.
#' - Statistical summary of the sum, including a Shapiro-Wilk test for normality and Q-Q plots.
#' - Educational explanations of key concepts like the Central Limit Theorem.
#'
#' @examples
#' \dontrun{
#' run_rv_sum_app() # Run the Shiny app
#' }
#'
#' @return
#' This function does not return any values. It starts a Shiny application in the user's default browser.
#'
#' @note
#' The app is designed for educational purposes and provides a hands-on way to
#' explore probabilistic concepts and the interaction between different types of distributions.
#'
#' @import shiny ggplot2 gridExtra reshape2 latex2exp
#' @export
run_rv_sum_app <- function() {

  library(shiny)
  library(ggplot2)
  library(gridExtra)
  library(reshape2)
  library(latex2exp)

  # Enhanced Distribution Handling Functions
  distribution_specs <- list(
    "Normal" = list(
      generator = function(n, param1, param2) rnorm(n, mean = param1, sd = param2),
      pdf = function(x, param1, param2) dnorm(x, mean = param1, sd = param2),
      params = c("Mean", "Std Dev"),
      theoretical_desc = "The normal distribution is symmetric and bell-shaped, characterized by its mean and standard deviation."
    ),
    "Exponential" = list(
      generator = function(n, param1, param2) rexp(n, rate = param1),
      pdf = function(x, param1, param2) dexp(x, rate = param1),
      params = c("Rate"),
      theoretical_desc = "The exponential distribution models time between events in a Poisson process, with a decreasing probability density."
    ),
    "Gamma" = list(
      generator = function(n, param1, param2) rgamma(n, shape = param1, rate = param2),
      pdf = function(x, param1, param2) dgamma(x, shape = param1, rate = param2),
      params = c("Shape", "Rate"),
      theoretical_desc = "The gamma distribution models waiting times and is used in various scientific and statistical applications."
    ),
    "Beta" = list(
      generator = function(n, param1, param2) rbeta(n, shape1 = param1, shape2 = param2),
      pdf = function(x, param1, param2) dbeta(x, shape1 = param1, shape2 = param2),
      params = c("Shape 1", "Shape 2"),
      theoretical_desc = "The beta distribution is defined on the interval [0,1] and is useful for modeling probabilities."
    ),
    "Chi-Square" = list(
      generator = function(n, param1, param2) rchisq(n, df = param1),
      pdf = function(x, param1, param2) dchisq(x, df = param1),
      params = c("Degrees of Freedom"),
      theoretical_desc = "The chi-square distribution is the sum of squared standard normal random variables."
    ),
    "Poisson" = list(
      generator = function(n, param1, param2) rpois(n, lambda = param1),
      pdf = function(x, param1, param2) dpois(x, lambda = param1),
      params = c("Lambda"),
      theoretical_desc = "The Poisson distribution models the number of events occurring in a fixed interval of time or space."
    )
  )

  # UI Definition
  ui <- fluidPage(
    tags$head(tags$style(HTML("
      .explanation {
        background-color: #f0f0f0;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 5px;
      }
      .well { background-color: #f5f5f5; }
    "))),

    titlePanel("Sum of Random Variables: Advanced Educational Visualization"),

    div(class = "explanation",
        h3("Learning Objectives"),
        tags$ul(
          tags$li("Understand distribution behavior when summing random variables"),
          tags$li("Explore theoretical and empirical distribution properties"),
          tags$li("Investigate the Central Limit Theorem"),
          tags$li("Analyze different distribution interactions")
        )
    ),

    sidebarLayout(
      sidebarPanel(
        numericInput("num_distributions",
                     "Number of Distributions to Sum",
                     value = 2,
                     min = 2,
                     max = 10),

        uiOutput("distribution_inputs"),

        numericInput("num_samples",
                     "Number of Samples",
                     value = 10000,
                     min = 1000,
                     max = 100000),

        actionButton("simulate", "Simulate Distributions", class = "btn-primary")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Distributions",
                   plotOutput("distribution_plot", height = "600px"),
                   tableOutput("theoretical_params"),
                   div(class = "explanation",
                       htmlOutput("distribution_explanation")
                   )
          ),
          tabPanel("Statistical Analysis",
                   plotOutput("statistical_plot"),
                   verbatimTextOutput("statistical_summary")
          ),
          tabPanel("Theoretical Background",
                   htmlOutput("theoretical_background")
          ),
          tabPanel("Conclusion and Insights",
                   htmlOutput("conclusion_insights")
          )
        )
      )
    )
  )

  # Server Logic
  server <- function(input, output, session) {
    # Dynamic Distribution Inputs
    output$distribution_inputs <- renderUI({
      num_dist <- input$num_distributions

      lapply(1:num_dist, function(i) {
        div(
          h4(paste("Distribution", i)),
          selectInput(paste0("dist", i, "_type"), "Distribution Type",
                      names(distribution_specs)),

          # Dynamic parameter inputs based on distribution
          uiOutput(paste0("dist", i, "_param_inputs"))
        )
      })
    })

    # Dynamic parameter inputs for each distribution
    lapply(1:10, function(i) {
      output[[paste0("dist", i, "_param_inputs")]] <- renderUI({
        req(input[[paste0("dist", i, "_type")]])

        dist_type <- input[[paste0("dist", i, "_type")]]
        params <- distribution_specs[[dist_type]]$params

        div(
          if(length(params) > 0)
            lapply(seq_along(params), function(j) {
              numericInput(
                paste0("dist", i, "_param", j),
                params[j],
                value = switch(dist_type,
                               "Normal" = if(j==1) 0 else 1,
                               "Exponential" = 1,
                               "Gamma" = if(j==1) 2 else 1,
                               "Beta" = 2,
                               "Chi-Square" = 3,
                               "Poisson" = 5
                ),
                min = 0.1,
                step = 0.1
              )
            })
        )
      })
    })

    # Simulation Logic
    simulation_data <- reactiveVal(NULL)

    observeEvent(input$simulate, {
      set.seed(123)

      # Collect distribution generators
      dist_generators <- lapply(1:input$num_distributions, function(i) {
        dist_type <- input[[paste0("dist", i, "_type")]]

        # Collect parameters dynamically
        params <- lapply(seq_along(distribution_specs[[dist_type]]$params), function(j) {
          input[[paste0("dist", i, "_param", j)]]
        })

        # Generate samples
        samples <- do.call(
          distribution_specs[[dist_type]]$generator,
          c(list(n = input$num_samples), params)
        )

        list(
          type = dist_type,
          samples = samples,
          params = params
        )
      })

      # Calculate sum of distributions
      sum_samples <- Reduce(`+`, lapply(dist_generators, `[[`, "samples"))

      # Store simulation data
      simulation_data(list(
        distributions = dist_generators,
        sum_dist = sum_samples
      ))
    })

    # Distribution Plot
    output$distribution_plot <- renderPlot({
      req(simulation_data())

      # Create plots for individual and sum distributions
      plots <- lapply(seq_along(simulation_data()$distributions), function(i) {
        dist_info <- simulation_data()$distributions[[i]]

        ggplot(data.frame(x = dist_info$samples), aes(x = x)) +
          geom_histogram(aes(y = after_stat(density)), bins = 50, fill = rainbow(10)[i], alpha = 0.5) +
          stat_function(
            fun = function(x) do.call(
              distribution_specs[[dist_info$type]]$pdf,
              c(list(x = x), dist_info$params)
            ),
            color = "red",
            size = 1
          ) +
          labs(title = paste(dist_info$type, "Distribution", i),
               x = "Value",
               y = "Density") +
          theme_minimal()
      })

      # Add sum distribution plot
      plots[[length(plots) + 1]] <- ggplot(data.frame(x = simulation_data()$sum_dist), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "purple", alpha = 0.5) +
        labs(title = "Sum of Distributions", x = "Value", y = "Density") +
        theme_minimal()

      # Arrange plots
      grid.arrange(grobs = plots, ncol = 3)
    })

    # Statistical Plot
    output$statistical_plot <- renderPlot({
      req(simulation_data())

      # QQ Plot for sum distribution
      qqnorm(simulation_data()$sum_dist, main = "Q-Q Plot of Sum Distribution")
      qqline(simulation_data()$sum_dist, col = "red")
    })

    # Theoretical Parameters Table
    output$theoretical_params <- renderTable({
      req(simulation_data())

      # Calculate theoretical parameters for each distribution and sum
      dist_params <- lapply(simulation_data()$distributions, function(dist) {
        type <- dist$type
        params <- dist$params

        # Theoretical mean and variance calculations
        list(
          mean = do.call(
            switch(type,
                   "Normal" = function(m, sd) m,
                   "Exponential" = function(rate) 1/rate,
                   "Gamma" = function(shape, rate) shape/rate,
                   "Beta" = function(a, b) a/(a+b),
                   "Chi-Square" = function(df) df,
                   "Poisson" = function(lambda) lambda
            ),
            params
          ),
          variance = do.call(
            switch(type,
                   "Normal" = function(m, sd) sd^2,
                   "Exponential" = function(rate) 1/(rate^2),
                   "Gamma" = function(shape, rate) shape/(rate^2),
                   "Beta" = function(a, b) (a*b)/((a+b)^2*(a+b+1)),
                   "Chi-Square" = function(df) 2*df,
                   "Poisson" = function(lambda) lambda
            ),
            params
          )
        )
      })

      # Compute sum distribution parameters
      total_mean <- sum(sapply(dist_params, `[[`, "mean"))
      total_variance <- sum(sapply(dist_params, `[[`, "variance"))

      # Create data frame for display
      data.frame(
        Distribution = c(sapply(simulation_data()$distributions, `[[`, "type"), "Sum Distribution"),
        Mean = c(sapply(dist_params, `[[`, "mean"), total_mean),
        Variance = c(sapply(dist_params, `[[`, "variance"), total_variance)
      )
    })

    # Distribution Explanation
    output$distribution_explanation <- renderUI({
      req(simulation_data())

      dist_types <- sapply(simulation_data()$distributions, `[[`, "type")

      HTML(paste0(
        "<h4>Distribution Interaction Analysis</h4>",
        "<p>Summing ", paste(dist_types, collapse = " and "), " distributions</p>",
        "<strong>Key Observations:</strong>",
        "<ul>",
        "<li>Distribution shape depends on individual distribution characteristics</li>",
        "<li>Central Limit Theorem suggests convergence towards normality</li>",
        "<li>Variance of sum is sum of individual variances for independent distributions</li>",
        "</ul>"
      ))
    })

    # Theoretical Background
    output$theoretical_background <- renderUI({
      HTML(
        "<h3>Theoretical Foundations of Sum of Random Variables</h3>" %+%
          "<h4>1. Basic Principles</h4>" %+%
          "<p>When independent random variables are added, their properties combine:</p>" %+%
          "<ul>" %+%
          "<li><strong>Mean:</strong> Sum of individual means</li>" %+%
          "<li><strong>Variance:</strong> Sum of individual variances</li>" %+%
          "</ul>" %+%

          "<h4>2. Central Limit Theorem</h4>" %+%
          "<p>As the number of random variables increases, their sum approaches a normal distribution.</p>" %+%

          "<h4>3. Distribution-Specific Behavior</h4>" %+%
          "<ul>" %+%
          "<li>Normal distributions sum to a normal distribution</li>" %+%
          "<li>Some distributions have specific sum properties</li>" %+%
          "<li>Asymmetric distributions can create complex sum behaviors</li>" %+%
          "</ul>"
      )
    })

    # Conclusion and Insights
    output$conclusion_insights <- renderUI({
      req(simulation_data())

      HTML(
        "<h3>Conclusions and Insights</h3>" %+%
          "<p>Key takeaways from distribution sum analysis:</p>" %+%
          "<ul>" %+%
          "<li>Distribution sums are fundamental to understanding probabilistic systems</li>" %+%
          "<li>Theoretical predictions may differ from empirical observations</li>" %+%
          "<li>Simulation helps visualize complex probabilistic interactions</li>" %+%
          "<li>Different distributions produce unique sum characteristics</li>" %+%
          "</ul>" %+%

          "<h4>Practical Implications</h4>" %+%
          "<p>Understanding distribution sums is crucial in:</p>" %+%
          "<ul>" %+%
          "<li>Statistical modeling</li>" %+%
          "<li>Financial risk assessment</li>" %+%
          "<li>Scientific research</li>" %+%
          "<li>Machine learning and data science</li>" %+%
          "</ul>"
      )
    })

    # Statistical Summary
    output$statistical_summary <- renderPrint({
      req(simulation_data())

      cat("Simulation Statistical Summary\n")
      cat("-----------------------------\n")
      cat("Number of Distributions:", length(simulation_data()$distributions), "\n")
      cat("Number of Samples:", length(simulation_data()$sum_dist), "\n\n")

      cat("Sum Distribution Summary:\n")
      print(summary(simulation_data()$sum_dist))

      cat("\nShapiro-Wilk Normality Test:\n")
      print(shapiro.test(simulation_data()$sum_dist))
    })
  }

  # Create Shiny App
  shinyApp(ui, server)
}
