

#' Run Distributions Visualizer App
#'
#' This function launches the Shiny app for visualizing many distributions with continuous and discrete sample spaces.
#' Users can select between Uniform, Normal, Exponential and Binomial distributions, define variable ranges and intervals,
#' and calculate both joint and individual probabilities. The app also provides visualizations
#' of the defined ranges and intervals.
#'
#' @return The Shiny app is launched in the default web browser.
#' @import ggplot2
#' @import shiny
#' @import gridExtra
#' @examples
#' \dontrun{
#' run_distribution_visualizer_app() # Run shiny app
#' }
#' @export
run_distribution_visualizer_app <- function() {

  library(shiny)
  library(ggplot2)
  #library(dplyr)
  library(gridExtra)


  # Define UI
  ui <- fluidPage(
    tags$head(
      tags$script(src = "mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),



    ),
    titlePanel("Distributions Visualizer"),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("distribution", "Select Distribution",
                    choices = c("Uniform" = "uniform", "Normal" = "normal", "Exponential"="exponential", "Binomial" = "binomial", "Geometric" = "geometric")),
        uiOutput("distribution_equations"),  # Display PDF, CDF equations above everything

        hr(),
        numericInput("numVars", "Number of Variables:", value = 1, min = 1),
        hr(),

        uiOutput("distSpecificInputs"),
        hr(),

        h3("Intervals"),
        numericInput("numIntervals", "Number of Intervals:", value = 1, min = 1),
        uiOutput("intervalInputs"),
        hr(),

        actionButton("calculate", "Visualize", class = "btn-primary")
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Visualization", plotOutput("visualization", height = "600px")),
          tabPanel("Probabilities",
                   h4("Joint Probability"),
                   verbatimTextOutput("jointProbability"),
                   h4("Individual Probabilities"),
                   tableOutput("individualProbabilities"))
        )
      )
    )
  )



  # Define Server
  server <- function(input, output, session) {

    # Conditionally render PDF and CDF formulas for Normal, Exponential, and Binomial distributions
    output$distribution_equations <- renderUI({
      dist <- input$distribution
      formulas <- switch(dist,
                         "normal" = withMathJax(
                           tags$div(
                             h4("Normal Distribution"),
                             tags$p("PDF: $$f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}$$"),
                             tags$p("CDF: $$F(x) = \\frac{1}{2} \\left[ 1 + \\text{erf} \\left( \\frac{x - \\mu}{\\sigma\\sqrt{2}} \\right) \\right]$$")
                           )
                         ),
                         "exponential" = withMathJax(
                           tags$div(
                             h4("Exponential Distribution"),
                             tags$p("PDF: $$f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0$$"),
                             tags$p("CDF: $$F(x) = 1 - e^{-\\lambda x}, \\quad x \\geq 0$$")
                           )
                         ),
                         "binomial" = withMathJax(
                           tags$div(
                             h4("Binomial Distribution"),
                             tags$p("PMF: $$P(X = k) = \\binom{n}{k} p^k (1 - p)^{n - k}$$"),
                             tags$p("CDF: $$F(k) = \\sum_{i=0}^{k} \\binom{n}{i} p^i (1 - p)^{n - i}$$")
                           )
                         ))

      formulas
    })

    # Generate UI inputs based on distribution
    output$distSpecificInputs <- renderUI({
      numVars <- input$numVars

      if (input$distribution == "uniform") {
        tagList(
          h3("Variable Ranges"),
          numericInput("numRanges", "Number of Ranges:", value = 1, min = 1),
          uiOutput("rangeInputs")
        )
      } else if (input$distribution == "normal") {
        lapply(1:numVars, function(i) {
          fluidRow(
            column(6, numericInput(paste0("mean", i), paste("Mean of Variable", i), value = 0)),
            column(6, numericInput(paste0("sd", i), paste("Standard Deviation of Variable", i), value = 1))
          )
        })
      } else if (input$distribution == "exponential") {
        lapply(1:numVars, function(i) {
          fluidRow(
            column(6, numericInput(paste0("rate", i), paste("Rate of Variable", i), value = 1))
          )
        })
      } else if (input$distribution == "binomial") {
        lapply(1:numVars, function(i) {
          fluidRow(
            column(6, numericInput(paste0("size", i), paste("Number of Trials (n) for Variable", i), value = 10, min = 1)),
            column(6, numericInput(paste0("prob", i), paste("Probability of Success (p) for Variable", i), value = 0.5, min = 0, max = 1)),
            numericInput("num_simulations", paste("Number of Simulations (N) for variable:", i), value = 1000, min = 20)
          )
        })
      } else if (input$distribution == "geometric") {
        # UI elements for Geometric distribution
        lapply(1:numVars, function(i) {
          fluidRow(
            column(6, numericInput(paste0("prob", i), paste("Probability of Success (p) for Variable", i),
                                   value = 0.5, min = 0.01, max = 1))
          )
        })
      }
    })
    # Automatically update numRanges when numVars changes
    observe({
      updateNumericInput(session, "numRanges", value = input$numVars)
      updateNumericInput(session, "numIntervals", value = input$numVars)
    })

    # Generate UI inputs for variable ranges (only for Uniform)
    output$rangeInputs <- renderUI({
      numRanges <- input$numRanges
      numVars <- input$numVars

      lapply(1:numRanges, function(i) {
        fluidRow(
          column(4, numericInput(paste0("range", i, "_min"), paste("Range", i, "Min Value:"), value = -3)),
          column(4, selectInput(paste0("range", i, "_var"), paste("Range", i, "Variable:"), choices = 1:numVars, selected = i)),
          column(4, numericInput(paste0("range", i, "_max"), paste("Range", i, "Max Value:"), value = 3))
        )
      })
    })

    # Generate UI inputs for intervals
    output$intervalInputs <- renderUI({
      numIntervals <- input$numIntervals
      numVars <- input$numVars

      lapply(1:numIntervals, function(i) {
        fluidRow(
          column(4, numericInput(paste0("interval", i, "_min"), paste("Interval", i, "Min Value:"), value = -2)),
          column(4, selectInput(paste0("interval", i, "_var"), paste("Interval", i, "Variable:"), choices = 1:numVars, selected = i)),
          column(4, numericInput(paste0("interval", i, "_max"), paste("Interval", i, "Max Value:"), value = 2))
        )
      })
    })

    # Merge overlapping intervals
    merge_intervals <- function(intervals) {
      if (nrow(intervals) <= 1) return(intervals)

      intervals <- intervals[order(intervals$min), ]
      merged_intervals <- data.frame(min = numeric(), max = numeric(), stringsAsFactors = FALSE)

      current_min <- intervals$min[1]
      current_max <- intervals$max[1]

      for (i in 2:nrow(intervals)) {
        if (intervals$min[i] <= current_max) {
          current_max <- max(current_max, intervals$max[i])
        } else {
          merged_intervals <- rbind(merged_intervals, data.frame(min = current_min, max = current_max))
          current_min <- intervals$min[i]
          current_max <- intervals$max[i]
        }
      }

      merged_intervals <- rbind(merged_intervals, data.frame(min = current_min, max = current_max))

      return(merged_intervals)
    }

    # Calculate probabilities and generate visualization
    observeEvent(input$calculate, {
      req(input$numVars, input$numIntervals)

      numVars <- input$numVars
      numIntervals <- input$numIntervals

      if (input$distribution == "uniform") {
        req(input$numRanges)
        numRanges <- input$numRanges

        ranges <- data.frame(
          min = numeric(),
          var = integer(),
          max = numeric(),
          stringsAsFactors = FALSE
        )

        for (i in 1:numRanges) {
          min_val <- input[[paste0("range", i, "_min")]]
          var_num <- as.integer(input[[paste0("range", i, "_var")]])
          max_val <- input[[paste0("range", i, "_max")]]

          if (min_val >= max_val) {
            showNotification(paste("Error in Range", i, ": Min value should be less than Max value."), type = "error")
            return(NULL)
          }

          ranges <- rbind(ranges, data.frame(min = min_val, var = var_num, max = max_val))
        }
      } else if (input$distribution == "normal") {
        ranges <- data.frame(
          mean = numeric(),
          sd = numeric(),
          var = integer(),
          stringsAsFactors = FALSE
        )

        for (i in 1:numVars) {
          mean_val <- input[[paste0("mean", i)]]
          sd_val <- input[[paste0("sd", i)]]
          ranges <- rbind(ranges, data.frame(mean = mean_val, sd = sd_val, var = i))
        }
      } else if (input$distribution == "exponential") {
        ranges <- data.frame(
          rate = numeric(),
          var = integer(),
          stringsAsFactors = FALSE
        )

        for (i in 1:numVars) {
          rate_val <- input[[paste0("rate", i)]]
          ranges <- rbind(ranges, data.frame(rate = rate_val, var = i))
        }
      } else if (input$distribution == "binomial") {
        ranges <- data.frame(
          size = integer(),
          prob = numeric(),
          var = integer(),
          stringsAsFactors = FALSE
        )

        for (i in 1:numVars) {
          size_val <- input[[paste0("size", i)]]
          prob_val <- input[[paste0("prob", i)]]

          # Validate inputs
          if (size_val < 1 || prob_val < 0 || prob_val > 1) {
            showNotification(paste("Error in Variable", i, ": Ensure n >= 1 and 0 <= p <= 1."), type = "error")
            return(NULL)
          }

          ranges <- rbind(ranges, data.frame(size = size_val, prob = prob_val, var = i))
        }
      } else if (input$distribution == "geometric") {
        ranges <- data.frame(
          prob = numeric(),
          var = integer(),
          stringsAsFactors = FALSE
        )

        for (i in 1:numVars) {
          prob_val <- input[[paste0("prob", i)]]

          # Validate input
          if (prob_val <= 0 || prob_val > 1) {
            showNotification(paste("Error in Variable", i, ": Probability should be between 0 and 1."), type = "error")
            return(NULL)
          }

          ranges <- rbind(ranges, data.frame(prob = prob_val, var = i))
        }
      }

      intervals <- data.frame(
        min = numeric(),
        var = integer(),
        max = numeric(),
        stringsAsFactors = FALSE
      )

      for (i in 1:numIntervals) {
        min_val <- input[[paste0("interval", i, "_min")]]
        var_num <- as.integer(input[[paste0("interval", i, "_var")]])
        max_val <- input[[paste0("interval", i, "_max")]]

        if (min_val > max_val) {
          showNotification(paste("Error in Interval", i, ": Min value should be less than Max value."), type = "error")
          return(NULL)
        }

        intervals <- rbind(intervals, data.frame(min = min_val, var = var_num, max = max_val))
      }

      # Calculate individual probabilities considering merged intervals
      individual_probs <- sapply(1:numVars, function(v) {
        if (input$distribution == "uniform")  {
          var_ranges <- ranges %>% filter(var == v)
          var_intervals <- intervals %>% filter(var == v)

          if (nrow(var_ranges) == 0) {
            showNotification(paste("Variable", v, "has no defined ranges."), type = "error")
            return(NA)
          }

          # Merge overlapping ranges to handle discontinuous ranges
          merged_ranges <- merge_intervals(var_ranges)
          total_range_length <- sum(merged_ranges$max - merged_ranges$min)

          if (nrow(var_intervals) == 0) {
            interval_length <- 0
          } else {
            merged_intervals <- merge_intervals(var_intervals)
            interval_length <- 0

            for (i in 1:nrow(merged_intervals)) {
              for (j in 1:nrow(merged_ranges)) {
                # Calculate the overlap of intervals with ranges
                overlap_min <- max(merged_intervals$min[i], merged_ranges$min[j])
                overlap_max <- min(merged_intervals$max[i], merged_ranges$max[j])

                if (overlap_min < overlap_max) {
                  interval_length <- interval_length + (overlap_max - overlap_min)
                }
              }
            }
          }

          prob <- interval_length / total_range_length
          return(prob)
        } else if (input$distribution == "normal") {
          mean_val <- ranges$mean[ranges$var == v]
          sd_val <- ranges$sd[ranges$var == v]
          var_intervals <- intervals %>% filter(var == v)

          if (nrow(var_intervals) == 0) {
            interval_prob <- 0
          } else {
            merged_intervals <- merge_intervals(var_intervals)
            interval_prob <- sum(sapply(1:nrow(merged_intervals), function(i) {
              pnorm(merged_intervals$max[i], mean = mean_val, sd = sd_val) -
                pnorm(merged_intervals$min[i], mean = mean_val, sd = sd_val)
            }))
          }

          return(interval_prob)
        } else if (input$distribution == "exponential") {
          rate_val <- ranges$rate[ranges$var == v]
          var_intervals <- intervals %>% filter(var == v)

          if (nrow(var_intervals) == 0) {
            interval_prob <- 0
          } else {
            merged_intervals <- merge_intervals(var_intervals)
            interval_prob <- sum(sapply(1:nrow(merged_intervals), function(i) {
              pexp(merged_intervals$max[i], rate = rate_val) -
                pexp(merged_intervals$min[i], rate = rate_val)
            }))
          }

          return(interval_prob)
        } else if (input$distribution == "binomial") {
          size_val <- ranges$size[ranges$var == v]
          prob_val <- ranges$prob[ranges$var == v]
          var_intervals <- intervals %>% filter(var == v)

          if (nrow(var_intervals) == 0) {
            interval_prob <- 0
          } else {
            # For Binomial, ensure intervals are integers
            merged_intervals <- merge_intervals(var_intervals)
            interval_prob <- sum(sapply(1:nrow(merged_intervals), function(i) {
              # Floor min and ceiling max to cover integer values
              min_int <- ceiling(merged_intervals$min[i])
              max_int <- floor(merged_intervals$max[i])
              if (min_int > max_int) return(0)
              pbinom(max_int, size = size_val, prob = prob_val) - pbinom(min_int - 1, size = size_val, prob = prob_val)
            }))
          }

          return(interval_prob)
        } else if (input$distribution == "geometric") {
          prob_val <- ranges$prob[ranges$var == v]
          var_intervals <- intervals %>% filter(var == v)

          if (nrow(var_intervals) == 0) {
            interval_prob <- 0
          } else {
            merged_intervals <- merge_intervals(var_intervals)
            interval_prob <- sum(sapply(1:nrow(merged_intervals), function(i) {
              min_int <- ceiling(merged_intervals$min[i])
              max_int <- floor(merged_intervals$max[i])

              # If invalid range, return 0
              if (min_int > max_int) return(0)

              # Geometric CDF calculation
              pgeom(max_int - 1, prob = prob_val) - pgeom(min_int - 1, prob = prob_val)
            }))
          }

          return(interval_prob)
        }
      })

      # Calculate joint probability (assuming independence)
      joint_prob <- prod(individual_probs, na.rm = TRUE)

      output$jointProbability <- renderText({
        if (is.na(joint_prob)) {
          "Joint Probability: Calculation Error"
        } else {
          paste("Joint Probability:", round(joint_prob, 4))
        }
      })

      output$individualProbabilities <- renderTable({
        data.frame(
          Variable = 1:numVars,
          Probability = sapply(individual_probs, function(x) format(x, nsmall = 4))  # Use 'nsmall' to ensure 6 decimal places
        )
      }, rownames = FALSE)

      output$visualization <- renderPlot({
        plot_list <- list()

        for (v in 1:numVars) {
          if (input$distribution == "uniform" && input$numVars == 2) {
            var1_ranges <- ranges %>% filter(var == 1)  # Ranges for variable 1
            var2_ranges <- ranges %>% filter(var == 2)  # Ranges for variable 2
            var1_intervals <- intervals %>% filter(var == 1)  # Intervals for variable 1
            var2_intervals <- intervals %>% filter(var == 2)  # Intervals for variable 2

            if (nrow(var1_ranges) > 0 && nrow(var2_ranges) > 0 && nrow(var1_intervals) > 0 && nrow(var2_intervals) > 0) {
              merged_ranges_var1 <- merge_intervals(var1_ranges)
              merged_ranges_var2 <- merge_intervals(var2_ranges)
              merged_intervals_var1 <- merge_intervals(var1_intervals)
              merged_intervals_var2 <- merge_intervals(var2_intervals)

              # Initialize empty data frames for ranges and intervals
              plot_data_ranges <- data.frame(x_min = numeric(), x_max = numeric(), y_min = numeric(), y_max = numeric())
              plot_data_intervals <- data.frame(x_min = numeric(), x_max = numeric(), y_min = numeric(), y_max = numeric())

              # Create each rectangle for the ranges
              for (i in 1:nrow(merged_ranges_var1)) {
                for (j in 1:nrow(merged_ranges_var2)) {
                  plot_data_ranges <- rbind(
                    plot_data_ranges,
                    data.frame(
                      x_min = merged_ranges_var1$min[i],
                      x_max = merged_ranges_var1$max[i],
                      y_min = merged_ranges_var2$min[j],
                      y_max = merged_ranges_var2$max[j]
                    )
                  )
                }
              }

              # Create each rectangle for the intervals
              for (i in 1:nrow(merged_intervals_var1)) {
                for (j in 1:nrow(merged_intervals_var2)) {
                  plot_data_intervals <- rbind(
                    plot_data_intervals,
                    data.frame(
                      x_min = merged_intervals_var1$min[i],
                      x_max = merged_intervals_var1$max[i],
                      y_min = merged_intervals_var2$min[j],
                      y_max = merged_intervals_var2$max[j]
                    )
                  )
                }
              }

              # Plot both the ranges (beneath) and the intervals (on top)
              p <- ggplot() +
                # Plot the ranges (background) with light blue color
                geom_rect(data = plot_data_ranges, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
                          fill = "lightblue", alpha = 0.5) +
                # Plot the intervals (foreground) with orange color
                geom_rect(data = plot_data_intervals, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
                          fill = "orange", alpha = 0.7) +
                scale_x_continuous(name = "Variable 1") +
                scale_y_continuous(name = "Variable 2") +
                theme_minimal() +
                theme(axis.text = element_text(size = 12))

              plot_list[[1]] <- p  # Add the 2D plot to the plot list
            }
          }
          else if (input$distribution == "uniform" && input$numVars != 2) {
            var_ranges <- ranges %>% filter(var == v)
            var_intervals <- intervals %>% filter(var == v)

            if (nrow(var_ranges) > 0 && nrow(var_intervals) > 0) {
              merged_intervals <- merge_intervals(var_intervals)
              p <- ggplot() +
                geom_rect(data = var_ranges, aes(xmin = min, xmax = max, ymin = v - 0.4, ymax = v + 0.4), fill = "lightblue", alpha = 0.5) +
                geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = v - 0.2, ymax = v + 0.2), fill = "orange", alpha = 0.7) +
                scale_y_continuous(breaks = v, labels = paste("Variable", v), limits = c(0.5, numVars + 0.5)) +
                labs(x = "Value", y = "") +
                theme_minimal() +
                theme(axis.text.y = element_text(size = 12))

              plot_list[[v]] <- p
            }
          } else if (input$distribution == "normal") {
            mean_val <- ranges$mean[ranges$var == v]
            sd_val <- ranges$sd[ranges$var == v]
            var_intervals <- intervals %>% filter(var == v)

            if (nrow(var_intervals) > 0) {
              merged_intervals <- merge_intervals(var_intervals)
              p_density <- ggplot() +
                stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "blue") +
                geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "orange", alpha = 0.5) +
                scale_x_continuous(name = paste("Variable", v)) +
                scale_y_continuous(name = "Density") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              # CDF plot
              p_cdf <- ggplot() +
                stat_function(fun = pnorm, args = list(mean = mean_val, sd = sd_val), color = "red") +
                geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "orange", alpha = 0.3) +
                scale_x_continuous(name = paste("Variable", v)) +
                scale_y_continuous(name = "CDF") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              # Combine the plots (if using gridExtra or patchwork to combine)
              plot_list[[v]] <- gridExtra::grid.arrange(p_density, p_cdf, ncol = 2) # Or use patchwork: (p_density + p_cdf)

              # plot_list[[v]] <- p
            }
          } else if (input$distribution == "exponential") {
            rate_val <- ranges$rate[ranges$var == v]
            var_intervals <- intervals %>% filter(var == v)

            if (nrow(var_intervals) > 0) {
              merged_intervals <- merge_intervals(var_intervals)
              p_density <- ggplot() +
                stat_function(fun = dexp, args = list(rate = rate_val), color = "blue") +
                geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "orange", alpha = 0.5) +
                scale_x_continuous(name = paste("Variable", v)) +
                scale_y_continuous(name = "Density") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              # CDF plot
              p_cdf <- ggplot() +
                stat_function(fun = pexp, args = list(rate = rate_val), color = "blue") +
                geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "orange", alpha = 0.5) +
                scale_x_continuous(name = paste("Variable", v)) +
                scale_y_continuous(name = "Cumulative Probability") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              plot_list[[v]] <- gridExtra::grid.arrange(p_density, p_cdf, ncol = 2)

            }
          } else if (input$distribution == "binomial") {
            size_val <- ranges$size[ranges$var == v]
            prob_val <- ranges$prob[ranges$var == v]
            var_intervals <- intervals %>% filter(var == v)

            if (nrow(var_intervals) > 0) {
              merged_intervals <- merge_intervals(var_intervals)

              # For Binomial, create a data frame for plotting
              binom_df <- data.frame(
                x = 0:size_val,
                y = dbinom(0:size_val, size = size_val, prob = prob_val)
              )

              # CDF data frame
              binom_cdf_df <- data.frame(
                x = 0:size_val,
                cdf = pbinom(0:size_val, size = size_val, prob = prob_val)
              )

              # Prepare data for highlighting the intervals (orange bars)
              highlight_df <- data.frame()
              for (i in 1:nrow(merged_intervals)) {
                min_int <- ceiling(merged_intervals$min[i])
                max_int <- floor(merged_intervals$max[i])
                if (min_int > max_int) next
                highlight_df <- rbind(highlight_df, data.frame(x = min_int:max_int))
              }

              # Fix: Compute binomial probabilities for highlighted intervals
              highlight_df$y <- dbinom(highlight_df$x, size = size_val, prob = prob_val)

              # Fix: Compute binomial CDF for highlighted intervals
              highlight_df$cdf <- pbinom(highlight_df$x, size = size_val, prob = prob_val)

              # Number of simulations (user-provided or predefined)
              num_simulations <- input$num_simulations

              # Perform binomial simulations
              simulation_results <- rbinom(num_simulations, size = size_val, prob = prob_val)

              # Create data frame for histogram
              sim_hist_df <- data.frame(Successes = simulation_results)

              # Normalize the histogram to match the scale of the PMF
              sim_hist_df <- sim_hist_df %>%
                group_by(Successes) %>%
                summarise(count = n()) %>%
                mutate(prob = count / num_simulations) # Normalized probabilities for comparison

              # Simulate the CDF from the simulation results
              sim_cdf_df <- sim_hist_df %>%
                arrange(Successes) %>%
                mutate(cumulative_prob = cumsum(prob)) # Cumulative sum to calculate the simulated CDF

              # Plot the PMF with overlaid histogram
              p <- ggplot(binom_df, aes(x = x, y = y)) +
                # Theoretical PMF plot (light blue bars)
                geom_bar(stat = "identity", fill = "#4A90E2", alpha = 0.5) +
                # Simulated Histogram plot overlaid (semi-transparent dark blue bars)
                geom_bar(data = sim_hist_df, aes(x = Successes, y = prob),
                         stat = "identity", fill = "#003366", color = "black", alpha = 0.3) +
                # Highlighted intervals (orange-red bars)
                geom_bar(data = highlight_df, aes(x = x, y = y),
                         stat = "identity", fill = "#FF6F61", alpha = 0.6) +
                scale_x_continuous(breaks = 0:size_val) +
                labs(x = paste("Variable", v), y = "Probability") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              # CDF plot (with both theoretical and simulated CDFs)
              p_cdf <- ggplot(binom_cdf_df, aes(x = x, y = cdf)) +
                # Theoretical CDF plot (light blue bars)
                geom_bar(stat = "identity", fill = "#4A90E2", alpha = 0.5) +
                # Simulated CDF (overlaid, semi-transparent dark blue)
                geom_line(data = sim_cdf_df, aes(x = Successes, y = cumulative_prob),
                          color = "#003366", size = 1.5, linetype = "solid", alpha = 0.7) +
                # Highlighted intervals (orange-red bars for the theoretical CDF)
                geom_bar(data = highlight_df, aes(x = x, y = cdf),
                         stat = "identity", fill = "#FF6F61", alpha = 0.6) +
                scale_x_continuous(breaks = 0:size_val) +
                labs(x = paste("Variable", v), y = "Cumulative Probability (CDF)") +
                theme_minimal() +
                theme(axis.text.x = element_text(size = 12))

              # Combine both plots into the plot list
              plot_list[[v]] <- gridExtra::grid.arrange(p, p_cdf, ncol = 2)
            }
          } else if (input$distribution == "geometric") {
            prob_val <- ranges$prob[ranges$var == v]
            var_intervals <- intervals %>% filter(var == v)

            if (nrow(var_intervals) > 0) {
              merged_intervals <- merge_intervals(var_intervals)

              # Prepare data for plotting
              max_plot_val <- max(floor(max(intervals$max)), 10)
              geom_df <- data.frame(
                x = 0:max_plot_val,
                y = dgeom(0:max_plot_val, prob = prob_val)
              )

              # CDF data
              geom_cdf_df <- data.frame(
                x = 0:max_plot_val,
                cdf = pgeom(0:max_plot_val, prob = prob_val)
              )

              # Highlighted intervals
              highlight_df <- data.frame()
              for (i in 1:nrow(merged_intervals)) {
                min_int <- ceiling(merged_intervals$min[i])
                max_int <- floor(merged_intervals$max[i])
                if (min_int > max_int) next
                highlight_df <- rbind(highlight_df, data.frame(x = min_int:max_int))
              }
              highlight_df$y <- dgeom(highlight_df$x, prob = prob_val)
              highlight_df$cdf <- pgeom(highlight_df$x, prob = prob_val)

              # Plot PMF
              p <- ggplot(geom_df, aes(x = x, y = y)) +
                geom_bar(stat = "identity", fill = "skyblue", alpha = 0.5) +
                geom_bar(data = highlight_df, aes(x = x, y = y), stat = "identity", fill = "orange", alpha = 0.7) +
                labs(x = paste("Variable", v), y = "PMF") +
                theme_minimal()

              # CDF Plot
              p_cdf <- ggplot(geom_cdf_df, aes(x = x, y = cdf)) +
                #geom_line(color = "blue") +
                geom_bar(stat = "identity", fill = "#4A90E2", alpha = 0.5) +
                geom_bar(data = highlight_df, aes(x = x, y = cdf), stat = "identity", fill = "orange", alpha = 0.7) +
                labs(x = paste("Variable", v), y = "CDF") +
                theme_minimal()

              # Combine plots using gridExtra
              plot_list[[v]] <- gridExtra::grid.arrange(p, p_cdf, ncol = 2)
            }
          }
        }

        if (length(plot_list) > 0) {
          gridExtra::grid.arrange(grobs = plot_list, ncol = 1)
        }
      })
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
# run_distribution_visualizer_app()
