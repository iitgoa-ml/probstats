library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)


#' Run Continuous Sample Space Probability Calculator App
#'
#' This function launches the Shiny app for calculating probabilities in continuous sample spaces.
#' Users can select between Uniform and Normal distributions, define variable ranges and intervals,
#' and calculate both joint and individual probabilities. The app also provides visualizations
#' of the defined ranges and intervals.
#'
#' @return The Shiny app is launched in the default web browser.
#' @examples
#' if (interactive()) {
#'   run_continuous_app()
#' }
#' @export

run_continuous_app <- function() {
  # Define UI
ui <- fluidPage(
  titlePanel("Continuous Sample Space Probability Calculator"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("distribution", "Select Distribution",
                  choices = c("Uniform" = "uniform", "Normal" = "normal")),

      numericInput("numVars", "Number of Variables:", value = 2, min = 1),
      hr(),

      uiOutput("distSpecificInputs"),
      hr(),

      h3("Intervals"),
      numericInput("numIntervals", "Number of Intervals:", value = 1, min = 1),
      uiOutput("intervalInputs"),
      hr(),

      actionButton("calculate", "Calculate Probability", class = "btn-primary")
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
    }
  })

  # Generate UI inputs for variable ranges (only for Uniform)
  output$rangeInputs <- renderUI({
    numRanges <- input$numRanges
    numVars <- input$numVars

    lapply(1:numRanges, function(i) {
      fluidRow(
        column(4, numericInput(paste0("range", i, "_min"), paste("Range", i, "Min Value:"), value = 0)),
        column(4, selectInput(paste0("range", i, "_var"), paste("Range", i, "Variable:"), choices = 1:numVars)),
        column(4, numericInput(paste0("range", i, "_max"), paste("Range", i, "Max Value:"), value = 1))
      )
    })
  })

  # Generate UI inputs for intervals
  output$intervalInputs <- renderUI({
    numIntervals <- input$numIntervals
    numVars <- input$numVars

    lapply(1:numIntervals, function(i) {
      fluidRow(
        column(4, numericInput(paste0("interval", i, "_min"), paste("Interval", i, "Min Value:"), value = 0)),
        column(4, selectInput(paste0("interval", i, "_var"), paste("Interval", i, "Variable:"), choices = 1:numVars)),
        column(4, numericInput(paste0("interval", i, "_max"), paste("Interval", i, "Max Value:"), value = 0.5))
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

      if (min_val >= max_val) {
        showNotification(paste("Error in Interval", i, ": Min value should be less than Max value."), type = "error")
        return(NULL)
      }

      intervals <- rbind(intervals, data.frame(min = min_val, var = var_num, max = max_val))
    }

    # Calculate individual probabilities considering merged intervals
    individual_probs <- sapply(1:numVars, function(v) {
      if (input$distribution == "uniform") {
        var_ranges <- ranges %>% filter(var == v)
        var_intervals <- intervals %>% filter(var == v)

        if (nrow(var_ranges) == 0) {
          showNotification(paste("Variable", v, "has no defined ranges."), type = "error")
          return(NA)
        }

        total_range_length <- sum(var_ranges$max - var_ranges$min)

        if (nrow(var_intervals) == 0) {
          interval_length <- 0
        } else {
          merged_intervals <- merge_intervals(var_intervals)
          interval_length <- sum(merged_intervals$max - merged_intervals$min)
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

#   output$individualProbabilities <- renderTable({
#      data.frame(
#        Variable = 1:numVars,
#        Probability = round(individual_probs, 4)
#      )
#    })
    output$individualProbabilities <- renderTable({
      data.frame(
        Variable = 1:numVars,
        Probability = sapply(individual_probs, function(x) format(x, nsmall = 4))  # Use 'nsmall' to ensure 6 decimal places
      )
    }, rownames = FALSE)

    output$visualization <- renderPlot({
      plot_list <- list()

      for (v in 1:numVars) {
        if (input$distribution == "uniform") {
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
            p <- ggplot() +
              stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "blue") +
              geom_rect(data = merged_intervals, aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "orange", alpha = 0.5) +
              scale_x_continuous(name = paste("Variable", v)) +
              scale_y_continuous(name = "Density") +
              theme_minimal() +
              theme(axis.text.x = element_text(size = 12))

            plot_list[[v]] <- p
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
# shiny::shinyApp(ui = ui, server = server)
# Run the application
# shinyApp(ui = ui, server = server)
