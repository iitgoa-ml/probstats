

#' Run the Probability Tree Shiny App
#'
#' This function launches a Shiny application that visualizes a probability tree
#' for disease prevalence, test sensitivity, and test specificity.
#'
#' @import shiny
#' @import ggplot2
#' @examples
#' \dontrun{
#' run_probability_tree_app() # Launch the app
#' }
#' @export
run_probability_tree_app <- function() {
  library(shiny)
  library(ggplot2)


ui <- fluidPage(
  titlePanel("Probability Tree for Disease and Test Result"),

  sidebarLayout(
    sidebarPanel(
      numericInput("pD", "Prevalence (P(D)):", 0.01, min = 0, max = 1, step = 0.01),
      numericInput("pTP", "Sensitivity (P(T | D)):", 0.9, min = 0, max = 1, step = 0.01),
      numericInput("pTN", "Specificity (P(~T | ~D)):", 0.95, min = 0, max = 1, step = 0.01),
      actionButton("recalc", "Recalculate")
    ),

    mainPanel(
      h3("Disease First Probability Tree"),
      plotOutput("treePlotDiseaseFirst"),

      h3("Test First Probability Tree"),
      plotOutput("treePlotTestFirst")
    )
  )
)

server <- function(input, output) {

  observeEvent(input$recalc, {

    # Input probabilities
    pD <- input$pD                  # P(D)
    pND <- 1 - pD                   # P(~D)
    pTP <- input$pTP                # P(T | D)
    pTN <- input$pTN                # P(~T | ~D)
    pFN <- 1 - pTP                  # P(~T | D)
    pFP <- 1 - pTN                  # P(T | ~D)

    # 1. Disease First Probability Tree Calculations
    probs <- list()
    probs$pT_given_D <- pTP
    probs$pT_given_ND <- pFP

    # Total probabilities for Disease First Tree
    total_p_D_T <- pTP * pD
    total_p_ND_T <- pFP * pND
    total_p_D_F <- pFN * pD
    total_p_ND_F <- pTN * pND

    # Data for Disease First Probability Tree
    tree_df_disease_first <- data.frame(
      x_start = c(0.5, 0.5, 0.25, 0.25, 0.75, 0.75),
      y_start = c(1, 1, 0.75, 0.75, 0.75, 0.75),
      x_end = c(0.25, 0.75, 0.125, 0.375, 0.625, 0.875),
      y_end = c(0.75, 0.75, 0.5, 0.5, 0.5, 0.5),
      label = c(
        paste0("Disease (P(D) = ", round(pD, 2), ")"),
        paste0("No Disease (P(~D) = ", round(pND, 2), ")"),
        paste0("Test +ve (P(T|D) = ", round(pTP, 2), ")", "\nP = ", round(total_p_D_T, 4)),
        paste0("Test -ve (P(~T|D) = ", round(pFN, 2), ")", "\nP = ", round(total_p_D_F, 4)),
        paste0("Test +ve (P(T|~D) = ", round(pFP, 2), ")", "\nP = ", round(total_p_ND_T, 4)),
        paste0("Test -ve (P(~T|~D) = ", round(pTN, 2), ")", "\nP = ", round(total_p_ND_F, 4))
      ),
      event = c("Disease", "No Disease", "Test Positive", "Test Negative", "Test Positive", "Test Negative"),
      color = c("red", "blue", "green", "green", "orange", "orange"),
      stringsAsFactors = FALSE
    )

    # 2. Test First Probability Tree Calculations
    # Total probability of a positive test
    pT <- (pTP * pD) + (pFP * pND)

    # Total probability of a negative test
    p_not_T <- 1 - pT

    # Probability of Disease given a positive test (using Bayes' theorem)
    probs$pD_given_T <- (pTP * pD) / pT

    # Probability of Disease given a negative test
    pD_given_not_T <- (pFN * pD) / p_not_T

    # Probability of No Disease given a negative test
    pND_given_not_T <- (pTN * pND) / p_not_T

    # Total probabilities for Test First Tree
    total_p_D_T <- probs$pD_given_T * pT
    total_p_ND_T <- (1 - probs$pD_given_T) * pT
    total_p_D_F <- pD_given_not_T * p_not_T
    total_p_ND_F <- pND_given_not_T * p_not_T

    # Data for Test First Probability Tree
    tree_df_test_first <- data.frame(
      x_start = c(0.5, 0.5, 0.25, 0.25, 0.75, 0.75),
      y_start = c(1, 1, 0.75, 0.75, 0.75, 0.75),
      x_end = c(0.25, 0.75, 0.125, 0.375, 0.625, 0.875),
      y_end = c(0.75, 0.75, 0.5, 0.5, 0.5, 0.5),
      label = c(
        paste0("Test +ve (P(T) = ", round(pT, 2), ")"),
        paste0("Test -ve (P(~T) = ", round(p_not_T, 2), ")"),
        paste0("Disease | Test +ve (P(D|T) = ", round(probs$pD_given_T, 2), ")", "\nP = ", round(total_p_D_T, 4)),
        paste0("No Disease | Test +ve (P(~D|T) = ", round(1 - probs$pD_given_T, 2), ")", "\nP = ", round(total_p_ND_T, 4)),
        paste0("Disease | Test -ve (P(D|~T) = ", round(pD_given_not_T, 2), ")", "\nP = ", round(total_p_D_F, 4)),
        paste0("No Disease | Test -ve (P(~D|~T) = ", round(pND_given_not_T, 2), ")", "\nP = ", round(total_p_ND_F, 4))
      ),
      event = c("Test Positive", "Test Negative", "Disease", "No Disease", "Disease", "No Disease"),
      color = c("green", "green", "red", "blue", "red", "blue"),
      stringsAsFactors = FALSE
    )

    # Plotting functions with adjusted label positions
    output$treePlotDiseaseFirst <- renderPlot({
      ggplot(tree_df_disease_first) +
        xlim(0, 1) +  # Adjust limits to ensure there is space

        geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = event), size = 1.2) +
        geom_text(data = tree_df_disease_first[1:2, ], # First level labels beside nodes
                  aes(x = x_end, y = y_end, label = label),
                  hjust = c(1.2, -0.2), size = 4, color = "black") +
        geom_text(data = tree_df_disease_first[3:6, ], # Second level labels below nodes
                  aes(x = x_end, y = y_end - 0.1, label = label), size = 4, color = "black") +
        geom_point(aes(x = x_start, y = y_start), size = 4, shape = 21, fill = "blue") +
        geom_point(aes(x = x_end, y = y_end), size = 4, shape = 21, fill = "green") +
        scale_color_manual(values = c("Disease" = "red", "No Disease" = "blue", "Test Positive" = "green", "Test Negative" = "orange")) +
        theme_void()
    })

    output$treePlotTestFirst <- renderPlot({
      ggplot(tree_df_test_first) +
        xlim(0, 1) +  # Adjust limits to ensure there is space

        geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = event), size = 1.2) +
        geom_text(data = tree_df_test_first[1:2, ], # First level labels beside nodes
                  aes(x = x_end, y = y_end, label = label),
                  hjust = c(1.2, -0.2), size = 4, color = "black") +
        geom_text(data = tree_df_test_first[3:6, ], # Second level labels below nodes
                  aes(x = x_end, y = y_end - 0.1, label = label), size = 4, color = "black") +
        geom_point(aes(x = x_start, y = y_start), size = 4, shape = 21, fill = "blue") +
        geom_point(aes(x = x_end, y = y_end), size = 4, shape = 21, fill = "green") +
        scale_color_manual(values = c("Disease" = "red", "No Disease" = "blue", "Test Positive" = "green", "Test Negative" = "orange")) +
        theme_void()
    })
  })
}

shinyApp(ui, server)
}
run_probability_tree_app()
