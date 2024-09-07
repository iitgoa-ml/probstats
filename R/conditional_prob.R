library(shiny)
library(ggplot2)
library(ggforce) # For drawing circles in Venn diagrams
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Interactive Conditional Probability Visualizer"),

  sidebarLayout(
    sidebarPanel(
      numericInput("pA", "Probability of Event A (P(A)):", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("pB", "Probability of Event B (P(B)):", value = 0.5, min = 0, max = 1, step = 0.01),
      checkboxInput("independent", "Assume A and B are independent?", value = TRUE),
      conditionalPanel(
        condition = "!input.independent",
        selectInput("inputType", "Choose input type:",
                    choices = list("P(A ∩ B)" = "pAB", "P(B | A)" = "pB_given_A")),
        uiOutput("dynamicInput")
      ),
      actionButton("update", "Update Visualizations")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Venn Diagram", plotOutput("vennPlot")),
        tabPanel("Probability Tree", plotOutput("treePlot")),
        tabPanel("Conditional Probabilities",
                 verbatimTextOutput("conditionalText"),
                 plotOutput("conditionalPlot"))
      )
    )
  )
)

server <- function(input, output, session) {

  output$dynamicInput <- renderUI({
    if (input$inputType == "pAB") {
      numericInput("pAB", "Probability of A and B occurring together (P(A ∩ B)):",
                   value = 0.25, min = 0, max = 1, step = 0.01)
    } else {
      numericInput("pB_given_A", "Probability of B given A (P(B | A)):",
                   value = 0.5, min = 0, max = 1, step = 0.01)
    }
  })

  # Reactive values to store probabilities
  probabilities <- eventReactive(input$update, {
    pA <- input$pA
    pB <- input$pB
    if (input$independent) {
      pAB <- pA * pB
    } else {
      if (input$inputType == "pAB") {
        pAB <- input$pAB
      } else {
        pB_given_A <- input$pB_given_A
        pAB <- pB_given_A * pA
      }
      # Ensure that P(A ∩ B) <= min(P(A), P(B))
      pAB <- min(pAB, min(pA, pB))
    }
    list(pA = pA, pB = pB, pAB = pAB)
  })

  # Venn Diagram
  output$vennPlot <- renderPlot({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB

    # Simple Venn Diagram using ggforce
    ggplot() +
      geom_circle(aes(x0=0, y0=0, r=1), fill="skyblue", alpha=0.5) +
      geom_circle(aes(x0=1.5, y0=0, r=1), fill="salmon", alpha=0.5) +
      annotate("text", x=-0.5, y=0, label = paste0("A = ", pA), size=5) +
      annotate("text", x=2.0, y=0, label = paste0("B = ", pB), size=5) +
      annotate("text", x=0.75, y=0, label = paste0("A ∩ B = ", pAB), size=5) +
      theme_void() +
      ggtitle("Venn Diagram of Events A and B")
  })

  # Probability Tree
  output$treePlot <- renderPlot({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pB_given_A <- ifelse(probs$pA > 0, probs$pAB / probs$pA, 0)
    pA_given_B <- ifelse(probs$pB > 0, probs$pAB / probs$pB, 0)

    # Create data for tree
    tree_data <- data.frame(
      step = rep(c("Start", "A", "¬A"), each = 2),
      outcome = c("A", "¬A", "B", "¬B", "B", "¬B"),
      prob = c(pA, 1 - pA, pB_given_A, 1 - pB_given_A, pA_given_B, 1 - pA_given_B)
    )

    ggplot() +
      geom_segment(aes(x=0, y=0, xend=1, yend=1), arrow = arrow()) +
      geom_segment(aes(x=0, y=0, xend=1, yend=-1), arrow = arrow()) +
      geom_segment(aes(x=1, y=1, xend=2, yend=1.5), arrow = arrow()) +
      geom_segment(aes(x=1, y=1, xend=2, yend=0.5), arrow = arrow()) +
      geom_segment(aes(x=1, y=-1, xend=2, yend=-0.5), arrow = arrow()) +
      geom_segment(aes(x=1, y=-1, xend=2, yend=-1.5), arrow = arrow()) +
      annotate("text", x=0.5, y=0.5, label = paste0("A (", pA, ")")) +
      annotate("text", x=0.5, y=-0.5, label = paste0("¬A (", 1 - pA, ")")) +
      annotate("text", x=1.5, y=1.5, label = paste0("B | A (", round(pB_given_A, 2), ")")) +
      annotate("text", x=1.5, y=0.5, label = paste0("¬B | A (", round(1 - pB_given_A, 2), ")")) +
      annotate("text", x=1.5, y=-0.5, label = paste0("B | ¬A (", round(pA_given_B, 2), ")")) +
      annotate("text", x=1.5, y=-1.5, label = paste0("¬B | ¬A (", round(1 - pA_given_B, 2), ")")) +
      theme_void() +
      ggtitle("Probability Tree for Events A and B")
  })

  # Conditional Probabilities Text
  output$conditionalText <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_given_B <- ifelse(pB > 0, pAB / pB, NA)
    pB_given_A <- ifelse(pA > 0, pAB / pA, NA)

    cat("Conditional Probabilities:\n")
    cat(sprintf("P(A | B) = P(A ∩ B) / P(B) = %.3f / %.3f = %.3f\n", pAB, pB, pA_given_B))
    cat(sprintf("P(B | A) = P(A ∩ B) / P(A) = %.3f / %.3f = %.3f\n", pAB, pA, pB_given_A))
  })

  # Conditional Probabilities Plot
  output$conditionalPlot <- renderPlot({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_given_B <- ifelse(pB > 0, pAB / pB, NA)
    pB_given_A <- ifelse(pA > 0, pAB / pA, NA)

    cond_probs <- data.frame(
      Condition = c("P(A | B)", "P(B | A)"),
      Probability = c(pA_given_B, pB_given_A)
    )

    ggplot(cond_probs, aes(x=Condition, y=Probability, fill=Condition)) +
      geom_bar(stat="identity") +
      ylim(0,1) +
      geom_text(aes(label=round(Probability, 3)), vjust=-0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle("Conditional Probabilities")
  })
}

shinyApp(ui = ui, server = server)
