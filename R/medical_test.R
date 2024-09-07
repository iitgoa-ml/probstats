library(shiny)
library(ggplot2)
library(ggforce) # For drawing circles in Venn diagrams
library(dplyr)
library(shinythemes)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Medical Test Probability Visualizer"),

  sidebarLayout(
    sidebarPanel(
      numericInput("pDisease", "Probability of Disease (P(Disease)):", value = 0.01, min = 0, max = 1, step = 0.01),
      numericInput("pTestPosGivenDisease", "Probability of Positive Test given Disease (P(Test Positive | Disease)):", value = 0.9, min = 0, max = 1, step = 0.01),
      numericInput("pTestPosGivenNoDisease", "Probability of Positive Test given No Disease (P(Test Positive | No Disease)):", value = 0.05, min = 0, max = 1, step = 0.01),
      actionButton("update", "Update Visualizations")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Venn Diagram", plotOutput("vennPlot")),
        tabPanel("Probability Tree", plotOutput("treePlot")),
        tabPanel("Conditional Probability",
                 verbatimTextOutput("condProbText"),
                 plotOutput("condProbPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  probabilities <- eventReactive(input$update, {
    pDisease <- input$pDisease
    pTestPosGivenDisease <- input$pTestPosGivenDisease
    pTestPosGivenNoDisease <- input$pTestPosGivenNoDisease

    pNoDisease <- 1 - pDisease
    pTestPos <- pDisease * pTestPosGivenDisease + pNoDisease * pTestPosGivenNoDisease
    pDiseaseGivenTestPos <- (pTestPosGivenDisease * pDisease) / pTestPos
    pNoDiseaseGivenTestPos <- (pTestPosGivenNoDisease * pNoDisease) / pTestPos

    list(pDisease = pDisease, pNoDisease = pNoDisease, pTestPosGivenDisease = pTestPosGivenDisease,
         pTestPosGivenNoDisease = pTestPosGivenNoDisease, pTestPos = pTestPos,
         pDiseaseGivenTestPos = pDiseaseGivenTestPos)
  })

  output$vennPlot <- renderPlot({
    probs <- probabilities()
    pDisease <- probs$pDisease
    pTestPosGivenDisease <- probs$pTestPosGivenDisease
    pTestPosGivenNoDisease <- probs$pTestPosGivenNoDisease
    pTestPos <- probs$pTestPos

    ggplot() +
      geom_circle(aes(x0=0, y0=0, r=1), fill="skyblue", alpha=0.5) +
      geom_circle(aes(x0=1.5, y0=0, r=1), fill="salmon", alpha=0.5) +
      annotate("text", x=-0.5, y=0, label = paste0("Disease = ", round(pDisease, 2)), size=5) +
      annotate("text", x=2.0, y=0, label = paste0("Test Positive = ", round(probs$pTestPos, 2)), size=5) +
      annotate("text", x=0.75, y=0, label = paste0("Overlap = ", round(pTestPosGivenDisease * pDisease, 2)), size=5) +
      theme_void() +
      ggtitle("Venn Diagram of Disease and Positive Test")
  })

  output$treePlot <- renderPlot({
    probs <- probabilities()
    pDisease <- probs$pDisease
    pNoDisease <- probs$pNoDisease
    pTestPosGivenDisease <- probs$pTestPosGivenDisease
    pTestPosGivenNoDisease <- probs$pTestPosGivenNoDisease
    pTestPos <- probs$pTestPos
    pDiseaseGivenTestPos <- probs$pDiseaseGivenTestPos

    tree_data <- data.frame(
      step = rep(c("Start", "Disease", "No Disease"), each = 2),
      outcome = c("Disease", "No Disease", "Positive Test", "Negative Test", "Positive Test", "Negative Test"),
      prob = c(pDisease, pNoDisease, pTestPosGivenDisease, 1 - pTestPosGivenDisease, pTestPosGivenNoDisease, 1 - pTestPosGivenNoDisease)
    )

    ggplot(tree_data, aes(x = step, y = prob, color = outcome)) +
      geom_segment(aes(xend = step, yend = 0), size = 1) +
      geom_text(aes(label = sprintf("%.2f", prob)), hjust = -0.2) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      ggtitle("Probability Tree for Medical Test")
  })

  output$condProbText <- renderPrint({
    probs <- probabilities()
    pDisease <- probs$pDisease
    pTestPosGivenDisease <- probs$pTestPosGivenDisease
    pTestPos <- probs$pTestPos
    pDiseaseGivenTestPos <- probs$pDiseaseGivenTestPos

    cat("Conditional Probabilities:\n")
    cat(sprintf("P(Disease | Positive Test) = (P(Positive Test | Disease) * P(Disease)) / P(Positive Test) = %.3f\n", pDiseaseGivenTestPos))
  })

  output$condProbPlot <- renderPlot({
    probs <- probabilities()
    pDiseaseGivenTestPos <- probs$pDiseaseGivenTestPos

    cond_probs <- data.frame(
      Condition = "P(Disease | Positive Test)",
      Probability = pDiseaseGivenTestPos
    )

    ggplot(cond_probs, aes(x = Condition, y = Probability, fill = Condition)) +
      geom_bar(stat = "identity") +
      ylim(0, 1) +
      geom_text(aes(label = sprintf("%.2f", Probability)), vjust = -0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle("Conditional Probability")
  })
}

shinyApp(ui = ui, server = server)
