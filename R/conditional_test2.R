library(shiny)
library(ggplot2)
library(ggforce)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Probability Concepts Visualization: Union, Intersection, Complement, Difference"),

  sidebarLayout(
    sidebarPanel(
      numericInput("pA", "Probability of Event A (P(A)):", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("pB", "Probability of Event B (P(B)):", value = 0.3, min = 0, max = 1, step = 0.01),
      numericInput("pAB", "Probability of Intersection (P(A ∩ B)):", value = 0.1, min = 0, max = 1, step = 0.01),
      actionButton("update", "Update Visualization")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Venn Diagram",
                 plotOutput("vennPlot"),
                 verbatimTextOutput("probabilityTextVenn")),
        tabPanel("Union",
                 plotOutput("unionPlot"),
                 verbatimTextOutput("probabilityTextUnion")),
        tabPanel("Intersection",
                 plotOutput("intersectionPlot"),
                 verbatimTextOutput("probabilityTextIntersection")),
        tabPanel("Difference A-B",
                 plotOutput("diffABPlot"),
                 verbatimTextOutput("probabilityTextDiffAB")),
        tabPanel("Difference B-A",
                 plotOutput("diffBAPlot"),
                 verbatimTextOutput("probabilityTextDiffBA")),
        tabPanel("Concepts",
                 verbatimTextOutput("conceptText"))
      )
    )
  )
)

server <- function(input, output, session) {

  probabilities <- eventReactive(input$update, {
    pA <- input$pA
    pB <- input$pB
    pAB <- input$pAB

    # Ensure valid intersection probability
    pAB <- min(pAB, pA, pB)

    list(pA = pA, pB = pB, pAB = pAB)
  })

  calculate_distance <- function(pA, pB, pAB) {
    rA <- sqrt(pA)
    rB <- sqrt(pB)
    if (pAB == 0) {
      return(rA + rB)  # No intersection
    } else if (pAB == min(pA, pB)) {
      return(abs(rA - rB))  # One circle completely inside the other
    } else {

      return(sqrt(rA^2 + rB^2 - 2 * rA * rB * cos((2 * acos((rA^2 + rB^2 - (rA + rB - pAB)^2) / (2 * rA * rB))) / 2)))
    }
  }

  render_venn <- function(highlight = NULL) {
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB

    rA <- sqrt(pA)
    rB <- sqrt(pB)

    d <- calculate_distance(pA, pB, pAB)

    xA <- 0
    xB <- d

    plot <- ggplot() +
      geom_circle(aes(x0 = xA, y0 = 0, r = rA), fill = "skyblue", alpha = if (is.null(highlight) || highlight == "A") 0.5 else 0.2) +
      geom_circle(aes(x0 = xB, y0 = 0, r = rB), fill = "salmon", alpha = if (is.null(highlight) || highlight == "B") 0.5 else 0.2) +
      coord_fixed() +
      theme_void()

    if (highlight == "union") {
      plot <- plot +
        geom_circle(aes(x0 = xA, y0 = 0, r = rA), fill = "red", alpha = 0.5) +
        geom_circle(aes(x0 = xB, y0 = 0, r = rB), fill = "red", alpha = 0.5)
    } else if (highlight == "intersection") {
      plot <- plot +
        geom_circle(aes(x0 = xA, y0 = 0, r = rA), fill = "red", alpha = 0.5) +
        geom_circle(aes(x0 = xB, y0 = 0, r = rB), fill = "red", alpha = 0.5)
    } else if (highlight == "diffAB") {
      plot <- plot +
        geom_circle(aes(x0 = xA, y0 = 0, r = rA), fill = "lightblue", alpha = 0.5) +
        geom_circle(aes(x0 = xB, y0 = 0, r = rB), fill = "red", alpha = 0.2)
    } else if (highlight == "diffBA") {
      plot <- plot +
        geom_circle(aes(x0 = xA, y0 = 0, r = rA), fill = "red", alpha = 0.2) +
        geom_circle(aes(x0 = xB, y0 = 0, r = rB), fill = "lightblue", alpha = 0.5)
    }

    plot <- plot +
      xlim(min(xA - rA - 0.5, xB - rB - 0.5), max(xA + rA + 0.5, xB + rB + 0.5)) +
      ylim(-rA - 0.5, rA + 0.5) +
      annotate("text", x = xA - rA - 0.2, y = 0, label = paste0("A = ", round(pA, 2)), size = 5, hjust = 1) +
      annotate("text", x = xB + rB + 0.2, y = 0, label = paste0("B = ", round(pB, 2)), size = 5, hjust = 0) +
      annotate("text", x = (xA + xB) / 2, y = rA + 0.2, label = paste0("A ∩ B = ", round(pAB, 2)), size = 5) +
      ggtitle(ifelse(is.null(highlight), "Venn Diagram of Events A and B", highlight)) +
      theme(plot.title = element_text(hjust = 0.5))

    return(plot)
  }

  output$vennPlot <- renderPlot({
    render_venn("")
  })

  output$unionPlot <- renderPlot({
    render_venn("union")
  })

  output$intersectionPlot <- renderPlot({
    render_venn("intersection")
  })

  output$diffABPlot <- renderPlot({
    render_venn("diffAB")
  })

  output$diffBAPlot <- renderPlot({
    render_venn("diffBA")
  })

  output$probabilityTextVenn <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_union_B <- pA + pB - pAB

    cat("Probabilities:\n")
    cat(sprintf("P(A) = %.3f\n", pA))
    cat(sprintf("P(B) = %.3f\n", pB))
    cat(sprintf("P(A ∩ B) = %.3f\n", pAB))
    cat(sprintf("P(A ∪ B) = %.3f\n", pA_union_B))
  })

  output$probabilityTextUnion <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_union_B <- pA + pB - pAB

    cat("Union:\n")
    cat(sprintf("P(A ∪ B) = %.3f\n", pA_union_B))
  })

  output$probabilityTextIntersection <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB

    cat("Intersection:\n")
    cat(sprintf("P(A ∩ B) = %.3f\n", pAB))
  })

  output$probabilityTextDiffAB <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_diff_B <- pA - pAB

    cat("Difference (A - B):\n")
    cat(sprintf("P(A - B) = %.3f\n", pA_diff_B))
  })

  output$probabilityTextDiffBA <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pB_diff_A <- pB - pAB

    cat("Difference (B - A):\n")
    cat(sprintf("P(B - A) = %.3f\n", pB_diff_A))
  })

  output$conceptText <- renderPrint({
    probs <- probabilities()
    pA <- probs$pA
    pB <- probs$pB
    pAB <- probs$pAB
    pA_union_B <- pA + pB - pAB
    pA_diff_B <- pA - pAB
    pB_diff_A <- pB - pAB

    cat("Concepts:\n")
    cat(sprintf("P(A) = %.3f: Probability of event A occurring.\n", pA))
    cat(sprintf("P(B) = %.3f: Probability of event B occurring.\n", pB))
    cat(sprintf("P(A ∩ B) = %.3f: Probability of both A and B occurring (Intersection).\n", pAB))
    cat(sprintf("P(A ∪ B) = %.3f: Probability of either A or B or both occurring (Union).\n", pA_union_B))
    cat(sprintf("P(A - B) = %.3f: Probability of A occurring without B (Difference A-B).\n", pA_diff_B))
    cat(sprintf("P(B - A) = %.3f: Probability of B occurring without A (Difference B-A).\n", pB_diff_A))
  })
}

shinyApp(ui = ui, server = server)
