library(shiny)


#' Launch the Probability Calculator with Operations Shiny App
#'
#' The `run_discrete_app` function initializes and launches a Shiny web application
#' designed to calculate and visualize probabilities based on discrete outcomes and
#' events. The app allows users to define outcomes, events, and trials, input
#' probabilities for each outcome, and perform operations such as union,
#' intersection, and complement on the defined events. The result of the operations,
#' along with the event probabilities, is then displayed for the user.
#'
#' @details
#' The app provides an interactive interface where users can:
#' \itemize{
#'   \item Define the number of outcomes, events, and trials.
#'   \item Input the probability for each outcome.
#'   \item Assign outcomes to each trial within an event.
#'   \item Perform operations on the events (union, intersection, complement).
#'   \item View the calculated probabilities for each event and the result of the selected operation.
#' }
#'
#' @return
#' A Shiny web application object that runs locally in the user's web browser.
#'
#' @examples
#' if (interactive()) {
#'   run_discrete_app()
#' }
#'
#' @export
run_discrete_app <- function() {

  ui <- fluidPage(
  titlePanel("Probability Calculator with Operations"),

  sidebarLayout(
    sidebarPanel(
      numericInput("numOutcomes", "Number of Outcomes:", value = 2, min = 2),
      numericInput("numEvents", "Number of Events:", value = 1, min = 1),
      numericInput("numTrials", "Number of Trials per Event:", value = 1, min = 1),
      uiOutput("probabilities"),
      actionButton("calculate", "Calculate Probabilities"),
      hr(),
      h4("Operations"),
      uiOutput("operationSelect"),
      uiOutput("eventSelect"),
      actionButton("performOperation", "Perform Operation")
    ),

    mainPanel(
      uiOutput("eventOutputs"),
      verbatimTextOutput("result"),
      verbatimTextOutput("operationResult")
    )
  )
)

server <- function(input, output, session) {

  # UI for probability inputs based on the number of outcomes
  output$probabilities <- renderUI({
    numOutcomes <- input$numOutcomes
    lapply(1:numOutcomes, function(i) {
      numericInput(paste0("prob", i), paste("Probability of Outcome ", i, ":"),
                   value = 1/numOutcomes, min = 0, max = 1, step = 0.01)
    })
  })

  # Generate event outputs based on the number of events and trials
  output$eventOutputs <- renderUI({
    numEvents <- input$numEvents
    numTrials <- input$numTrials
    lapply(1:numEvents, function(eventNum) {
      fluidRow(
        column(12, h4(paste("Event", eventNum))),
        lapply(1:numTrials, function(trialNum) {
          column(12 / numTrials,
                 checkboxGroupInput(paste0("event", eventNum, "trial", trialNum),
                                    label = paste("Trial", trialNum),
                                    choices = 1:input$numOutcomes,
                                    selected = NULL)
          )
        }),
        hr()
      )
    })
  })

  # UI to select events and operation type
  output$operationSelect <- renderUI({
    selectInput("operation", "Select Operation",
                choices = c("Union" = "union", "Intersection" = "intersection", "Complement" = "complement"))
  })

  # Conditional UI to select events based on the chosen operation
  output$eventSelect <- renderUI({
    numEvents <- input$numEvents
    if (input$operation == "complement") {
      selectInput("event1", "Select Event", choices = 1:numEvents)
    } else {
      tagList(
        selectInput("event1", "Select Event 1", choices = 1:numEvents),
        selectInput("event2", "Select Event 2", choices = 1:numEvents)
      )
    }
  })

  # Calculate and display probabilities when the button is clicked
  observeEvent(input$calculate, {
    numEvents <- input$numEvents
    numTrials <- input$numTrials
    probabilities <- sapply(1:input$numOutcomes, function(i) input[[paste0("prob", i)]])

    eventProbs <- sapply(1:numEvents, function(eventNum) {
      event <- lapply(1:numTrials, function(trialNum) {
        selectedOutcomes <- input[[paste0("event", eventNum, "trial", trialNum)]]
        sapply(1:input$numOutcomes, function(outcome) {
          if (as.character(outcome) %in% selectedOutcomes) 1 else 0
        })
      })
      calcEventProb(event, probabilities)
    })

    output$result <- renderText({
      paste0("Event Probabilities: ", paste(round(eventProbs, 3), collapse = ", "))
    })
  })

  # Calculate and display the result of the selected operation
  observeEvent(input$performOperation, {
    numTrials <- input$numTrials
    probabilities <- sapply(1:input$numOutcomes, function(i) input[[paste0("prob", i)]])

    event1 <- eventsList(input$event1, numTrials)
    event1Prob <- calcEventProb(event1, probabilities)

    if (input$operation == "complement") {
      resultProb <- 1 - event1Prob
    } else {
      event2 <- eventsList(input$event2, numTrials)
      event2Prob <- calcEventProb(event2, probabilities)

      if (input$operation == "union") {
        intersectionEvent <- intersectEvents(event1, event2)
        intersectionProb <- calcEventProb(intersectionEvent, probabilities)
        resultProb <- event1Prob + event2Prob - intersectionProb
      } else if (input$operation == "intersection") {
        resultEvent <- intersectEvents(event1, event2)
        resultProb <- calcEventProb(resultEvent, probabilities)
      }
    }

    output$operationResult <- renderText({
      paste0("Resulting Event Probability: ", round(resultProb, 3))
    })
  })

  # Function to get the event list based on the event number
  eventsList <- function(eventNum, numTrials) {
    lapply(1:numTrials, function(trialNum) {
      selectedOutcomes <- input[[paste0("event", eventNum, "trial", trialNum)]]
      sapply(1:input$numOutcomes, function(outcome) {
        if (as.character(outcome) %in% selectedOutcomes) 1 else 0
      })
    })
  }

  # Function to calculate the probability of an event
  calcEventProb <- function(event, probabilities) {
    eventProb <- 1
    for (trial in event) {
      trialProb <- sum(trial * probabilities)
      eventProb <- eventProb * trialProb
    }
    return(eventProb)
  }

  # Function to perform intersection of two events
  intersectEvents <- function(event1, event2) {
    lapply(1:length(event1), function(trialNum) {
      pmin(event1[[trialNum]], event2[[trialNum]])
    })
  }
}

  shiny::shinyApp(ui = ui, server = server)
}
# run_discrete_app()

# shinyApp(ui = ui, server = server)
