


#' Run Descriptive Analysis Shiny App
#'
#' This function launches the Shiny app for descriptive analysis of datasets.
#' The app allows users to upload Excel files, select datasets (including built-in datasets like `iris` and `palmerpenguins`),
#' and create various plots such as histograms, boxplots, and scatter plots.
#'
#' @return The Shiny app is launched in the default web browser.
#' @import ggplot2
#' @import shiny
#' @import palmerpenguins
#' @import DT
#' @import readxl
#' @examples
#' \dontrun{
#' run_descriptive_analysis_app() # Run the app
#' }
#' @export
run_descriptive_analysis_app <- function(){

  # Load necessary libraries with explicit namespaces
  library(shiny)
  library(palmerpenguins)
  library(ggplot2)
  library(DT)
  library(readxl)


# Define the User Interface (UI) of the Shiny App

ui <- fluidPage(
  titlePanel("Dataset Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File:", accept = c(".xlsx")),
      actionButton("addFile", "Add Another Excel File"),
      actionButton("removeSheet", "Remove Current Sheet"),
      radioButtons("plotType", "Plot Type:", choices = c("Histogram", "Boxplot", "Scatter Plot")),
      uiOutput("datasetSelect"),
      uiOutput("varselect"),
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        sliderInput("numbins", "Number of Bins:", min = 5, max = 50, value = 30)
      ),
      conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        uiOutput("scattervarselect")  # Second variable for scatter plot
      ),
      uiOutput("fillselect"),
    ),
    mainPanel(
      plotOutput("plot"),
      conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        verbatimTextOutput("correlationOutput")  # Display the correlation below the graph
      ),
      DTOutput("table")
    )
  )
)

# Define the Server logic of the Shiny App
server <- function(input, output, session) {
  sheets <- reactiveVal(c("iris", "palmerpenguins"))

  observeEvent(input$file, {
    new_sheets <- excel_sheets(input$file$datapath)
    sheets(c(sheets(), new_sheets))
    updateSelectInput(session, "dataset", choices = sheets(), selected = sheets()[1])
  })

  observeEvent(input$addFile, {
    updateActionButton(session, "file", "Upload Excel File:")
  })

  observeEvent(input$removeSheet, {
    selected_sheet <- input$dataset
    updated_sheets <- setdiff(sheets(), selected_sheet)
    sheets(updated_sheets)
    if (length(updated_sheets) > 0) {
      updateSelectInput(session, "dataset", choices = updated_sheets, selected = updated_sheets[1])
    }
  })
  # Display correlation coefficient separately below the graph
  output$correlationOutput <- renderPrint({
    dataset <- datasetInput()
    var <- input$variable
    scatterVar <- input$scattervar

    if (input$plotType == "Scatter Plot" && !is.null(var) && !is.null(scatterVar)) {
      correlation <- cor(dataset[[var]], dataset[[scatterVar]], use = "complete.obs")
      paste("Correlation Coefficient between", var, "and", scatterVar, ":", round(correlation, 2))
    }
  })

  datasetInput <- reactive({
    if (!is.null(input$file) && input$dataset %in% excel_sheets(input$file$datapath)) {
      read_excel(input$file$datapath, sheet = input$dataset)
    } else if (input$dataset == "iris") {
      iris
    } else {
      na.omit(penguins)
    }
  })

  output$datasetSelect <- renderUI({
    selectInput("dataset", "Select Dataset:", choices = sheets())
  })

  output$varselect <- renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      numericVars <- names(dataset)[sapply(dataset, is.numeric)]
      selectInput("variable", "Select Variable:", choices = numericVars)
    }
  })

  output$scattervarselect <- renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      numericVars <- names(dataset)[sapply(dataset, is.numeric)]
      selectInput("scattervar", "Select Variable for Y-axis:", choices = numericVars)
    }
  })

  output$fillselect <- renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      fillVars <- names(dataset)[!sapply(dataset, is.numeric)]
      fillChoices <- c("None", fillVars)
      selectInput("fillvar", "Select Fill Variable:", choices = fillChoices)
    }
  })

  output$plot <- renderPlot({
    dataset <- datasetInput()
    var <- input$variable
    plotType <- input$plotType
    fillVar <- input$fillvar

    if (!is.null(var) && !is.null(dataset)) {
      if (plotType == "Histogram") {
        if (fillVar == "None") {
          p <- ggplot(dataset, aes_string(x = var))
        } else {
          p <- ggplot(dataset, aes_string(x = var, fill = fillVar))
        }
        numbins <- input$numbins
        binwidth <- (max(dataset[[var]], na.rm = TRUE) - min(dataset[[var]], na.rm = TRUE)) / numbins
        p <- p + geom_histogram(binwidth = binwidth, na.rm = TRUE, position = "stack") + labs(title = paste("Histogram of", var))
      } else if (plotType == "Boxplot") {
        if (fillVar == "None") {
          p <- ggplot(dataset, aes_string(x = var))
        } else {
          p <- ggplot(dataset, aes_string(x = var, fill = fillVar))
        }
        p <- p + geom_boxplot() + labs(title = paste("Boxplot of", var))
      } else if (plotType == "Scatter Plot") {
        scatterVar <- input$scattervar
        if (!is.null(scatterVar)) {
          if (fillVar == "None") {
            p <- ggplot(dataset, aes_string(x = var, y = scatterVar))
          } else {
            p <- ggplot(dataset, aes_string(x = var, y = scatterVar, color = fillVar))
          }
          p <- p + geom_point() + labs(title = paste("Scatter Plot of", var, "vs", scatterVar))
          # Calculate correlation
          correlation <- cor(dataset[[var]], dataset[[scatterVar]], use = "complete.obs")
          p <- p + annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(correlation, 2)), hjust = 1.1, vjust = 1.1, size = 5, color = "blue")
        }
      }
      print(p)
    }
  })

  output$table <- renderDT({
    dataset <- datasetInput()
    datatable(dataset)
  })
}

  shinyApp(ui = ui, server = server)
}
# run_descriptive_analysis_app()
