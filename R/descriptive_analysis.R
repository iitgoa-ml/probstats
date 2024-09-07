# Load necessary libraries with explicit namespaces
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(DT)
library(readxl)


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
#' if (interactive()) {
#'   run_descriptive_analysis_app()
#' }
#' @export
run_descriptive_analysis_app <- function(){


# Define the User Interface (UI) of the Shiny App

ui <- shiny::fluidPage(
  shiny::titlePanel("Dataset Viewer"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("file", "Upload Excel File:", accept = c(".xlsx")),
      shiny::actionButton("addFile", "Add Another Excel File"),
      shiny::actionButton("removeSheet", "Remove Current Sheet"),
      shiny::radioButtons("plotType", "Plot Type:", choices = c("Histogram", "Boxplot", "Scatter Plot")),
      shiny::uiOutput("datasetSelect"),
      shiny::uiOutput("varselect"),
      shiny::conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        shiny::sliderInput("numbins", "Number of Bins:", min = 5, max = 50, value = 30)
      ),
      shiny::conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        shiny::uiOutput("scattervarselect")  # Second variable for scatter plot
      ),
      shiny::uiOutput("fillselect"),
    ),
    shiny::mainPanel(
      shiny::plotOutput("plot"),
      shiny::conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        shiny::verbatimTextOutput("correlationOutput")  # Display the correlation below the graph
      ),
      DT::DTOutput("table")
    )
  )
)

# Define the Server logic of the Shiny App
server <- function(input, output, session) {
  sheets <- shiny::reactiveVal(c("iris", "palmerpenguins"))

  shiny::observeEvent(input$file, {
    new_sheets <- readxl::excel_sheets(input$file$datapath)
    sheets(c(sheets(), new_sheets))
    shiny::updateSelectInput(session, "dataset", choices = sheets(), selected = sheets()[1])
  })

  shiny::observeEvent(input$addFile, {
    shiny::updateActionButton(session, "file", "Upload Excel File:")
  })

  shiny::observeEvent(input$removeSheet, {
    selected_sheet <- input$dataset
    updated_sheets <- setdiff(sheets(), selected_sheet)
    sheets(updated_sheets)
    if (length(updated_sheets) > 0) {
      shiny::updateSelectInput(session, "dataset", choices = updated_sheets, selected = updated_sheets[1])
    }
  })
  # Display correlation coefficient separately below the graph
  output$correlationOutput <- shiny::renderPrint({
    dataset <- datasetInput()
    var <- input$variable
    scatterVar <- input$scattervar

    if (input$plotType == "Scatter Plot" && !is.null(var) && !is.null(scatterVar)) {
      correlation <- cor(dataset[[var]], dataset[[scatterVar]], use = "complete.obs")
      paste("Correlation Coefficient between", var, "and", scatterVar, ":", round(correlation, 2))
    }
  })

  datasetInput <- shiny::reactive({
    if (!is.null(input$file) && input$dataset %in% readxl::excel_sheets(input$file$datapath)) {
      readxl::read_excel(input$file$datapath, sheet = input$dataset)
    } else if (input$dataset == "iris") {
      datasets::iris
    } else {
      na.omit(palmerpenguins::penguins)
    }
  })

  output$datasetSelect <- shiny::renderUI({
    shiny::selectInput("dataset", "Select Dataset:", choices = sheets())
  })

  output$varselect <- shiny::renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      numericVars <- names(dataset)[sapply(dataset, is.numeric)]
      shiny::selectInput("variable", "Select Variable:", choices = numericVars)
    }
  })

  output$scattervarselect <- shiny::renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      numericVars <- names(dataset)[sapply(dataset, is.numeric)]
      shiny::selectInput("scattervar", "Select Variable for Y-axis:", choices = numericVars)
    }
  })

  output$fillselect <- shiny::renderUI({
    dataset <- datasetInput()
    if (!is.null(dataset)) {
      fillVars <- names(dataset)[!sapply(dataset, is.numeric)]
      fillChoices <- c("None", fillVars)
      shiny::selectInput("fillvar", "Select Fill Variable:", choices = fillChoices)
    }
  })

  output$plot <- shiny::renderPlot({
    dataset <- datasetInput()
    var <- input$variable
    plotType <- input$plotType
    fillVar <- input$fillvar

    if (!is.null(var) && !is.null(dataset)) {
      if (plotType == "Histogram") {
        if (fillVar == "None") {
          p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var))
        } else {
          p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, fill = fillVar))
        }
        numbins <- input$numbins
        binwidth <- (max(dataset[[var]], na.rm = TRUE) - min(dataset[[var]], na.rm = TRUE)) / numbins
        p <- p + ggplot2::geom_histogram(binwidth = binwidth, na.rm = TRUE, position = "stack") + ggplot2::labs(title = paste("Histogram of", var))
      } else if (plotType == "Boxplot") {
        if (fillVar == "None") {
          p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var))
        } else {
          p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, fill = fillVar))
        }
        p <- p + ggplot2::geom_boxplot() + ggplot2::labs(title = paste("Boxplot of", var))
      } else if (plotType == "Scatter Plot") {
        scatterVar <- input$scattervar
        if (!is.null(scatterVar)) {
          if (fillVar == "None") {
            p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, y = scatterVar))
          } else {
            p <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, y = scatterVar, color = fillVar))
          }
          p <- p + ggplot2::geom_point() + ggplot2::labs(title = paste("Scatter Plot of", var, "vs", scatterVar))
          # Calculate correlation
          correlation <- cor(dataset[[var]], dataset[[scatterVar]], use = "complete.obs")
          p <- p + ggplot2::annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(correlation, 2)), hjust = 1.1, vjust = 1.1, size = 5, color = "blue")
        }
      }
      print(p)
    }
  })

  output$table <- DT::renderDT({
    dataset <- datasetInput()
    DT::datatable(dataset)
  })
}

  shiny::shinyApp(ui = ui, server = server)
}
# run_descriptive_analysis_app()
