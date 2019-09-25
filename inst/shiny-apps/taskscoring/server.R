library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(englelab)

options(shiny.maxRequestSize=100*1024^2)

function(input, output) {

  data <- reactiveValues(trial = NULL, scores = NULL)

  observeEvent(input$update, {
    import <- read_delim(input$file$datapath, "\t",
                         escape_double = FALSE, trim_ws = TRUE)

    if (input$task == "Operation Span") {
      data$trial <- raw_ospan(import, blocks = input$blocks)
      data$scores <- select(data$trial, Subject, OSpan.Absolute:SessionTime)
      data$scores <- distinct(data$scores)
    }

    if (input$task == "Symmetry Span") {
      data$trial <- raw_symspan(import, blocks = input$blocks)
      data$scores <- select(data$trial, Subject, SymSpan.Absolute:SessionTime)
      data$scores <- distinct(data$scores)
    }

    if (input$task == "Rotation Span") {
      data$trial <- raw_rotspan(import, blocks = input$blocks)
      data$scores <- select(data$trial, Subject, RotSpan.Absolute:SessionTime)
      data$scores <- distinct(data$scores)
    }
  })

  output$trialLevel <- renderTable({
    data$trial
  })

  output$score <- renderTable({
    data$scores
  })

  # Downloadable csv of selected dataset ----
  output$downloadTrialLevel <- downloadHandler(
    filename = function() {
      str_replace(input$file, ".txt", "_raw.csv")
    },
    content = function(file) {
      write_csv(data$trial, file)
    }
  )

  # Downloadable csv of selected dataset ----
  output$downloadScores <- downloadHandler(
    filename = function() {
      str_replace(input$file, ".txt", "_Scores.csv")
    },
    content = function(file) {
      write_csv(data$scores, file)
    }
  )
}
