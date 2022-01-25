library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(englelab)

options(shiny.maxRequestSize=100*1024^2)

function(input, output) {

  data <- reactiveValues(trial = NULL, scores = NULL)
  message <- reactiveValues(status = "It may take a minute to finish running.")

  observeEvent(input$update, {
    import <- read_delim(input$file$datapath,
                         "\t", escape_double = FALSE, trim_ws = TRUE)

    if (ncol(import) == 1) {
      message$status <-
        "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
    } else {

      output$status <- renderText({
        message$status
      })

      if (input$task == "Operation Span") {
        data$trial <- raw_ospan(import)
        data$scores <- group_by(data$trial, Subject) %>%
          score_ospan()
      }

      if (input$task == "Symmetry Span") {
        data$trial <- raw_symspan(import)
        rm(import)
        data$scores <- group_by(data$trial, Subject) %>%
          score_symspan()
      }

      if (input$task == "Rotation Span") {
        data$trial <- raw_rotspan(import)
        data$scores <- group_by(data$trial, Subject)
        data$scores <- score_rotspan(data$scores)
      }

      if (input$task == "Antisaccade") {
        data$trial <- raw_antisaccade(import)
        data$scores <- filter(data$trial, TrialProc == "real") %>%
          group_by(Subject) %>%
          summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE),
                    Antisaccade.RT = mean(RT, na.rm = TRUE),
                    AdminTime = first(AdminTime),
                    SessionDate = first(SessionDate),
                    SessionTime = first(SessionTime))
      }

      if (input$task == "Visual Arrays") {
        data$trial <- raw_visualarrays(import)
        data$scores <- filter(data$trial, TrialProc == "real") %>%
          group_by(Subject) %>%
          score_visualarrays() %>%
          pivot_wider(id_cols = "Subject",
                      names_from = "SetSize",
                      names_prefix = "VAorient_S.k_",
                      values_from = "k") %>%
          mutate(VAorient_S.k = (VAorient_S.k_5 + VAorient_S.k_7) / 2) %>%
          select(Subject, VAorient_S.k, VAorient_S.k_5, VAorient_S.k_7)
      }

      if (input$task == "SACT") {
        data$trial <- raw_sact(import)
        data$scores <- filter(data$trial, TrialProc == "real") %>%
          group_by(Subject) %>%
          summarise(SACT.acc = mean(Accuracy, na.rm = TRUE),
                    AdminTime = first(AdminTime),
                    SessionDate = first(SessionDate),
                    SessionTime = first(SessionTime))
      }

      if (input$task == "FlankerDL") {
        data$trial <- raw_flankerDL(import)
        data$scores <- select(data$trial, Subject, FlankerDLScore, AdminTime)
        data$scores <- distinct(data$scores)
      }

      if (input$task == "StroopDL") {
        data$trial <- raw_stroopDL(import)
        data$scores <- select(data$trial, Subject, StroopDLScore, AdminTime)
        data$scores <- distinct(data$scores)
      }

      if (!is.null(data$trial)) {
        message$status <- "Done! Click on the 'Trial Level' and 'Task Scores' tabs to view the data. Tables may take a minute to display"
      }
    }
  })

  output$status <- renderText({
    message$status
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
