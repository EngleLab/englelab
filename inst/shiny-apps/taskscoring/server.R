library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(englelab)

options(shiny.maxRequestSize = 100*1024^2)

function(input, output) {

  data <- reactiveValues(trial = NULL, scores = NULL)
  message <- reactiveValues(status = "It may take a minute to finish running.")

  observeEvent(input$update, {
    import <- tryCatch(read_delim(input$file$datapath,
                         "\t", escape_double = FALSE, trim_ws = TRUE,
                         locale =
                           locale(encoding =
                                    guess_encoding(input$file$datapath)$encoding[1])),
                       error = function(e) {"error"})

    if (!is.data.frame(import)) {
      import <- tryCatch(read_delim(input$file$datapath,
                                    locale = locale(encoding = "UCS-2LE"),
                                    delim = "\t", escape_double = FALSE,
                                    trim_ws = TRUE, na = "NULL"),
                         error = function(e) {"error"})
    }

    if (!is.data.frame(import)) {
      message$status <-
        "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"

      data$trial <- data.frame()
      data$scores <- data.frame()
    } else {

      output$status <- renderText({
        message$status
      })

      if (input$task == "Operation Span") {
        data$trial <- tryCatch(raw_ospan(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- group_by(data$trial, Subject) %>%
            score_ospan()
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "Symmetry Span") {
        data$trial <- tryCatch(raw_symspan(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- group_by(data$trial, Subject) %>%
            score_symspan()
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "Rotation Span") {
        data$trial <- tryCatch(raw_rotspan(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- group_by(data$trial, Subject)
          data$scores <- score_rotspan(data$scores)
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "Reading Span") {
        data$trial <- tryCatch(raw_readspan(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- group_by(data$trial, Subject)
          data$scores <- score_readspan(data$scores)
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "Antisaccade") {
        data$trial <- tryCatch(raw_antisaccade(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- filter(data$trial, TrialProc == "real") %>%
            group_by(Subject) %>%
            summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE),
                      Antisaccade.RT = mean(RT, na.rm = TRUE),
                      AdminTime = first(AdminTime),
                      SessionDate = first(SessionDate),
                      SessionTime = first(SessionTime))
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "Visual Arrays") {
        data$trial <- tryCatch(raw_visualarrays(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- filter(data$trial, TrialProc == "real") %>%
            group_by(Subject, SetSize) %>%
            score_visualarrays(taskname = "VAorient_S")
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "SACT") {
        data$trial <- tryCatch(raw_sact(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- filter(data$trial, TrialProc == "real") %>%
            group_by(Subject) %>%
            summarise(SACT.acc = mean(Accuracy, na.rm = TRUE),
                      AdminTime = first(AdminTime),
                      SessionDate = first(SessionDate),
                      SessionTime = first(SessionTime))
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "FlankerDL") {
        data$trial <- tryCatch(raw_flankerDL(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- select(data$trial, Subject, FlankerDLScore, AdminTime)
          data$scores <- distinct(data$scores)
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (input$task == "StroopDL") {
        data$trial <- tryCatch(raw_stroopDL(import),
                               error = function(e) {"error"})
        if (is.data.frame(data$trial)) {
          data$scores <- group_by(data$trial, Subject)
          data$scores <- tryCatch(score_stroopDL(data$scores),
                                  error = function(e) {data.frame()})
        } else {
          message$status <-
            "Error: Make sure you exported the E-Prime file using the StatView and SPSS option (see Instructions)"
          data$trial <- data.frame()
          data$scores <- data.frame()
        }

      }

      if (nrow(data$trial) > 0) {
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
