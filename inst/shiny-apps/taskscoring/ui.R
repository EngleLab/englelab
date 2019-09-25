library(shiny)
library(readr)
library(dplyr)
library(englelab)

fluidPage(

  titlePanel("Complex Span Task - Data Processing and Scoring"),

  sidebarPanel(

    # Input: Select a file ----
    fileInput("file", "Choose text File",
              multiple = FALSE,
              accept = c("text/txt",
                         "text/tab-delimited,text/plain",
                         ".txt")),

    selectInput("task", "Which Complex-Span Task?",
                choices = c("Operation Span", "Symmetry Span", "Rotation Span",
                            ""),
                selected = ""),

    numericInput("blocks", "How many blocks were adminstered?",
                 2, min = 1, max = 3),

    # Input: actionButton() to defer the rendering of output ----
    # until the user explicitly clicks the button (rather than
    # doing it immediately when inputs change). This is useful if
    # the computations required to render output are inordinately
    # time-consuming.
    actionButton("update", "Run"),
    helpText("Tables may take a few minutes to display after clicking Run"),

    hr(),
    hr(),
    helpText("Once the tables are displayed, you can download the data to your computer"),
    downloadButton("downloadTrialLevel", "Trial Level"), downloadButton("downloadScores", "Task Scores")
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(
      tabPanel("Trial Level", tableOutput("trialLevel")),
      tabPanel("Task Scores", tableOutput("score"))
    )
    # Output: Data file ----

  )
)
