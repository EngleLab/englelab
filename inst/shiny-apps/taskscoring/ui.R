library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(englelab)

fluidPage(

  titlePanel("EngleLab WebApp"),

  sidebarPanel(

    # Input: Select a file ----
    fileInput("file", "Choose File",
              multiple = FALSE,
              accept = c("text/txt",
                         "text/tab-delimited,text/plain",
                         ".txt")),

    selectInput("task", "Which Task?",
                choices = c("Operation Span", "Symmetry Span", "Rotation Span",
                            "Antisaccade", "Visual Arrays", "SACT",
                            "FlankerDL", "StroopDL", ""),
                selected = ""),

    # Input: actionButton() to defer the rendering of output ----
    # until the user explicitly clicks the button (rather than
    # doing it immediately when inputs change). This is useful if
    # the computations required to render output are inordinately
    # time-consuming.
    hr(),
    hr(),
    actionButton("update", "Run"),
    helpText("Tables may take a few minutes to display after clicking Run"),

    hr(),
    hr(),
    helpText("Once the tables are displayed (click ont the 'Trial Level' and
             'Task Scores' tabs to view),
             you can download the data to your computer"),
    downloadButton("downloadTrialLevel", "Trial Level"),
    downloadButton("downloadScores", "Task Scores")
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(
      tabPanel("Instructions",

               br(),
               p("Warning: This WebApp is still under development.
        Contact jason.tsukahara@gatech.edu for any issues"),
        br(),
        p("This app allows you to easily create two types of data files:"),
        br(),
        p(strong("1) A trial level data file:")),
        p("This is basically a cleaned up
      version of the extremely messy data file E-Prime produces. It can be
      useful for data storage, sharing, and calculating reliability
      estimates in your data."),
      br(),
      p(strong("2) A task scores data file:")),
      p("This file has one row per subject, and columns with various scores
        calculated from the task."),

      hr(),

      h3(strong("Instructions")),
      p(strong("1. Choose a file to upload")),
      p("This file should be a merged E-Prime file (.emrg) exported as a .txt
      file. See this YouTube video on",
      a("how to create a merged E-Prime file",
        href = "https://www.youtube.com/watch?v=rQOg7ECK2Kw",
        target = "_blank")),
      p("To export the merged E-Prime file to a .txt file See these
        instructions on", a("How to Export E-Prime files to .txt",
                            href = "https://support.pstnet.com/hc/en-us/articles/115012298367-E-DATAAID-Exporting-Data-22832-",
                            target = "_blank"), ". You need to follow the 'Export Data to StatView' instructions AND",
        strong("uncheck Unicode")),
      p(strong("2. Select which task the data corresponds to")),
      p("Right now only newly downloaded versions of the complex span
        tasks are supported. If you modified the task it is not guaranteed
        to work (it will depend on how you modified it)."),
      p(strong("3. Click on Run")),
      p("Once you click on Run it may take a few minutes to process the data
        and display the tables. You can see a display of the two types of
        data files by selecting the Tabs above."),
      p(strong("4. Download the data files")),
      p("Click on the download buttons to download the data files. You will
        have to wait for the tables to be displayed before they are
        ready for download."),

      hr(),
      hr(),

      p("Visit ", a("https://englelab.github.io/englelab/",
                    href = "https://englelab.github.io/englelab/",
                    target = "_blank"), " for more information"),

      ),

      tabPanel("Trial Level", tableOutput("trialLevel")),
      tabPanel("Task Scores", tableOutput("score"))
    )
    # Output: Data file ----

  )
)
