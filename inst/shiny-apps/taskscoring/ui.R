library(shiny)
library(readr)
library(dplyr)
library(englelab)

fluidPage(

  titlePanel("Data Processing and Scoring: Complex Span and Attention
             Control Tasks"),

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

    numericInput("blocks",
                 "How many blocks were adminstered? (complex-span tasks only)",
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
    helpText("Once the tables are displayed, you can download the data to
             your computer"),
    downloadButton("downloadTrialLevel", "Trial Level"),
    downloadButton("downloadScores", "Task Scores")
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(
      tabPanel("Instructions",

      br(),
      p("This app allows you to easily create two types of data files from the
      Complex Span tasks. If, like me, you are scared of the file E-Prime produces
        for these tasks you will find this app useful."),
      br(),
      p(strong("1) A trial level data file:")),
      p("This is basically a cleaned up
      version of the extremely messy data file E-Prime produces. It can be
      useful for data storage, sharing, and calculating cronbach's alpha
      estimates in your data."),
      br(),
      p(strong("2) A task scores data file:")),
      p("This file has one row per subject, and columns with various scores
        calculated from the task."),

      hr(),

      h3(strong("Instructions")),
      p(strong("1. Choose a file to upload")),
      p("This file should be a merged E-Prime file (.emrg) exported as a .txt
      file from one of the Complex Span task. See this YouTube video on",
        a("how to create a merged E-Prime file",
          href = "https://www.youtube.com/watch?v=rQOg7ECK2Kw",
          target = "_blank")),
      p("To export the merged E-Prime file to a .txt file See these
        instructions on", a("How to Export E-Prime files to .txt",
        href = "https://support.pstnet.com/hc/en-us/articles/115012298367-E-DATAAID-Exporting-Data-22832-",
        target = "_blank"), ". You need to follow the 'Export Data to StatView' instructions AND",
        strong("uncheck Unicode")),
      p(strong("2. Select which complex span task the data corresponds to")),
      p("Right now only newly downloaded versions of the complex span
        tasks are supported. If you modified the task it is not guaranteed
        to work (it will depend on how you modified it)."),
      p(strong("3. Select how many blocks you administered")),
      p(strong("4. Click on Run")),
      p("Once you click on Run it may take a few minutes to process the data
        and display the tables. You can see a display of the two types of
        data files by selecting the Tabs above."),
      p(strong("5. Download the data files")),
      p("Click on the download buttons to download the data files. You will
        have to wait for the tables to be displayed before they are
        ready for download."),

      hr(),

      h3(strong("Description of Columns")),
      br(),
      h4(strong("Trial Level Data")),
      p(strong("Subject: "), "Subject ID column"),
      p(strong("Block"), "Block number (1-3)"),
      p(strong("Trial"), "Within a block, the trial number."),
      p("Trial number refers to an entire set-size sequence (presentation of
        processing items, memory items, and recall screen)"),
      p(strong("SetSize"), "For the trial, the set-size (number of memory items)"),
      p(strong("SubTrial"), "Within a trial, there are processing items
        presented sequentially"),
      p("Sub-Trial refers to this sequential presentation. It also represents
        the order of responses on the recall screen"),
      p(strong("SubTrialProc"), "Processing or Recall portion of the task"),
      p(strong("RT"), "Reaction time"),
      p(strong("Accuracy"), "Accuracy (provided for both Processing and Recall)"),
      p(strong("Response"), "The subject's response (provided for both
        Processing and Recall)"),
      p(strong("CorrectResponse"), "The correct response (provided for both
        Processing and Recall)"),

      br(),

      h4(strong("Task Scores Data")),
      p(strong("Processing.total"), "The total number of processing items
        correctly answered"),
      p(strong("Recall.total"), "The total number of recall items
        correctly answered"),
      p(strong("[Task].Absolute"), "The task score using the Absolute
        Scoring method"),
      p("i.e. SymSpan.Absolute or OSpan.Absolute."),
      p(strong("[Task].Partial"), "The task score using the Partial
        Scoring method"),
      p(strong("[Task]..Partial_Block1"), "The score on block 1 using the
        Partial Scoring method"),
      p(strong("[Task].Partial_Block2"), "The score on block 2 using the
        Partial Scoring method"),
      p(strong("[Task].[ProcessingTask].ACC"), "Proportion of processing task
        items correctly answered"),
      p("i.e. SymSpan.SymmetryACC or OSpan.MathACC."),
      p(strong("[Task].Avg[ProcessingTask].Time"), "The average time to
        complete a processing task item"),
      p("i.e. SymSpan.AvgSymmetryTime or OSpan.AvgMathTime.")

        ),
      tabPanel("Trial Level", tableOutput("trialLevel")),
      tabPanel("Task Scores", tableOutput("score"))
    )
    # Output: Data file ----

  )
)
