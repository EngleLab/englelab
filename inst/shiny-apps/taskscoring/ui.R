library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(englelab)

fluidPage(
  tags$head(tags$script(HTML(
    "var _paq = window._paq = window._paq || [];
    _paq.push(['trackPageView']);
    _paq.push(['enableLinkTracking']);
    (function() {
      var u='//englelab.gatech.edu/matomo/';
      _paq.push(['setTrackerUrl', u+'matomo.php']);
      _paq.push(['setSiteId', '2']);
      var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
      g.type='text/javascript'; g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
    })();"
  ))),

  titlePanel("EngleLab WebApp"),

  sidebarPanel(

    # Input: Select a file ----
    fileInput("file", "Choose File",
              multiple = FALSE,
              accept = c("text/txt",
                         "text/tab-delimited,text/plain",
                         ".txt")),

    selectInput("task", "Which Task?",
                choices = c("Operation Span", "Symmetry Span",
                            "Rotation Span", "Reading Span",
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
    helpText(textOutput("status")),

    hr(),
    hr(),
    helpText("Download the data files to your computer"),
    downloadButton("downloadTrialLevel", "Trial Level"),
    downloadButton("downloadScores", "Task Scores"),
    hr(),
    hr(),
    helpText("If the app 'disconnects from server' then refresh the
             page and try again."),
    helpText("You can also try running the app in R (see instructions below)."),
    helpText("If you cannot get the app to work contact:
             Jason Tsukahara (jason.tsukahara@gatech.edu) for support."),
    hr(),
    hr(),
    helpText("If you use this WebApp, then please cite the englelab
              R package in publications."),
    helpText("Tsukahara, Jason S. (2022). englelab: An R package for processing
             complex-span and attention control tasks downloaded from the
             EngleLab (1.1.0). Zenodo. https://doi.org/10.5281/zenodo.6987145"),
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
      p(a("Description of columns for Complex-Span Tasks",
          href = "https://englelab.github.io/englelab/articles/Complex_Span.html#columns-outputted-by-raw_-functions",
          target = "_blank")),
      br(),
      p(strong("2) A task scores data file:")),
      p("This file has one row per subject, and columns with various scores
        calculated from the task."),
      p(a("Description of columns for Complex-Span Tasks",
          href = "https://englelab.github.io/englelab/articles/Complex_Span.html#columns-outputted-by-score_-functions",
          target = "_blank")),

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
                            target = "_blank"),
        ". You need to follow the 'Export Data to StatView' instructions AND",
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

      h3(strong("Run the app in R")),
      p("If you are having difficulty getting the WebApp to run in a broswer,
        then you can also run the app in R. This method does not require an
        internet connection."),
      p(strong("1. Install R and/or RStudio")),
      p("If you do not have R or RStudio installed, then you will first need
        to install these programs."),
      p(a("Install R (select the 0-Cloud option)",
          href = "https://cran.r-project.org/mirrors.html",
          target = "_blank")),
      p(a("Install RStudio",
          href = "https://www.rstudio.com/products/rstudio/download/#download",
          target = "_blank")),
      br(),
      p(strong("2. Install the englelab R package")),
      p("In the R or RStudio console window: "),
      code("install.packages(\"devtools\")"),
      br(),
      code("install.packages(\"shiny\")"),
      br(),
      code("devtools::install_github(\"EngleLab/englelab\")"),
      br(),
      br(),
      p("Restart R or RStudio after installing the englelab R Package"),
      p(strong("3. Run the app")),
      code("englelab::runApp()"),

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
