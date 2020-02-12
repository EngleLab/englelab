# englelab <img src = "man/figures/logo_small.png" align = "right" />

This package contains various functions for processing and scoring data common to the <a href = "http://englelab.gatech.edu" target = "_blank"><b>EngleLab</b></a>

Most relevant to other researchers will be the functions available to process data from the **complex span tasks**. The complex span tasks are programmed in E-Prime, and unfortunately the resulting data file (**.edat**) the program produces is not easy to work with. The column names may not make any sense to you, there are way too many columns, the values in the columns are not always clear, and more messy issues. Even if you are not using R for data analysis you still may find this package useful.

Currently there are only functions for the **Operation Span**, **Symmetry Span**, and **Rotation Span** tasks.

## Non-R Users

For those of you that do not use R for processing and analyzing data, we have developed a point-and-click GUI to process and score data files from the complex span tasks. 

#### <a href = "https://englelab.shinyapps.io/taskscoring/" target = "_blank"><b>Web App for Complex Span Data Processing and Scoring</b></a>

----

## Install

```r
devtools::install_github("EngleLab/englelab")
```

## Data Preparation

Once you are ready to start analyzing data from the complex span tasks, you will first need to create a merged (**.emrg**) data file of all the individual **.edat** files E-Prime produces. <a href = "https://www.youtube.com/watch?v=rQOg7ECK2Kw" target = "_blank">How to create a merged E-Prime file</a>

You then will need to export that **.emrg** file as a tab-delimited .txt file. <a href = "https://support.pstnet.com/hc/en-us/articles/115012298367-E-DATAAID-Exporting-Data-22832-" target = "_blank">How to Export E-Prime files to .txt</a>. You need to follow the 'Export Data to StatView' instructions AND **uncheck Unicode**.

You can then import the .txt merged file into R. `readr::read_delim()`

## Usage

Once you have the data file imported into R as a dataframe object, you can then use the functions to easily create clean and tidy raw data files for the complex span tasks. Depending on your sample size, these functions may actually take a little while to execute.

**Example with Operation Span**: `raw_ospan()`

```r
library(readr)
library(englelab)

## Import
import <- read_delim("import/OSpan.txt", "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
                     
## Raw
data <- raw_ospan(import, blocks = 2)

## Output
write_csv(data, "output/OSpan_raw.txt")
```

**Symmetry Span**: `raw_symspan()`

**Rotation Span**: `raw_rotspan()`

## Download R Scripts

Instead of creating your own R scripts from scratch you can download scripts to score each of the complex span tasks. With these downloaded scripts you should only have to change the import/output directories and filenames.

```r
library(englelab)
get_script(type = "raw", to = "R Scripts",
           ospan = TRUE, symspan = TRUE, rotspan = TRUE)
```


## Tidy File

The resulting dataframe will contain **both raw trial-level data and scored data**. The columns in the data are as follows

**Raw Trial-Level Columns**

- **Subject**:  Subject ID column

- **Block**:  Block number (1-3)

- **Trial**:  Within a block, the trial number. 

    Trial number refers to an entire set-size sequence (presentation of processing items, memory items, and recall screen)
    
- **SetSize**:  For the trial, the set-size (number of memory items)

- **SubTrial**:  Within a trial, there are processing items presented sequentially.

    Sub-Trial refers to this sequential presentation. It also represents the order of responses on the recall screen

- **SubTrialProc**: "Processing" or "Recall" portion of the task

- **RT**:  Reaction time

- **Accuracy**:  Accuracy (provided for both "Processing" and "Recall")

- **Response**:  The subject's response (provided for both "Processing" and "Recall")

- **CorrectResponse**:  The correct response (provided for both "Processing" and "Recall")

**Task Scores**

- **Processing.total**:  The total number of processing items correctly answered

- **Recall.total**:  The total number of recall items correctly answered

- **[Task].Absolute**: The task score using the Absolute Scoring method

    i.e. SymSpan.Absolute or OSpan.Absolute. 

- **[Task].Partial**:  The task score using the Partial Scoring method

- **[Task].Partial_Block1**:  The score on block 1 using the Partial Scoring method

- **[Task].Partial_Block2**:  The score on block 2 using the Partial Scoring method

- **[Task].Partial_Block3**:  The score on block 3 using the Partial Scoring method

- **[Task].[ProcessingTask]ACC**: Proportion of processing task items correctly answered

    i.e. SymSpan.SymmetryACC or OSpan.MathACC. 

- **[Task].Avg[ProcessingTask]Time**: The average time to complete a processing task item

     i.e. SymSpan.AvgSymmetryTime or OSpan.AvgMathTime.
     
----

See [References](https://englelab.github.io/englelab/reference/index.html) for more information about the other functions in this package

## Citation

> Tsukahara, J.S. (2018). englelab: An R Package for Processing Data Common to the EngleLab. Retrieved from https://englelab.github.io/englelab/


