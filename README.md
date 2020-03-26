# englelab <img src = "man/figures/logo_small.png" align = "right" />

This package contains various functions for processing and scoring data from tasks downloaded from the <a href = "http://englelab.gatech.edu" target = "_blank"><b>EngleLab</b></a>

Most relevant to other researchers will be the functions available to process data from the **complex span tasks** and **attention control tasks**. The tasks are programmed in E-Prime, and unfortunately the resulting data file (**.edat**) the program produces is not easy to work with. The column names may not make any sense to you, there are way too many columns, the values in the columns are not always clear, and more messy issues. Even if you are not using R for data analysis you still may find this package useful.

Currently there are only functions for the **Advanced versions** of the complex-span tasks and functions for all the **Attention Control** tasks.

## Non-R Users

For those of you that do not use R for processing and analyzing data, we have developed a point-and-click GUI to process and score data files from the tasks. 

#### <a href = "https://englelab.shinyapps.io/taskscoring/" target = "_blank"><b>Web App for EngleLab Data Processing and Scoring</b></a>

----

## Install

```r
devtools::install_github("EngleLab/englelab")
```

## Data Preparation

Once you are ready to start analyzing data from the tasks, you will first need to create a merged (**.emrg**) data file of all the individual **.edat** files E-Prime produces. <a href = "https://www.youtube.com/watch?v=rQOg7ECK2Kw" target = "_blank">How to create a merged E-Prime file</a>

You then will need to export that **.emrg** file as a tab-delimited .txt file. <a href = "https://support.pstnet.com/hc/en-us/articles/115012298367-E-DATAAID-Exporting-Data-22832-" target = "_blank">How to Export E-Prime files to .txt</a>. You need to follow the 'Export Data to StatView' instructions AND **uncheck Unicode**.

You can then import the .txt merged file into R. `readr::read_delim()`

## Usage

Once you have the data file imported into R as a dataframe object, you can then use the functions to easily create clean and tidy raw data files for the tasks. Depending on your sample size, these functions may actually take a little while to execute.

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

**Antisaccade**: `raw_antisaccade()`

**Visual Arrays**: `raw_visualarrays()`

**SACT**: `raw_sact()`

**FlankerDL**: `raw_flankerDL()`

**StroopDL**: `raw_stroopDL()`

## Download R Scripts

Instead of creating your own R scripts from scratch you can download scripts to score each of the tasks. With these downloaded scripts you should only have to change the import/output directories and filenames.

```r
library(englelab)
get_script(type = "raw", to = "R Scripts",
           ospan = TRUE, symspan = TRUE, rotspan = TRUE,
           antisaccade = TRUE, va = TRUE, sact = TRUE,
           flankerDL = TRUE, stroopDL = TRUE)
```

## Troubleshooting

If you are having any difficulties using the functions or R scripts please contact Jason Tsukahara at jason.tsukahara@gatech.edu 
     
----

See [References](https://englelab.github.io/englelab/reference/index.html) for more information about the other functions in this package

## Citation

> Tsukahara, J.S. (2018). englelab: An R Package for Processing Data from Tasks Downloaded from the EngleLab. Retrieved from https://englelab.github.io/englelab/


