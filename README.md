# englelab <img src="man/figures/logo_small.png" align="right"/>

This package contains various functions for processing and scoring complex-span and attention control tasks downloaded from the <a href = "http://englelab.gatech.edu" target = "_blank"><b>EngleLab</b></a>

The tasks are programmed in E-Prime and the resulting data file (**.edat**) the program produces is not easy to work with. The column names may not make any sense to you, there are way too many columns, the values in the columns are not always clear, and other messy issues.

Currently there are functions for the **Advanced versions** and **Foster Shortened versions** of the complex-span tasks and functions for all the **Attention Control** tasks.

------------------------------------------------------------------------

## Install

``` r
devtools::install_github("EngleLab/englelab")
```

## Data Preparation

Once you are ready to start analyzing data from the tasks, you will first need to create a merged (**.emrg**) data file of all the individual **.edat** files E-Prime produces. <a href = "https://www.youtube.com/watch?v=rQOg7ECK2Kw" target = "_blank">How to create a merged E-Prime file</a>

You then will need to export that **.emrg** file as a tab-delimited .txt file. <a href = "https://support.pstnet.com/hc/en-us/articles/115012298367-E-DATAAID-Exporting-Data-22832-" target = "_blank">How to Export E-Prime files to .txt</a>. You need to follow the 'Export Data to StatView' instructions AND **uncheck Unicode**.

You can then import the .txt merged file into R. `readr::read_delim()`

## Usage

**Symmetry Span Example**

``` r
library(readr)
library(dplyr)
library(englelab)

## Import
data_import <- read_delim("import/SymSpan.txt", "\t", 
                          escape_double = FALSE, trim_ws = TRUE)
                     
## Raw
data_raw <- raw_symspan(data_import)

## Score
data_score <- data_raw %>%
  group_by(Subject) %>%
  score_symspan()
```

**See Articles:**

-   [Complex Span Tasks](https://englelab.github.io/englelab/articles/Complex_Span.html)

-   [Attention Control Tasks](https://englelab.github.io/englelab/articles/Attention_Control.html)

-   [Download R Scripts](https://englelab.github.io/englelab/articles/Download_Scripts.html)

## Non-R Users

For non-R users, we created a point-and-click GUI to process and score data files from the tasks.

#### <a href = "https://englelab.shinyapps.io/taskscoring/" target = "_blank"><b>Web App for englelab</b></a>

## Troubleshooting

If you are having any difficulties with this package please contact Jason Tsukahara at [jason.tsukahara\@gatech.edu](mailto:jason.tsukahara@gatech.edu)

------------------------------------------------------------------------

## Citation

[![DOI](https://zenodo.org/badge/151780396.svg)](https://zenodo.org/badge/latestdoi/151780396)

> Tsukahara, J.S. (2021). englelab: An R package for processing complex-span and attention control tasks downloaded from the EngleLab (v1.0.1). <http://doi.org/10.5281/zenodo.4757606>
