# englelab

This package contains various functions for processing data common to the [EngleLab](http://englelab.gatech.edu)

Most relevant to other researchers will be the functions available to process data from the complex span tasks. The complex span tasks are programmed in E-Prime, and unfortunately the resulting data file (**.edat**) the program produces is not easy to work with. The column names may not make any sense to you, there are way too many columns, the values in the columns are not always clear, and more messy issues. Even if you are not using R for data analysis you still may find the funtions useful.

Currently there are only functions for the **Operation Span**, **Symmetry Span**, and **Rotation Span** tasks. For each task there are two functions you may choose to use. One is for creating a clean and tidy raw data file. I suggest using this because you may want to go back to the raw data to compute internal consistency estimates. It is also easy to score the task from this clean and tidy raw data file. The other function will directly score the task but you will lose the raw data.

This package also contains functions to calculate binned scores (CITATION). There are also functions to easily calculate internal consistency estimates from trial-level raw data; both cronbach's alpha and split-half reliability

## Install

```r
devtools::install_github("EngleLab/englelab")
```

## Usage

Once you are ready to start analyzing data from the complex span tasks, you will first need to create a merged (**.emrg**) data file of all the individual **.edat** files E-Prime produces. You then will need to export that **.emrg** file as either a tab-delimited .txt file or a comma delimited .csv file. You can then import the .txt or .csv merged file into R. `readr::read_delim()` or `readr::read_csv()`

Once you have the data file imported into R as a dataframe object, you can then use the functions to easily create clean and tidy raw data files for the complex span tasks. Depending on your sample size, these functions may actually take a little while to execute.

----

### Operation Span

`raw_ospan()` will create a clean and tidy raw data file for the operation span task. The only arguments you need to specify are:

* __x__: The dataframe object

* __blocks__: How many blocks were administered? (1-3)

### Symmetry Span

`raw_symspan()` will create a clean and tidy raw data file for the symmetry span task. The only arguments you need to specify are:

* __x__: The dataframe object

* __blocks__: How many blocks were administered? (1-3)

### Rotation Span

`raw_rotspan()` will create a clean and tidy raw data file for the rotation span task. The only arguments you need to specify are:

* __x__: The dataframe object

* __blocks__: How many blocks were administered? (1-3)

----

You will then want to write the resulting dataframe to a file on your computer. `readr::write_csv()`

## Tidy File

The resulting dataframe will contain both raw trial-level data and scored data. The columns in the data are as follows

**Raw Trial-Level Columns**

- Subject:  Subject ID column

- Block:  Which block? (1-3)

- Trial:  Within a block, what trial number. 

    Trial number refers to an entire set-size sequence (presentation of processing items, memory items, and recall screen)
    
- SetSize:  For the trial, what is the set-size?

- SubTrial:  Within a trial, there are processing items presented sequentially. 

    Sub-Trial refers to this sequential presentation. It also represents the order of responses on the recall screen

- SubTrialProc:   "Processing" or "Recall" portion of the task?

- RT:  Reaction time

- Accuracy:  Accuracy (provided for both "Processing" and "Recall")

- Response:  The subject's response (provided for both "Processing" and "Recall")

- CorrectResponse:  The correct response (provided for both "Processing" and "Recall")

**Task Scores**

- Processing.total:  The total number of processing items correctly answered

- Recall.total:  The total number of recall items correctly answered

- [Task].Absolute: The task score using the Absolute Scoring method

    i.e. SymSpan.Absolute or OSpan.Absolute. 

- [Task].Partial:  The task score using the Partial Scoring method

- [Task].Partial_Block1:  The score on block 1 using the Partial Scoring method

- [Task].Partial_Block2:  The score on block 2 using the Partial Scoring method

- [Task].Partial_Block3:  The score on block 3 using the Partial Scoring method

- [Task].[Processing]ACC: Proportion of processing task items correctly answered

    i.e. SymSpan.SymmetryACC or OSpan.MathACC. 

- [Task].Avg[Processing]Time: The average time to complete a processing task item

     i.e. SymSpan.AvgSymmetryTime or OSpan.AvgMathTime.
     
----

See [References](https://englelab.github.io/englelab/reference/index.html) for more information about the other functions in this package

## Citation

> Tsukahara, J.S. (2018). englelab: An R Package for Processing Data Common to the EngleLab. Retrieved from https://englelab.github.io/englelab/


