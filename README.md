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

See [References](https://englelab.github.io/englelab/reference/index.html) for more information about the other functions in this package

## Citation

> Tsukahara, J.S. (2018). englelab: An R Package for Processing Data Common to the EngleLab. Retrieved from https://englelab.github.io/englelab/


