## Set up ####
## Load packages
library(readr)
library(here)
library(englelab)

## Set import/output directories
import_dir <- "Data Files/Merged"
output_dir <- "Data Files"

## Set import/output files
task <- "StroopDL"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "_raw.csv", sep = "")
##############

#### Import Data ####
data_import <- read_delim(here(import_dir, import_file), "\t",
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000)
#####################

#### Tidy raw data ####
data_raw <- raw_stroopDL(data_import)
#######################

#### Output ####
write_csv(data_raw, here(output_dir, output_file))
################

rm(list=ls())
