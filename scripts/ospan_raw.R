## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
import.dir <- "Data Files/Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "OSpan"
import.file <- paste(task, ".txt", sep = "")
output.file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE)

## Clean up raw data and save
data_raw <- raw_ospan(data_import, blocks = 2)

## Output Data
write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())
