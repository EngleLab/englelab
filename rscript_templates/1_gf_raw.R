## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
directories <- readRDS(here::here("directories.rds"))
import.dir <- directories$emerge
output.dir <- directories$raw
##############

## RAPM ####
task <- "RAPM"
# Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

# Clean up raw data file and save
data_raw <- raw.rapm(data_import)
output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")
#############

################
################

## Letter Sets ####
task <- "LetterSets"
# Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

# Clean up raw data file and save
data_raw <- raw.lettersets(data_import)
output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")
###################

################
################

## Number Series ####
task <- "NumberSeries"
# Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

# Clean up raw data file and save
data_raw <- raw.numberseries(data_import)
output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")
#####################

################
################

rm(list=ls())