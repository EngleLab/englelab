## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set import/output files
task <- "FlankerDL"
import_file <- paste(task, "_raw.csv", sep = "")
output_file <- paste(task, "_Scores.csv", sep = "")
##############

#### Import Data ####
data_import <- read_csv(here(import_dir, import_file)) %>%
  filter(TrialProc == "real")
#####################

#### Score Data ####
data_scores <- data_import %>%
  select(Subject, contains(task), contains("Time"), contains("Date")) %>%
  distinct()
####################

#### Clean Data ####
# remove problematic subjects based on some criteria
# remove outliers based on accuracy
####################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
