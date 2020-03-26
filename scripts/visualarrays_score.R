## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)
library(datawrangling)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"
removed_dir <- "Data Files/Scored Data/removed"

## Set import/output files
task <- "VisualArrays_4"
import_file <- paste(task, "_raw.csv", sep = "")
output_file <- paste(task, "_Scores.csv", sep = "")
removed_file <- paste(task, "_removed.csv", sep = "")

## Set Trimming criteria
acc_criteria <- -3.5
##############

## Import Data
data_import <- read_csv(here(import_dir, import_file)) %>%
  filter(TrialProc == "real")

## Scores
data_scores <- data_import %>%
  select(Subject, contains("VA"), contains("Time"), contains("Date")) %>%
  distinct()

## Trim outliers
data_remove <- data_import %>%
  group_by(Subject) %>%
  summarise(ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardize = TRUE) %>%
  filter(ACC.mean < acc_criteria)

data_scores <- remove_save(data_scores, data_remove,
                           output.dir = here(removed_dir),
                           output.file = removed_file)

## Save Data
write_csv(data_scores, path = here(output_dir, output_file))

rm(list=ls())

