## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"

## Set import/output files
task <- "SACT"
import.file <- paste(task, "_raw.csv", sep = "")
output.file <- paste(task, "_Scores.csv", sep = "")
##############

## Import Data
data_import <- read_csv(here(import.dir, import.file))

## Score
data_sact <- data_import %>%
  filter(TrialProc == "real") %>%
  group_by(Subject) %>%
  summarise(SACT_ACC.mean = mean(Accuracy, na.rm = TRUE))

## Output Data
write_csv(data_sact, path = here(output.dir, output.file))

rm(list=ls())

