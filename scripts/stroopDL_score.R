## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"
removed.dir <- "Data Files/Scored Data/removed"

## Set import/output files
task <- "StroopDL"
import.file <- paste(task, "_trial_raw.csv", sep = "")
output.file <- paste(task, "_Scores.csv", sep = "")
removed.file <- paste(task, "_removed.csv", sep = "")

## Set Trimming criteria
acc.criteria <- -3.5
##############

## Import Data
data_import <- read_csv(here(import.dir, import.file))

data_stroopDL <- data_import %>%
  filter(TrialProc == "real") %>%
  select(Subject, StroopDL.score = StroopDLTime, StroopMissedDeadlines,
         StroopDLTotalAccuracy, StroopDLCorrectRT, StroopDLTotalRT) %>%
  distinct()

data_remove <- data_import %>%
  group_by(Subject) %>%
  summarise(ACC.mean = mean(TrialCriteria.Acc, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardize = TRUE) %>%
  filter(ACC.mean_z < acc.criteria)

data_stroopDL <- remove_save(data_stroopDL, data_remove,
                             output.dir = removed.dir,
                             output.file = removed.file)

## Output Data
write_csv(data_stroopDL, path = here(output.dir, output.file))


rm(list=ls())

