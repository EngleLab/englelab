## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"

## Set import/output files
task <- "Antisaccade"
import.file <- paste(task, "_raw.csv", sep = "")
output.file <- paste(task, "_Scores.csv", sep = "")
##############

## Import Data
data_import <- read_csv(here(import.dir, import.file)) %>%
  filter(TrialProc=="real")

## Score
data_as <- data_import %>%
  group_by(Subject) %>%
  summarise(Antisaccade_ACC.mean = mean(Accuracy, na.rm = TRUE),
            Antisaccade_RT.mean = mean(RT, na.rm = TRUE))

## Output data
write_csv(data_as, path = here(output.dir, output.file))

rm(list=ls())
