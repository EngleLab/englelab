## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set import/output files
task <- "Antisaccade"
import_file <- paste(task, "_raw.csv", sep = "")
output_file <- paste(task, "_Scores.csv", sep = "")
##############

## Import Data
data_import <- read_csv(here(import_dir, import_file)) %>%
  filter(TrialProc == "real")

## Scores
data_scores <- data_import %>%
  group_by(Subject) %>%
  summarise(Antisaccade_ACC.mean = mean(Accuracy, na.rm = TRUE), 
            Antisaccade_RT.mean = mean(RT, na.rm = TRUE),
            AdminTime = first(AdminTime),
            SessionDate = first(SessionDate),
            SessionTime = first(SessionTime))

## Save Data
write_csv(data_scores, path = here(output_dir, output_file))

rm(list=ls())
