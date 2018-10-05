## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "FlankerPR"

# Set RT trimming variables. How many SDs above and below the mean?
sd.criteria <- 3.5

## Import Data
import.file <- paste(task, "trial_raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(TrialProc=="real")

data_flankerPR <- data_import %>%
  select(Subject, FlankerPR.score = FlankerPRTime, FlankerPRTotalAccuracy, FlankerPRCorrectRT, FlankerPRTotalRT) %>%
  distinct()

data_remove <- data_import %>%
  group_by(Subject) %>%
  summarise(ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardized = TRUE) %>%
  filter(ACC.mean_z > sd.criteria | ACC.mean_z < (-1*sd.criteria))

data_flankerPR <- remove.save(data_flankerPR, remove = data_remove, save = here(output.dir, "remove"), taskname = "FlankerPR")

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_flankerPR, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())
