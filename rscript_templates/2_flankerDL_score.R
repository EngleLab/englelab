## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "FlankerDL"

# Set RT trimming variables. How many SDs above and below the mean?
sd.criteria <- 3.5

## Import Data
import.file <- paste(task, "trial_raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(TrialProc=="real")

data_flankerDL <- data_import %>%
  select(Subject, FlankerDL.score = FlankerDLTime, FlankerMissedDeadlines, FlankerDLTotalAccuracy, FlankerDLCorrectRT, FlankerDLTotalRT) %>%
  distinct()

data_remove <- data_import %>%
  group_by(Subject) %>%
  summarise(ACC.mean = mean(TrialCriteria.Acc, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardized = TRUE) %>%
  filter(ACC.mean_z > sd.criteria | ACC.mean_z < (-1*sd.criteria))

data_flankerDL <- remove.save(data_flankerDL, remove = data_remove, save = here(output.dir, "remove"), taskname = "FlankerDL")

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_flankerDL, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())
