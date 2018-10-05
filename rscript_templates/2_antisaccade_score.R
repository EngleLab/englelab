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

task <- "Antisaccade"

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(TrialProc=="real")

## Score
data_as <- data_import %>%
  group_by(Subject) %>%
  summarise(Antisaccade_ACC.mean = mean(Accuracy, na.rm = TRUE), Antisaccade_RT.mean = mean(RT, na.rm = TRUE))

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_as, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())