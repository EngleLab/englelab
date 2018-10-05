## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here::here("directories.rds"))
import.dir <- directories$emerge
output.dir <- directories$raw
##############

task <- "Antisaccade"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]`=="TrialProc" | `Procedure[Trial]`=="pracproc") %>%
  mutate(TrialProc = ifelse(`Procedure[Trial]`=="TrialProc", "real", "practice"),
         Trial = ifelse(TrialProc=="real", TrialList.Sample, practice.Sample),
         Accuracy = ifelse(is.na(Mask.ACC), masks.ACC, Mask.ACC),
         RT = ifelse(is.na(Mask.RT), masks.RT, Mask.RT),
         Target = ifelse(is.na(left_targ), right_targ, left_targ)) %>%
  select(Subject, TrialProc, Trial, Accuracy, RT, Target, FixationDuration = t4, SessionDate, SessionTime)

output.file <- paste(task, "_raw.txt", sep = "")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
