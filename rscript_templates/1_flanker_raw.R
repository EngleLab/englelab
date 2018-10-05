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

task <- "Flanker"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]`=="TrialProc" | `Procedure[Trial]`=="PracTrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  mutate(TrialProc = ifelse(TrialProc=="TrialProc", "real", "practice"),
         Trial = ifelse(TrialProc=="real", TrialList.Sample, PracTrialList.Sample),
         RT = ifelse(TrialProc=="real", SlideTarget.RT, PracSlideTarget.RT),
         Accuracy = ifelse(TrialProc=="real", SlideTarget.ACC, PracSlideTarget.ACC),
         Response = ifelse(TrialProc=="real", SlideTarget.RESP, PracSlideTarget.RESP),
         Response = ifelse(Response=="z", "left", ifelse(Response=="{/}", "right", NA)),
         TargetArrowDirection = ifelse(TrialProc=="real", TargetDirection, TargerDirection)) %>%
  select(Subject, TrialProc, Trial, Condition = FlankerType, RT, Accuracy, Response, TargetArrowDirection, SessionDate, SessionTime)

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
