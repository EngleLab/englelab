## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
import.dir <- "Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "SACT"
import.file <- paste(task, ".txt", sep = "")
output.file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file),
                          "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates_remove(taskname = task,
                    output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]` == "TrialProc"|
           `Procedure[Trial]` == "PracticeTrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "TrialProc" ~ "real",
                               TrialProc == "PracticeTrialProc" ~ "practice"),
         StartTime = min(PractBegin.OnsetTime, na.rm = TRUE),
         FinishTime = max(Response.RTTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Trial, WaitTime,
         RT = ResponseRT, Accuracy = Response.ACC, Response = ResponseMade,
         AdminTime, SessionDate, SessionTime)

write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())

