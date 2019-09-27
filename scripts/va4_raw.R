## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
import.dir <- "Data Files/Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "VisualArrays_4"
import.file <- paste(task, ".txt", sep = "")
output.file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE)

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]` == "showproc" |
           `Procedure[Trial]` == "pracproc") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "showproc" ~ "real",
                               TrialProc == "pracproc" ~ "practice"),
         StartTime = min(VisResponse.OnsetTime, na.rm = TRUE),
         FinishTime = max(VisResponse.RTTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Trial, SetSize,
         Accuracy = VisResponse.ACC, Response = VisResponse.RESP,
         Answer = VisResponse.CRESP, Answer.key = Correct,
         AdminTime, SessionDate, SessionTime)

write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())

