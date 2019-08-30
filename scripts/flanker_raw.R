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
task <- "Flanker"
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
  filter(`Procedure[Trial]` == "TrialProc" |
           `Procedure[Trial]` == "PracTrialProc" |
           `Procedure[Trial]` == "MapTrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "TrialProc"     ~ "real",
                               TrialProc == "PracTrialProc" ~ "practice",
                               TrialProc == "MapTrialProc"  ~ "practice"),
         Trial = case_when(TrialProc == "real"     ~ TrialList.Sample,
                           TrialProc == "practice" ~ PracTrialList.Sample),
         RT = case_when(TrialProc == "real"     ~ SlideTarget.RT,
                        TrialProc == "practice" ~ PracSlideTarget.RT),
         Accuracy = case_when(TrialProc == "real" ~ SlideTarget.ACC,
                              TrialProc == "practice" ~ PracSlideTarget.ACC),
         Response = case_when(TrialProc == "real" ~ SlideTarget.RESP,
                              TrialProc == "practice" ~ PracSlideTarget.RESP),
         Response = case_when(Response == "z" ~ "left",
                              Response == "{/}" ~ "right"),
         TargetArrowDirection =
           case_when(TrialProc == "real" ~ TargetDirection,
                     TrialProc == "practice" ~ TargerDirection),
         StartTime = min(PracSlideFixationStart.OnsetTime, na.rm = TRUE),
         FinishTime = max(SlideFixationEnd.OnsetTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Trial, Condition = FlankerType, RT, Accuracy,
         Response, TargetArrowDirection, AdminTime, SessionDate, SessionTime)

## Output Data
write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())
