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
task <- "Stroop"
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
  filter(`Procedure[Trial]` == "BlockProc" |
           `Procedure[Trial]` == "stroopPRAC2" |
           `Procedure[Trial]` == "stroopPRAC") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "BlockProc"   ~ "real",
                               TrialProc == "stroopPRAC"  ~ "practice",
                               TrialProc == "stroopPRAC2" ~ "practice"),
         Trial = case_when(TrialProc == "real"     ~ TrialList.Sample,
                           TrialProc == "practice" ~ PractList2.Sample),
         Condition = case_when(TrialProc == "real"     ~ trialType,
                               TrialProc == "practice" ~ pracTYPE),
         Condition = case_when(Condition == "Cong" ~ "congruent",
                               Condition == "Filler" ~ "congruent",
                               Condition == "Incong" ~ "incongruent"),
         RT = case_when(TrialProc == "real" ~ stim.RT,
                        TrialProc == "practice" ~ PracStim2.RT),
         Accuracy = case_when(TrialProc == "real" ~ stim.ACC,
                              TrialProc == "practice" ~ PracStim2.ACC),
         Response = case_when(TrialProc == "real" ~ stim.RESP,
                              TrialProc == "practice" ~ PracStim2.RESP),
         Response = case_when(Response == 1 ~ "GREEN",
                              Response == 2 ~ "BLUE",
                              Response == 3 ~ "RED",
                              TRUE ~ as.character(NA)),
         Word = case_when(TrialProc == "real" ~ word,
                          TrialProc == "practice" ~ pracWORD),
         Hue = case_when(TrialProc == "real" ~ hue,
                         TrialProc == "practice" ~ pracHUE),
         StartTime = min(pracSTIM.OnsetTime, na.rm = TRUE),
         FinishTime = max(stim.RTTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Trial, Condition, RT, Accuracy, Response,
         Word, Hue, AdminTime, SessionDate, SessionTime)

## Output Data
write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())

