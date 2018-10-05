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

task <- "Stroop"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]`=="BlockProc" | `Procedure[Trial]`=="stroopPRAC2") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  mutate(TrialProc = ifelse(TrialProc=="BlockProc", "real", "practice"),
         Trial = ifelse(TrialProc=="real", TrialList.Sample, PractList2.Sample),
         Condition = ifelse(TrialProc=="real", trialType, pracTYPE),
         Condition = ifelse(Condition=="Cong" | Condition=="Filler", "congruent", "incongruent"),
         RT = ifelse(TrialProc=="real", stim.RT, PracStim2.RT),
         Accuracy = ifelse(TrialProc=="real", stim.ACC, PracStim2.ACC),
         Response = ifelse(TrialProc=="real", stim.RESP, PracStim2.RESP),
         Response = ifelse(Response==1, "GREEN", ifelse(Response==2, "BLUE", ifelse(Response==3, "RED", NA))),
         Word = ifelse(TrialProc=="real", word, pracWORD),
         Hue = ifelse(TrialProc=="real", hue, pracHUE)) %>%
  select(Subject, TrialProc, Trial, Condition, RT, Accuracy, Response, Word, Hue, SessionDate, SessionTime)

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
