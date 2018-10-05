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

task <- "SACT"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(`Procedure[Trial]`=="ProbeTrialProc"|`Procedure[Trial]`=="TrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  mutate(TrialProc = ifelse(TrialProc=="TrialProc", "real", "probe")) %>%
  select(Subject, TrialProc, Trial, WaitTime, 
         RT = ResponseRT, Accuracy = Response.ACC, Response = ResponseMade, Probe1Resp, Probe2Resp, Probe3Resp)

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
