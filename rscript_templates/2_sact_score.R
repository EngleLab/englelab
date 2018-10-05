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

task <- "SACT"

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
  
## Score
data_import.real <- data_import %>% 
  filter(TrialProc=="real") %>%
  select(-Probe1Resp, -Probe2Resp, -Probe3Resp)

data_import.probe <- data_import %>%
  filter(TrialProc=="probe") %>%
  select(-RT, -Accuracy, -Response)

data_sact.real <- data_import.real %>%
  group_by(Subject) %>% 
  summarise(SACT_ACC.mean = mean(Accuracy, na.rm = TRUE))

data_sact.waittime <- data_import.real %>%
  group_by(Subject, WaitTime) %>%
  summarise(ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  reshape(key = "WaitTime", value = "ACC.mean", by = "Subject") %>%
  rename(SACT_2000_ACC.mean = `2000_ACC.mean`, 
         SACT_4000_ACC.mean = `4000_ACC.mean`,
         SACT_8000_ACC.mean = `8000_ACC.mean`, 
         SACT_12000_ACC.mean = `12000_ACC.mean`)

data_sact <- merge(data_sact.real, data_sact.waittime, by = "Subject")

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_sact, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())

