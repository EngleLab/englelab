#### Setup ####
## Load packages
library(here)
library(readr)
library(dplyr)

## Set Import/Output Directories
import_dir <- "Data Files/Merged"
output_dir <- "Data Files"

## Set Import/Output Filenames
task <- "taskname"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")
################

#### Import ####
data_import <- read_delim(here(import_dir, import_file),
                     "\t", escape_double = FALSE, trim_ws = TRUE)
################

#### Tidy raw data ####
data_raw <- data_import %>%
  rename(TrialProc = `Procedure[Trial]`) %>%
  filter(TrialProc == "showproc" | TrialProc == "pracproc") %>%
  mutate(TrialProc = case_when(TrialProc == "showproc" ~ "real",
                               TrialProc == "pracproc" ~ "practice"),
         Response = case_when(VisResponse.RESP == 5 ~ "same",
                              VisResponse.RESP == 6 ~ "different",
                              TRUE ~ as.character(NA)),
         CorrectResponse = case_when(VisResponse.CRESP == 5 ~ "same",
                                     VisResponse.CRESP == 6 ~ "different"),
         CorrectRejection = case_when(CorrectResponse == "same" & 
                                        Response == "same" ~ 1,
                                      TRUE ~ 0),
         FalseAlarm = case_when(CorrectResponse == "same" & 
                                  Response == "different" ~ 1,
                                TRUE ~ 0),
         Miss = case_when(CorrectResponse == "different" & 
                            Response == "same" ~ 1,
                          TRUE ~ 0),
         Hit = case_when(CorrectResponse == "different" & 
                           Response == "different" ~ 1,
                         TRUE ~ 0)) %>%
  select(Subject, TrialProc, Trial, SetSize, Accuracy = VisResponse.ACC, 
         Response, CorrectResponse, CorrectRejection, FalseAlarm, Miss, Hit, 
         SessionDate, SessionTime)
#######################

#### Output ####
write_csv(data_raw, here(output_dir, output_file))
################

rm(list=ls())
