#### Setup ####
## Load Packages
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(datawrangling)

## Set Import/Output Directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"
removed_dir <- "Data Files/Scored Data/removed"

## Set Import/Output Filenames
task <- "taskname"
import_file <- paste(task, "raw.csv", sep = "_")
output_file <- paste(task, "Scores.csv", sep = "_")
removed_file <- paste(task, "_removed.csv", sep = "")

## Set Data Cleaning Params
acc_criteria <- -3.5
###############

#### Import ####
data_import <- read_csv(here(import_dir, import_file))
################

#### Data Cleaning and Scoring ####
data_scores <- data_import %>%
  filter(TrialProc == "real") %>%
  group_by(Subject, SetSize) %>%
  summarise(CR.n = sum(CorrectRejection, na.rm = TRUE),
            FA.n = sum(FalseAlarm, na.rm = TRUE),
            M.n = sum(Miss, na.rm = TRUE),
            H.n = sum(Hit, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(CR = CR.n / (CR.n + FA.n),
         H = H.n / (H.n + M.n),
         k = SetSize * (H + CR -1)) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "SetSize",
              values_from = "k",
              names_prefix = "k.") %>%
  mutate(VA_k = (k.5 + k.7) / 2)

## Remove problematic subjects
data_remove <- data_import %>%
  group_by(Subject) %>%
  summarise(ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardize = TRUE) %>%
  filter(ACC.mean < acc_criteria)

data_scores <- remove_save(data_scores, data_remove,
                           output_dir = here(removed_dir),
                           output_file = removed_file)
###################################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
