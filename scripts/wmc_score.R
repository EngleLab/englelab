## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)
library(datawrangling)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"
removed_dir <- "Data Files/Scored Data/removed"

## Set output file name
output_file <- "WMC_Scores.csv"

## Set Trimming criteria
acc_criteria <- -3.5
##############

## OSpan ####
task <- "OSpan"
import_file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import_dir, import_file))
## Score
data_OSpan <- data_import %>%
  select(Subject, contains(task), contains("Time"), contains("Date")) %>%
  distinct()

data_remove <- data_OSpan %>%
  center(variables = "OSpan.MathACC", standardize = TRUE) %>%
  filter(OSpan.MathACC_z < acc_criteria)

data_OSpan <- remove_save(data_OSpan, data_remove,
                          output.dir = removed_dir,
                          output.file = paste(task, "removed.csv", sep = "_"))
#############

## SymSpan ####
task <- "SymSpan"
import_file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import_dir, import_file))
## Score
data_SymSpan <- data_import %>%
  select(Subject, contains(task), contains("Time"), contains("Date")) %>%
  distinct()

data_remove <- data_SymSpan %>%
  center(variables = "SymSpan.SymmetryACC", standardize = TRUE) %>%
  filter(SymSpan.SymmetryACC_z < acc_criteria)

data_SymSpan <- remove_save(data_SymSpan, data_remove,
                            output.dir = removed_dir,
                            output.file = paste(task, "removed.csv", sep = "_"))
#############

## RotSpan ####
task <- "RotSpan"
import_file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import_dir, import_file))
## Score
data_RotSpan <- data_import %>%
  select(Subject, contains(task), contains("Time"), contains("Date")) %>%
  distinct()

data_remove <- data_RotSpan %>%
  center(variables = "RotSpan.RotationACC", standardize = TRUE) %>%
  filter(RotSpan.RotationACC_z < acc_criteria)

data_RotSpan <- remove_save(data_RotSpan, data_remove,
                            output.dir = removed_dir,
                            output.file = paste(task, "removed.csv", sep = "_"))
###############

## Merge data from tasks
data_WMC <- plyr::join_all(list(data.frame(data_OSpan),
                                data.frame(data_SymSpan),
                                data.frame(data_RotSpan)),
                           by = "Subject", type = "full")

## Saving data
write_csv(data_WMC, path = here(output_dir, output_file))

rm(list=ls())

