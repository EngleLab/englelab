## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"
removed.dir <- "Data Files/Scored Data/removed"

## Set Trimming criteria
acc.criteria <- -3.5
##############

## OSPAN ####
task <- "OSpan"
import.file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import.dir, import.file))
## Score
data_OSpan <- data_import %>%
  select(Subject, OSpan.Absolute:SessionTime) %>%
  distinct()

data_remove <- data_OSpan %>%
  center(variables = "OSpan.MathACC", standardize = TRUE) %>%
  filter(OSpan.MathACC_z < acc.criteria)

data_OSpan <- remove_save(data_OSpan, data_remove,
                          output.dir = removed.dir,
                          output.file = paste(task, "removed.csv", sep = "_"))
#############

## SSPAN ####
task <- "SymSpan"
import.file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import.dir, import.file))
## Score
data_SymSpan <- data_import %>%
  select(Subject, SymSpan.Absolute:SessionTime) %>%
  distinct()

data_remove <- data_SymSpan %>%
  center(variables = "SymSpan.SymmetryACC", standardize = TRUE) %>%
  filter(SymSpan.SymmetryACC_z < acc.criteria)

data_SymSpan <- remove_save(data_SymSpan, data_remove,
                            output.dir = removed.dir,
                            output.file = paste(task, "removed.csv", sep = "_"))
#############

## RotSPAN ####
task <- "RotSpan"
import.file <- paste(task, "raw.csv", sep = "_")
## Import and remove duplicate subjects
data_import <- read_csv(here(import.dir, import.file))
## Score
data_RotSpan <- data_import %>%
  select(Subject, RotSpan.Absolute:SessionTime) %>%
  distinct()

data_remove <- data_RotSpan %>%
  center(variables = "RotSpan.RotationACC", standardize = TRUE) %>%
  filter(RotSpan.RotationACC_z < acc.criteria)

data_RotSpan <- remove_save(data_RotSpan, data_remove,
                            output.dir = removed.dir,
                            output.file = paste(task, "removed.csv", sep = "_"))
###############

## Merge data from tasks and save to file
data_WMC <- plyr::join_all(list(data.frame(data_OSpan),
                                data.frame(data_SymSpan),
                                data.frame(data_RotSpan)),
                           by = "Subject", type = "full")

## Saving output ####
write_csv(data_WMC, path = here(output.dir, "WMC_Scores.csv"))

rm(list=ls())

