## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library()

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

# Set Accuracy Criteria
sd.criteria <- 3.5

## OSPAN ####
task <- "OSpan"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score 
data_OSpan <- data_import %>%
  select(-Trial, -SetSize, -Recall.correct, -Math.correct) %>%
  distinct()

data_remove <- data_OSpan %>%
  center(variables = "OSpan.MathACC", standardized = TRUE) %>%
  filter(OSpan.MathACC_z > sd.criteria | OSpan.MathACC_z < (-1*sd.criteria))

data_OSpan <- remove.save(data_OSpan, remove = data_remove, save = here(output.dir, "remove"), taskname = "OSpan")
#############

## SSPAN ####
task <- "SymSpan"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score
data_SymSpan <- data_import %>%
  select(-Trial, -SetSize, -Recall.correct, -Symm.correct) %>%
  distinct()

data_remove <- data_SymSpan %>%
  center(variables = "SymSpan.SymmetryACC", standardized = TRUE) %>%
  filter(SymSpan.SymmetryACC_z > sd.criteria | SymSpan.SymmetryACC_z < (-1*sd.criteria))

data_SymSpan <- remove.save(data_SymSpan, remove = data_remove, save = here(output.dir, "remove"), taskname = "SymSpan")
#############

## RotSPAN ####
task <- "RotSpan"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score
data_RotSpan <- data_import %>%
  select(-Trial, -SetSize, -Recall.correct, -Rotations.correct) %>%
  distinct()

data_remove <- data_RotSpan %>%
  center(variables = "RotSpan.RotationACC", standardized = TRUE) %>%
  filter(RotSpan.RotationACC_z > sd.criteria | RotSpan.RotationACC_z < (-1*sd.criteria))

data_RotSpan <- remove.save(data_RotSpan, remove = data_remove, save = here(output.dir, "remove"), taskname = "RotSpan")
###############

## Merge data from tasks and save to file
data_WMC <- plyr::join_all(list(data.frame(data_OSpan),
                                data.frame(data_SymSpan),
                                data.frame(data_RotSpan)), by = "Subject", type = "full")

## Saving output ####
output.file <- "WMC_Scores.txt"
write_delim(data_WMC, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())

