## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
import.dir <- "Data Files/Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "RotSpan"
import.file <- paste(task, ".txt", sep = "")
output.file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file),
                          "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates_remove(taskname = task,
                    output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- raw_rotspan(data_import, blocks = 2) %>%
  filter(Trial <= 12) %>%
  group_by(Subject) %>%
  mutate(RotSpan.Partial = RotSpan.Partial_Block1 + RotSpan.Partial_Block2,
         RotSpan.RotationACC =
           sum(Processing.total, na.rm = TRUE) / sum(SetSize))

## Output Data
write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())
