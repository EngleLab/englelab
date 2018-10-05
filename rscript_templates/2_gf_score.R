## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

## RAPM ####
task <- "RAPM"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score 
data_RAPM <- data_import %>%
  select(Subject, RAPM = TotalScore) %>%
  distinct()

#############

## LetterSets ####
task <- "LetterSets"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score
data_LS <- data_import %>%
  select(Subject, LetterSets = TotalScore) %>%
  distinct()

#############

## NumberSeries ####
task <- "NumberSeries"
## Import and remove duplicate subjects
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)
## Score
data_NS <- data_import %>%
  select(Subject, NumberSeries = TotalScore) %>%
  distinct()

###############

## Merge data from tasks and save to file
data_Gf <- plyr::join_all(list(data.frame(data_RAPM),
                               data.frame(data_LS),
                               data.frame(data_NS)), by = "Subject", type = "full")

## Saving output ####
output.file <- "Gf_Scores.txt"
write_delim(data_Gf, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())
