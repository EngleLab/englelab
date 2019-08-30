## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"
##############

## RAPM ####
import.file <- "RAPM_raw.csv"
## Import and remove duplicate subjects
import_RAPM <- read_csv(here(import.dir, import.file))
## Score
data_RAPM <- import_RAPM %>%
  select(Subject, RAPM = TotalScore) %>%
  distinct()
#############

## LetterSets ####
import.file <- "LetterSets_raw.csv"
## Import and remove duplicate subjects
import_LS <- read_csv(here(import.dir, import.file))
## Score
data_LS <- import_LS %>%
  select(Subject, LetterSets = TotalScore) %>%
  distinct()
#############

## NumberSeries ####
import.file <- "NumberSeries_raw.csv"
## Import and remove duplicate subjects
import_NS <- read_csv(here(import.dir, import.file))
## Score
data_NS <- import_NS %>%
  select(Subject, NumberSeries = TotalScore) %>%
  distinct()
###############

## Merge data from tasks and save to file
data_Gf <- plyr::join_all(list(data.frame(data_RAPM),
                               data.frame(data_LS),
                               data.frame(data_NS)),
                          by = "Subject", type = "full")

## Saving output ####
write_csv(data_Gf, path = here(output.dir, "Gf_Scores.csv"))

rm(list=ls())

