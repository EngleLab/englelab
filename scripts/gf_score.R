## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set output file name
output_file <- "Gf_Scores.csv"
##############

## RAPM ####
import_file <- "RAPM_raw.csv"
## Import and remove duplicate subjects
import_RAPM <- read_csv(here(import_dir, import_file))
## Score
data_RAPM <- import_RAPM %>%
  select(Subject, RAPM = TotalScore) %>%
  distinct()
#############

## LetterSets ####
import_file <- "LetterSets_raw.csv"
## Import and remove duplicate subjects
import_LS <- read_csv(here(import_dir, import_file))
## Score
data_LS <- import_LS %>%
  select(Subject, LetterSets = TotalScore) %>%
  distinct()
#############

## NumberSeries ####
import_file <- "NumberSeries_raw.csv"
## Import and remove duplicate subjects
import_NS <- read_csv(here(import_dir, import_file))
## Score
data_NS <- import_NS %>%
  select(Subject, NumberSeries = TotalScore) %>%
  distinct()
###############

## Merge data from tasks
data_Gf <- plyr::join_all(list(data.frame(data_RAPM),
                               data.frame(data_LS),
                               data.frame(data_NS)),
                          by = "Subject", type = "full")

## Save data ####
write_csv(data_Gf, path = here(output_dir, output_file))

rm(list=ls())

