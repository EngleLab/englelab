## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$scored
output.dir <- directories$scored
##############

## Score task data from raw data files
source(here(directrories$scripts, "2_antisaccade_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_flanker_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_flankerDL_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_flankerPR_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_stroop_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_stroopDL_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_va4_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_pvt_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_sact_score.R"), echo=TRUE)
source(here(directrories$scripts, "2_cuedlinedetection_score.R"), echo=TRUE)

## Import Files
data_as <- read_delim(here(import.dir, "Antisaccade_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_flanker <- read_delim(here(import.dir, "Flanker_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_flankerDL <- read_delim(here(import.dir, "FlankerDL_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_flankerPR <- read_delim(here(import.dir, "FlankerPR_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_stroop <- read_delim(here(import.dir, "Stroop_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_stroopDL <- read_delim(here(import.dir, "StroopDL_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_va4 <- read_delim(here(import.dir, "VA4_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_pvt <- read_delim(here(import.dir, "PVT_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_sact <- read_delim(here(import.dir, "SACT_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)
data_cuedlinedetection <- read_delim(here(import.dir, "CuedLineDetection_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)

## Merge attention tasks into one dataframe
data_AC <- plyr::join_all(list(data_as,
                               data_flanker,
                               data_flankerDL,
                               data_flankerPR,
                               data_stroop, 
                               data_stroopDL,
                               data_va4,
                               data_pvt,
                               data_sact,
                               data_cuedlinedetection), by = "Subject", type = "full")

## Save
write_delim(data_AC, path = here(output.dir, "Attention_Scores.txt"), delim = "\t", na = "")

rm(list=ls())