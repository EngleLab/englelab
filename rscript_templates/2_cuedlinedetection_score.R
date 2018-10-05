## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "CuedLineDetection"

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)

data_cuedlinedetection <- data_import %>% 
  group_by(Subject) %>%
  mutate(NextTrial.Acc = lag(Acc),
         Reversal = ifelse(NextTrial.Acc!=Acc, 1, 0)) %>%
  group_by(Subject, Reversal) %>%
  mutate(ReversalNumb = ifelse(Reversal==1, row_number(), NA)) %>%
  group_by(Subject) %>%
  mutate(last4rev.target = ifelse(ReversalNumb>(max(ReversalNumb, na.rm = TRUE)-4), Target, NA),
         ACT_Threshold.last4rev = mean(last4rev.target, na.rm = TRUE)) %>%
  select(Subject, CuedLineDetection_Threshold.last4rev = ACT_Threshold.last4rev) %>%
  distinct()

## Save scored data file
output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_cuedlinedetection, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())
