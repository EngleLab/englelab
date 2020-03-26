## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Merged"
output_dir <- "Data Files"

## Set import/output files
task <- "LetterSets"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import_dir, import_file), "\t",
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000)

## Clean up raw data
data_raw <- data_import %>%
  filter(Blocks == "Real" | Blocks == "End",
         ShowStim.RT > 0 | !is.na(TotalScore)) %>%
  select(Subject, Trial, Answer = answer, Response = ItemResp,
         Accuracy = ShowStim.ACC, RT = ShowStim.RT,
         TimeLeft = StopTime, TotalScore, SessionDate, SessionTime) %>%
  group_by(Subject) %>%
  mutate(TotalScore = mean(TotalScore, na.rm = TRUE)) %>%
  filter(!is.na(Trial))

## Save Data
write_csv(data_raw, path = here(output_dir, output_file))

rm(list=ls())
