## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
import.dir <- "Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "NumberSeries"
import.file <- paste(task, ".txt", sep = "")
output.file <- paste(task, "_raw.csv", sep = "")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file),
                          "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates_remove(taskname = task,
                    output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw <- data_import %>%
  filter(Blocks == "Real" | Blocks == "End",
         ShowStim.RT > 0 | !is.na(TotalScore)) %>%
  select(Subject, Trial, Answer = answer, Response = ItemResp,
         Accuracy = ShowStim.ACC, RT = ShowStim.RT,
         TimeLeft = StopTime, TotalScore, SessionDate, SessionTime) %>%
  group_by(Subject) %>%
  mutate(TotalScore = mean(TotalScore, na.rm = TRUE)) %>%
  filter(!is.na(Trial))

## Output Data
write_csv(data_raw, path = here(output.dir, output.file))

rm(list=ls())
