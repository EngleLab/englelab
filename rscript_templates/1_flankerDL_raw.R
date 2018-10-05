## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here::here("directories.rds"))
import.dir <- directories$emerge
output.dir <- directories$raw
##############

task <- "FlankerDL"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw.trial <- data_import %>%
  filter(`Procedure[Trial]`=="TrialProc" | `Procedure[Trial]`=="PracTrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`,
         ResponseDeadline = ArrowDuration,
         FixationDuration = DurationOfFixation) %>%
  mutate(TrialProc = ifelse(TrialProc=="TrialProc", "real", "practice"),
         Block = TrialList.Cycle,
         RT = ifelse(TrialProc=="real", 
                     ifelse(SlideTarget.RT==0, MissedDeadline.RT + ResponseDeadline, SlideTarget.RT), PracSlideTarget.RT),
         Accuracy = ifelse(TrialProc=="real", 
                               ifelse(is.na(SlideTarget.RESP), MissedDeadline.ACC, SlideTarget.ACC), PracSlideTarget.ACC),
         Response = ifelse(TrialProc=="real", 
                           ifelse(is.na(SlideTarget.RESP), MissedDeadline.RESP, SlideTarget.RESP), PracSlideTarget.RESP),
         Response = ifelse(Response=="z", "left", ifelse(Response=="{/}", "right", NA)),
         MissedDeadline = ifelse(TrialProc=="real" & is.na(SlideTarget.RESP), 1, 0),
         TrialCriteria.Acc = ifelse(TrialProc=="real", SlideTarget.ACC, 0),
         TargetArrowDirection = ifelse(TrialProc=="real", TargetDirection, TargerDirection)) %>%
  select(Subject, TrialProc, Block, Trial, ResponseDeadline, Condition = FlankerType, 
         RT, MissedDeadline, Accuracy, Response, TrialCriteria.Acc,
         TargetArrowDirection, FixationDuration,
         FlankerDLTime = ArrowDLTime, FlankerMissedDeadlines = ArrowMissedDeadlines, 
         FlankerDLTotalAccuracy = ArrowDLTotalAccuracy, FlankerDLCorrectRT = ArrowDLCorrectRT, 
         FlankerDLTotalRT = ArrowDLTotalRT)


data_raw.block <- data_raw.trial %>%
  group_by(Subject, Block) %>%
  mutate(Block.Correct = sum(TrialCriteria.Acc, na.rm = TRUE),
         BlockCriteria.Reach = ifelse(Block.Correct > 14, 1, 0)) %>%
  filter(Trial==1, !is.na(Block)) %>%
  group_by(Subject, TrialProc) %>%
  mutate(PrevBlockCriteria.Reach = lag(BlockCriteria.Reach),
         Reversal = ifelse(PrevBlockCriteria.Reach!=BlockCriteria.Reach, 1, 0)) %>%
  group_by(Subject, Reversal) %>%
  mutate(ReversalNumb = ifelse(Reversal==1, row_number(), NA)) %>%
  select(Subject, Block, ResponseDeadline, Block.Correct,
         BlockCriteria.Reach, PrevBlockCriteria.Reach, Reversal, ReversalNumb,
         FlankerDLTime, FlankerMissedDeadlines, FlankerDLTotalAccuracy, FlankerDLCorrectRT, FlankerDLTotalRT)


output.file <- paste(task, "trial_raw.txt", sep = "_")
write_delim(data_raw.trial, path = here(output.dir, output.file), "\t", na = "")
output.file <- paste(task, "block_raw.txt", sep = "_")
write_delim(data_raw.block, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
