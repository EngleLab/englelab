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

task <- "StroopDL"

## Import Data
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw.trial <- data_import %>%
  filter(`Procedure[Trial]`=="BlockProc" | `Procedure[Trial]`=="stroopPRAC2") %>%
  rename(TrialProc = `Procedure[Trial]`,
         ResponseDeadline = `StroopDuration[SubTrial]`,
         FixationDuration = `DurationOfFixation[SubTrial]`) %>%
  mutate(TrialProc = ifelse(TrialProc=="BlockProc", "real", "practice"),
         Block = TrialList.Cycle,
         Trial = ifelse(TrialProc=="real", SubTrial, PractList2.Sample),
         Condition = ifelse(TrialProc=="real", trialType, pracTYPE),
         Condition = ifelse(Condition=="Cong" | Condition=="Filler", "congruent", "incongruent"),
         RT = ifelse(TrialProc=="real", 
                     ifelse(is.na(stim.RESP), MissedDL.RT + ResponseDeadline, stim.RT), PracStim2.RT),
         Accuracy = ifelse(TrialProc=="real", 
                               ifelse(is.na(stim.RESP), MissedDL.ACC, stim.ACC), PracStim2.ACC),
         Response = ifelse(TrialProc=="real", 
                           ifelse(is.na(stim.RESP), MissedDL.RESP, stim.RESP), PracStim2.RESP),
         Response = ifelse(Response==1, "GREEN", ifelse(Response==2, "BLUE", ifelse(Response==3, "RED", NA))),
         MissedDeadline = ifelse(TrialProc=="real" & is.na(stim.RESP), 1, 0),
         TrialCriteria.Acc = ifelse(TrialProc=="real", stim.ACC, 0),
         Word = ifelse(TrialProc=="real", word, pracWORD),
         Hue = ifelse(TrialProc=="real", hue, pracHUE)) %>%
  select(Subject, TrialProc, Block, Trial, ResponseDeadline, Condition, 
         RT, MissedDeadline, Accuracy, Response, TrialCriteria.Acc,
         Word, Hue, FixationDuration,
         StroopDLTime, StroopMissedDeadlines, StroopDLTotalAccuracy, StroopDLCorrectRT, StroopDLTotalRT)


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
         StroopDLTime, StroopMissedDeadlines, StroopDLTotalAccuracy, StroopDLCorrectRT, StroopDLTotalRT)


output.file <- paste(task, "trial_raw.txt", sep = "_")
write_delim(data_raw.trial, path = here(output.dir, output.file), "\t", na = "")
output.file <- paste(task, "block_raw.txt", sep = "_")
write_delim(data_raw.block, path = here(output.dir, output.file), "\t", na = "")

rm(list=ls())
