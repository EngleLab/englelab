## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
import.dir <- "Data Files/Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "StroopDL"
import.file <- paste(task, ".txt", sep = "")
trial_output.file <- paste(task, "trial_raw.csv", sep = "_")
block_output.file <- paste(task, "block_raw.csv", sep = "_")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file),
                          "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates_remove(taskname = task,
                    output.folder = here(output.dir, "duplicates"))

## Clean up raw data and save
data_raw.trial <- data_import %>%
  filter(`Procedure[Trial]`=="BlockProc" |
           `Procedure[Trial]`=="stroopPRAC2" |
           `Procedure[Trial]`=="stroopPRAC") %>%
  rename(TrialProc = `Procedure[Trial]`,
         ResponseDeadline = `StroopDuration[SubTrial]`,
         FixationDuration = `DurationOfFixation[SubTrial]`) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "BlockProc"   ~ "real",
                               TrialProc == "stroopPRAC"  ~ "practice",
                               TrialProc == "stroopPRAC2" ~ "practice"),
         Block = TrialList.Cycle,
         Trial = case_when(TrialProc == "real" ~ SubTrial,
                           TrialProc == "practice" ~ PractList2.Sample),
         Condition = case_when(TrialProc == "real"     ~ trialType,
                               TrialProc == "practice" ~ pracTYPE),
         Condition = case_when(Condition == "Cong" ~ "congruent",
                               Condition == "Filler" ~ "congruent",
                               Condition == "Incong" ~ "incongruent"),
         RT = case_when(TrialProc == "real" &
                          is.na(stim.RESP) ~ MissedDL.RT + ResponseDeadline,
                        TrialProc == "real" &
                          !is.na(stim.RESP) ~ stim.RT,
                        TrialProc == "practice" ~ PracStim2.RT),
         Accuracy = case_when(TrialProc == "real" &
                                is.na(stim.RESP) ~ MissedDL.ACC,
                              TrialProc == "real" &
                                !is.na(stim.RESP) ~ stim.ACC,
                              TrialProc == "practice" ~ PracStim2.ACC),
         Response = case_when(TrialProc == "real" &
                                is.na(stim.RESP) ~ MissedDL.RESP,
                              TrialProc == "real" &
                                !is.na(stim.RESP) ~ stim.RESP,
                              TrialProc == "practice" ~ PracStim2.RESP),
         Response = case_when(Response == 1 ~ "GREEN",
                              Response == 2 ~ "BLUE",
                              Response == 3 ~ "RED",
                              TRUE ~ as.character(NA)),
         MissedDeadline = ifelse(TrialProc == "real" & is.na(stim.RESP), 1, 0),
         TrialCriteria.Acc = case_when(TrialProc == "real" ~ stim.ACC,
                                       TrialProc == "practice" ~ 0),
         Word = case_when(TrialProc == "real" ~ word,
                          TrialProc == "practice" ~ pracWORD),
         Hue = case_when(TrialProc == "real" ~ hue,
                         TrialProc == "practice" ~ pracHUE),
         StartTime = min(pracSTIM.OnsetTime, na.rm = TRUE),
         FinishTime = max(stim.RTTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Block, Trial, ResponseDeadline, Condition,
         RT, MissedDeadline, Accuracy, Response, TrialCriteria.Acc,
         Word, Hue, FixationDuration, StroopDLTime, StroopMissedDeadlines,
         StroopDLTotalAccuracy, StroopDLCorrectRT, StroopDLTotalRT,
         AdminTime, SessionDate, SessionTime)


data_raw.block <- data_raw.trial %>%
  group_by(Subject, Block) %>%
  mutate(Block.Correct = sum(TrialCriteria.Acc, na.rm = TRUE),
         BlockCriteria.Reach = ifelse(Block.Correct > 14, 1, 0)) %>%
  filter(Trial == 1, !is.na(Block)) %>%
  group_by(Subject, TrialProc) %>%
  mutate(PrevBlockCriteria.Reach = lag(BlockCriteria.Reach),
         Reversal =
           ifelse(PrevBlockCriteria.Reach != BlockCriteria.Reach, 1, 0)) %>%
  group_by(Subject, Reversal) %>%
  mutate(ReversalNumb = ifelse(Reversal == 1, row_number(), NA)) %>%
  select(Subject, Block, ResponseDeadline, Block.Correct,
         BlockCriteria.Reach, PrevBlockCriteria.Reach, Reversal, ReversalNumb,
         StroopDLTime, StroopMissedDeadlines, StroopDLTotalAccuracy,
         StroopDLCorrectRT, StroopDLTotalRT, SessionDate, SessionTime)

## Output Data
write_csv(data_raw.trial, path = here(output.dir, trial_output.file))
write_csv(data_raw.block, path = here(output.dir, block_output.file))

rm(list=ls())

