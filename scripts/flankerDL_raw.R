## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)

## Set import/output directories
import.dir <- "Data Files/Merged"
output.dir <- "Data Files"

## Set import/output files
task <- "FlankerDL"
import.file <- paste(task, ".txt", sep = "")
trial_output.file <- paste(task, "trial_raw.csv", sep = "_")
block_output.file <- paste(task, "block_raw.csv", sep = "_")
##############

## Import Data
data_import <- read_delim(here(import.dir, import.file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE)

## Clean up raw data and save
data_raw.trial <- data_import %>%
  filter(`Procedure[Trial]` == "TrialProc" |
           `Procedure[Trial]` == "PracTrialProc" |
           `Procedure[Trial]` == "MapTrialProc") %>%
  rename(TrialProc = `Procedure[Trial]`,
         ResponseDeadline = ArrowDuration,
         FixationDuration = DurationOfFixation) %>%
  group_by(Subject) %>%
  mutate(TrialProc = case_when(TrialProc == "TrialProc"     ~ "real",
                               TrialProc == "PracTrialProc" ~ "practice",
                               TrialProc == "MapTrialProc"  ~ "practice"),
         Block = TrialList.Cycle,
         RT =
           case_when(TrialProc == "real" &
                       SlideTarget.RT == 0 ~ MissedDeadline.RT + ResponseDeadline,
                     TrialProc == "real" &
                       SlideTarget.RT > 0  ~ SlideTarget.RT,
                     TrialProc == "practice" ~ PracSlideTarget.RT),
         Accuracy =
           case_when(TrialProc == "real" &
                       is.na(SlideTarget.RESP)  ~ MissedDeadline.ACC,
                     TrialProc == "real" &
                       !is.na(SlideTarget.RESP) ~ SlideTarget.ACC,
                     TrialProc == "practice"    ~ PracSlideTarget.ACC),
         Response =
           case_when(TrialProc == "real" &
                       is.na(SlideTarget.RESP)  ~ MissedDeadline.RESP,
                     TrialProc == "real" &
                       !is.na(SlideTarget.RESP) ~ SlideTarget.RESP,
                     TrialProc == "practice"    ~ PracSlideTarget.RESP),
         Response = case_when(Response == "z"   ~ "left",
                              Response == "{/}" ~ "right",
                              TRUE              ~ as.character(NA)),
         MissedDeadline = ifelse(TrialProc == "real" &
                                   is.na(SlideTarget.RESP), 1, 0),
         TrialCriteria.Acc =
           case_when(TrialProc == "real"     ~ SlideTarget.ACC,
                     TrialProc == "practice" ~ 0),
         TargetArrowDirection =
           case_when(TrialProc == "real"     ~ TargetDirection,
                     TrialProc == "practice" ~ TargerDirection),
         StartTime = min(PracSlideFixationStart.OnsetTime, na.rm = TRUE),
         FinishTime = max(SlideFixationEnd1.OnsetTime, na.rm = TRUE),
         AdminTime = (FinishTime - StartTime) / 60000) %>%
  select(Subject, TrialProc, Block, Trial, ResponseDeadline,
         Condition = FlankerType, RT, MissedDeadline, Accuracy, Response,
         TrialCriteria.Acc, TargetArrowDirection, FixationDuration,
         FlankerDLTime = ArrowDLTime,
         FlankerMissedDeadlines = ArrowMissedDeadlines,
         FlankerDLTotalAccuracy = ArrowDLTotalAccuracy,
         FlankerDLCorrectRT = ArrowDLCorrectRT,
         FlankerDLTotalRT = ArrowDLTotalRT, AdminTime, SessionDate, SessionTime)


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
         FlankerDLTime, FlankerMissedDeadlines, FlankerDLTotalAccuracy,
         FlankerDLCorrectRT, FlankerDLTotalRT, SessionDate, SessionTime)

## Output data
write_csv(data_raw.trial, path = here(output.dir, trial_output.file))
write_csv(data_raw.block, path = here(output.dir, block_output.file))

rm(list=ls())
