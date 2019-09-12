## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
import.dir <- "Data Files/Raw Data"
output.dir <- "Data Files/Scored Data"
removed.dir <- "Data Files/Scored Data/removed"

## Set import/output files
task <- "Flanker"
import.file <- paste(task, "_raw.csv", sep = "")
output.file <- paste(task, "_Scores.csv", sep = "")
removed.file <- paste(task, "_removed.csv", sep = "")

## Set Trimming criteria
rt.min <- 200
acc.criteria <- -3.5
##############

## Import Data
data_import <- read_csv(here(import.dir, import.file)) %>%
  filter(TrialProc == "real")
###################

## Trim RTs ####
data_trim <- data_import %>%
  filter(TrialProc == "real") %>%
  mutate(Accuracy = ifelse(RT < rt.min, 0, Accuracy),
         RT = ifelse(RT < rt.min, NA, RT))
################

## Calculate Flanker scores using Accurate trials only ####
data_flanker <- data_trim %>%
  mutate(RT = ifelse(Accuracy == 0, NA, RT)) %>%
  group_by(Subject, Condition) %>%
  summarise(RT.mean = mean(RT, na.rm = TRUE),
            RT.sd = sd(RT, na.rm = TRUE),
            ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_spread(variables = "Condition",
                 values = c("ACC.mean", "RT.mean", "RT.sd"),
                 id = "Subject") %>%
  mutate(FlankerEffect_RT = incongruent_RT.mean - congruent_RT.mean,
         FlankerEffect_ACC = incongruent_ACC.mean - congruent_ACC.mean) %>%
  rename(Flanker_incongruent_RT.mean = incongruent_RT.mean,
         Flanker_congruent_RT.mean = congruent_RT.mean,
         Flanker_incongruent_ACC.mean = incongruent_ACC.mean,
         Flanker_congruent_ACC.mean = congruent_ACC.mean)
###########################################################

## Remove subjects with poor performance based on acc.criteria ####
data_remove <- data_flanker %>%
  center(variables = c("Flanker_congruent_ACC.mean", "Flanker_incongruent_ACC.mean"), 
         standardize = TRUE) %>%
  filter(Flanker_congruent_ACC.mean_z < acc.criteria | 
           Flanker_incongruent_ACC.mean_z < acc.criteria)

data_flanker <- remove_save(data_flanker, data_remove,
                            output.dir = removed.dir,
                            output.file = removed.file)

## Calculate Bin Scores ####
data_binned <- data_trim %>%
  filter(!is.na(RT), !(Subject %in% data_remove$Subject)) %>%
  bin_score(condition.col = "Condition", baseline.condition = "congruent") %>%
  rename(FlankerBin = "BinScore")
############################

## Merge Flanker and Binned scores, then save ####
data_flanker <- merge(data_flanker, data_binned, by = "Subject", all = TRUE)
write_csv(data_flanker, path = here(output.dir, output.file))
##################################################

rm(list=ls())
