## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "Stroop"

# Set RT trimming variables. How many SDs above and below the mean?
min_RT.criteria <- 200
sd.criteria <- 3.5

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(TrialProc=="real")

## Calculate Stroop scores based on trimming long RT trials and removing low performers
data_stroop <- data_import %>%
  mutate(RT = ifelse(Accuracy==0 | RT < min_RT.criteria, NA, RT)) %>%
  trim(variables = "RT", context = c("Subject", "Condition"), replace = "cutoff") %>%
  group_by(Subject, Condition) %>%
  summarise(RT.mean = mean(RT, na.rm = TRUE),
            RT.sd = sd(RT, na.rm = TRUE),
            ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape(key = "Condition", values = c("ACC.mean", "RT.mean", "RT.sd"), by = "Subject") %>%
  mutate(StroopEffect_RT = incongruent_RT.mean - congruent_RT.mean,
         StroopEffect_ACC = incongruent_ACC.mean - congruent_ACC.mean)

## Subjects that got too few of trials correct
data_remove <- data_stroop %>%
  center(variables = c("congruent_ACC.mean", "incongruent_ACC.mean"), standardized = TRUE) %>%
  filter(congruent_ACC.mean_z > sd.criteria | congruent_ACC.mean_z < (-1*sd.criteria) |
           incongruent_ACC.mean_z > sd.criteria | incongruent_ACC.mean_z < (-1*sd.criteria))

data_stroop <- remove.save(data_stroop, remove = data_remove, 
                            save = here(output.dir, "remove"), taskname = "Stroop")

## Calculate Bin Scores ####
data_binned <- data_import %>%
  filter(RT >= min_RT.criteria) %>%
  trim(variables = "RT", context = c("Subject", "Condition"), replace = "cutoff") %>%
  filter(!(Subject %in% data_remove$Subject)) %>%
  bin.score(Condition.label = "Condition", baseline.condition = "congruent") %>%
  rename(StroopBin = "BinScore")
############################

data_stroop <- merge(data_stroop, data_binned, by = "Subject")

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_stroop, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())

