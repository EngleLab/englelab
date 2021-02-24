## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set import/output files
task <- "VAorient_S"
import_file <- paste(task, "_raw.csv", sep = "")
output_file <- paste(task, "_Scores.csv", sep = "")
##############

#### Import Data ####
data_import <- read_csv(here(import_dir, import_file)) %>%
  filter(TrialProc == "real")
#####################

#### Score Data ####
data_scores <- data_import %>%
  group_by(Subject) %>%
  score_visualarrays() %>%
  pivot_wider(id_cols = "Subject",
              names_from = "SetSize",
              names_prefix = "VAorient_S.k_",
              values_from = "k") %>%
  mutate(VAorient_S.k = (VAorient_S.k_5 + VAorient_S.k_7) / 2)
####################

#### Clean Data ####
# remove problematic subjects based on some criteria
# remove outliers based on accuracy
####################

#### Calculate Reliability ####
splithalf <- data_raw %>%
  mutate(Split = ifelse(Trial %% 2, "odd", "even")) %>%
  group_by(Subject, Split) %>%
  score_visualarrays() %>%
  pivot_wider(id_cols = "Subject",
              names_from = c("SetSize", "Split"),
              names_prefix = "k_",
              values_from = "k") %>%
  mutate(VAorient_S.k_even = (k_5_even + k_7_even) / 2,
         VAorient_S.k_odd = (k_5_odd + k_7_odd) / 2) %>%
  summarise(r_VA.k = cor(VAorient_S.k_even, VAorient_S.k_odd)) %>%
  mutate(r_VA.k = (2 * r_VA.k) / (1 + r_VA.k))

data_scores$VAorient_S.k_splithalf <- splithalf$r_VA.k
###############################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
