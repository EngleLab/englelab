## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set import/output files
task <- "SACT"
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
  summarise(SACT.acc = mean(Accuracy, na.rm = TRUE),
            AdminTime = first(AdminTime),
            SessionDate = first(SessionDate),
            SessionTime = first(SessionTime))
####################

#### Clean Data ####
# remove problematic subjects based on some criteria
# remove outliers based on accuracy
####################

#### Calculate Reliability ####
splithalf <- data_import %>%
  group_by(Subject) %>%
  mutate(Split = ifelse(Trial %% 2, "odd", "even")) %>%
  group_by(Subject, Split) %>%
  summarise(SACT.acc = mean(Accuracy, na.rm = TRUE)) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = "SACT.acc") %>%
  summarise(r_sact.acc =
              cor(SACT.acc_odd, SACT.acc_even)) %>%
  mutate(r_sact.acc =
           (2 * r_sact.acc) / (1 + r_sact.acc))

data_scores$SACT.acc_splithalf <- splithalf$r_sact.acc

cronbachalpha <- data_import %>%
  distinct(Subject, Trial, .keep_all = TRUE) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = "Accuracy") %>%
  alpha()

data_scores$SACT.acc_cronbachalpha <- cronbachalpha$total.std.alpha
###############################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
