## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)

## Set import/output directories
import_dir <- "Data Files/Raw Data"
output_dir <- "Data Files/Scored Data"

## Set import/output files
task <- "Antisaccade"
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
  summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE),
            Antisaccade.RT = mean(RT, na.rm = TRUE),
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
  summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE)) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = "Antisaccade.ACC") %>%
  summarise(r_antisaccade.acc =
              cor(Antisaccade.acc_odd, Antisaccade.acc_even)) %>%
  mutate(r_antisaccade.acc =
           (2 * r_antisaccade.acc) / (1 + r_antisaccade.acc))

data_scores$Antisaccade.ACC_splithalf <- splithalf$r_antisaccade.acc

cronbachalpha <- data_import %>%
  distinct(Subject, Trial, .keep_all = TRUE) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = "Accuracy") %>%
  alpha()

data_scores$Antisaccade.acc_cronbachalpha <- cronbachalpha$total.std.alpha
###############################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
