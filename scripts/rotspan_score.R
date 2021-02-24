## Set up ####
## Load packages
library(readr)
library(here)
library(dplyr)
library(englelab)

## Set import/output directories
import_dir <- "Data Files/Merged"
output_dir <- "Data Files"

## Set import/output files
task <- "RotSpan"
import_file <- paste(task, "_raw.csv", sep = "")
output_file <- paste(task, "_Scores.csv", sep = "")
##############

#### Import Data ####
data_import <- read_csv(here(import_dir, import_file))
#####################

#### Score Data ####
data_scores <- data_import %>%
  group_by(Subject) %>%
  score_rotspan()
####################

#### Clean Data ####
# remove problematic subjects based on processing task accuracy
# remove outliers based on span score
####################

#### Calculate Reliability ####
splithalf <- data_import %>%
  mutate(Split = ifelse(Trial %% 2, "odd", "even")) %>%
  group_by(Subject, Split) %>%
  score_rotspan() %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = contains("RotSpan")) %>%
  summarise(r_partial.load =
              cor(RotSpan.PartialLoad_odd, RotSpan.PartialLoad_even)) %>%
  mutate(r_partial.load =
           (2 * r_partial.load) / (1 + r_partial.load))

data_scores$RotSpan.PartialLoad_splithalf <- splithalf$r_partial.load

cronbachalpha <- data_import %>%
  distinct(Subject, Trial, .keep_all = TRUE) %>%
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = contains("Partial.load")) %>%
  alpha()

data_scores$RotSpan.PartialLoad_cronbachalpha <- cronbachalpha$total.std.alpha
###############################

#### Output ####
write_csv(data_scores, here(output_dir, output_file))
################

rm(list=ls())
