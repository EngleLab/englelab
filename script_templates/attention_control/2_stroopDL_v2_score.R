# Set Up -----------------------------------------------------------------------
# load packages
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(englelab)
library(psych)      # for cronbach's alpha

# set import/output directories
import_dir <- "data/raw"
output_dir <- "data/scored"

# set import/output file names
task <- "StroopDL_v2"
import_file <- paste(task, "raw.csv", sep = "_")
output_file <- paste(task, "Scores.csv", sep = "_")

# set data cleaning parameters
acc_criterion <- 1/3
outlier_criterion <- 3.5
rt_criterion <- 200
# ------------------------------------------------------------------------------

# Import Data ------------------------------------------------------------------
data_import <- read_csv(here(import_dir, import_file), guess_max = 10000) |>
  filter(TrialProc == "real")
# ------------------------------------------------------------------------------

# Score Data -------------------------------------------------------------------
data_scores <- score_stroopDL(data_import, 
                              scoring_method = c("Last 4 Reversals",
                                                 "Last 8 Reversals",
                                                 "Last 10 Reversals",
                                                 "Last Trial",
                                                 "All Reversals",
                                                 "Overall Median"),
                              rt_cutoff = rt_criterion)
# ------------------------------------------------------------------------------

# Clean Data -------------------------------------------------------------------
data_scores <- data_scores |>
  remove_problematic(remove = "StroopDL_Overall.ACC <= acc_criterion", 
                     log_file = here("data/logs/stroopDL_problematic.csv")) |>
  replace_outliers(variables = "StroopDL_Last4Rev.ResponseDeadline", 
                   cutoff = outlier_criterion, with = "NA", pass = 2,
                   log_file = here("data/logs/stroopDL_outliers.csv")) |>
  filter(!is.na(StroopDL_Last4Rev.ResponseDeadline))
# ------------------------------------------------------------------------------

# Calculate Reliability --------------------------------------------------------
reliability <- data_import |>
  filter(Subject %in% data_scores$Subject) |>
  group_by(Subject) |>
  mutate(Trial_New = as.numeric(row_number()),
         var_last_reversals = 
           case_when(
             ReversalNumber >= max(ReversalNumber, na.rm = TRUE) - (4 - 1) ~ 
               ResponseDeadline, 
             TRUE ~ as.numeric(NA)),
         last_reversals_firsttrial = 
           case_when(
             ReversalNumber == max(ReversalNumber, na.rm = TRUE) - (4 - 1) ~ 
               Trial_New, 
             TRUE ~ as.numeric(NA)),
         last_reversals_lasttrial = 
           case_when(ReversalNumber == max(ReversalNumber, na.rm = TRUE) ~
                       Trial_New,
                     TRUE ~ as.numeric(NA))) |>
  filter(!is.na(var_last_reversals))

cronbachalpha <- reliability |>
  group_by(Subject) |>
  mutate(Trial = row_number()) |>
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = "var_last_reversals") |>
  ungroup() |>
  psych::alpha()

data_scores$StroopDL_Last4Rev.ResponseDeadline.cronbachalpha <- 
  cronbachalpha$total$std.alpha
# ------------------------------------------------------------------------------

# Save File --------------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
