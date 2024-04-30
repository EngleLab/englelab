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
task <- "VA_Selective_Orientation"
import_file <- paste(task, "raw.csv", sep = "_")
output_file <- paste(task, "Scores.csv", sep = "_")

# set data cleaning parameters
acc_criterion <- .5
outlier_criterion <- 3.5
# ------------------------------------------------------------------------------

# Import Data ------------------------------------------------------------------
data_import <- read_csv(here(import_dir, import_file)) |>
  filter(TrialProc == "real")
# ------------------------------------------------------------------------------

# Score Data -------------------------------------------------------------------
data_scores <- data_import |>
  group_by(Subject, SetSize) |>
  score_visualarrays(taskname = "VAorient_S") |>
  ungroup()
# ------------------------------------------------------------------------------

# Clean Data -------------------------------------------------------------------
data_scores <- data_scores |>
  remove_problematic(remove = "VAorient_S.ACC <= acc_criterion", 
                     log_file = 
                       here("data/logs/vaorient_s_problematic.csv")) |>
  replace_outliers(variables = "VAorient_S.k", 
                   cutoff = outlier_criterion, with = "NA", pass = 2,
                   log_file = here("data/logs/vaorient_s_outliers.csv")) |>
  filter(!is.na(VAorient_S.k))
# ------------------------------------------------------------------------------

# Calculate Reliability --------------------------------------------------------
reliability <- data_import |>
  filter(Subject %in% data_scores$Subject)

splithalf <- reliability |>
  mutate(Trial = row_number(),
         Split = ifelse(Trial %% 2, "odd", "even")) |>
  group_by(Subject, Split, SetSize) |>
  score_visualarrays(taskname = "VAorient_S") |>
  rowwise() |>
  mutate(even = mean(c(VAorient_S_even_3.k, VAorient_S_even_5.k), na.rm = TRUE),
         odd = mean(c(VAorient_S_odd_3.k, VAorient_S_odd_5.k), na.rm = TRUE)) |>
  ungroup() |>
  summarise(r = cor(even, odd)) |>
  mutate(r = (2 * r) / (1 + r))

data_scores$VAorient_S.k.splithalf <- splithalf$r
# ------------------------------------------------------------------------------

# Save File --------------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
