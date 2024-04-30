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
task <- "Antisaccade"
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
  group_by(Subject) |>
  summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE), 
            Antisaccade.RT_mean = mean(RT, na.rm = TRUE),
            Antisaccade.RT_sd = sd(RT, na.rm = TRUE),
            Trials = n()) |>
  ungroup()
# ------------------------------------------------------------------------------

# Clean Data -------------------------------------------------------------------
data_scores <- data_scores |>
  remove_problematic(remove = "Antisaccade.ACC <= acc_criterion", 
                     log_file = here("data/logs/antisaccade_problematic.csv")) |>
  replace_outliers(variables = "Antisaccade.ACC", 
                   cutoff = outlier_criterion, with = "NA", pass = 2,
                   log_file = here("data/logs/antisaccade_outliers.csv")) |>
  filter(!is.na(Antisaccade.ACC))
# ------------------------------------------------------------------------------

# Calculate Reliability --------------------------------------------------------
reliability <- data_import |>
  filter(Subject %in% data_scores$Subject)

splithalf <- reliability |>
  mutate(Trial = row_number(),
         Split = ifelse(Trial %% 2, "odd", "even")) |>
  group_by(Subject, Split) |>
  summarise(Antisaccade.ACC = mean(Accuracy, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = "Antisaccade.ACC") |>
  summarise(r = cor(even, odd)) |>
  mutate(r = (2 * r) / (1 + r))

data_scores$Antisaccade.ACC.splithalf <- splithalf$r

cronbachalpha <- reliability |>
  select(Subject, Trial, Accuracy) |>
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = "Accuracy") |>
  select(-Subject) |>
  alpha()

data_scores$Antisaccade.ACC.cronbachalpha <- cronbachalpha$total$std.alpha
# ------------------------------------------------------------------------------

# Save File --------------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
