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
task <- "SACT_v2"
import_file <- paste(task, "raw.csv", sep = "_")
output_file <- paste(task, "Scores.csv", sep = "_")

# set data cleaning parameters
acc_criterion <- 1/3
outlier_criterion <- 3.5
# ------------------------------------------------------------------------------

# Import Data ------------------------------------------------------------------
data_import <- read_csv(here(import_dir, import_file)) |>
  filter(TrialProc == "real") |>
  mutate(WaitTime = ifelse(WaitTime == 17, 0, WaitTime))
# ------------------------------------------------------------------------------

# Score Data -------------------------------------------------------------------
data_waittime <- data_import |>
  group_by(Subject, WaitTime) |>
  summarise(ACC = mean(Accuracy, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(id_cols = "Subject",
              names_from = "WaitTime",
              names_glue = "SACT_{WaitTime}.{.value}",
              values_from = "ACC")

data_overall <- data_import |>
  group_by(Subject) |>
  summarise(SACT.ACC = mean(Accuracy, na.rm = TRUE),
            SACT.AdminTime = first(AdminTime)) |>
  ungroup()

data_scores <- merge(data_overall, data_waittime, by = "Subject")
# ------------------------------------------------------------------------------

# Clean Data -------------------------------------------------------------------
data_scores <- data_scores |>
  remove_problematic(remove = "SACT.ACC <= acc_criterion", 
                     log_file = here("data/logs/sact_problematic.csv")) |>
  replace_outliers(variables = "SACT.ACC", 
                   cutoff = outlier_criterion, with = "NA", pass = 2,
                   log_file = here("data/logs/sact_outliers.csv")) |>
  filter(!is.na(SACT.ACC))
# ------------------------------------------------------------------------------

# Calculate Reliability --------------------------------------------------------
reliability <- data_import |>
  filter(Subject %in% data_scores$Subject,
         WaitTime != 0) |>
  group_by(Subject) |>
  mutate(Trial = row_number()) |>
  ungroup()

splithalf <- reliability |>
  group_by(Subject) |>
  mutate(Split = ifelse(Trial %% 2, "odd", "even")) |>
  group_by(Subject, Split) |>
  summarise(SACT.ACC = mean(Accuracy, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = "SACT.ACC") |>
  summarise(r = cor(even, odd, use = "pairwise.complete.obs")) |>
  mutate(r = (2 * r) / (1 + r))

data_scores$SACT.ACC.splithalf <- splithalf$r

cronbachalpha <- reliability |>
  select(Subject, Trial, Accuracy) |>
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = "Accuracy") |>
  select(-Subject) |>
  alpha()

data_scores$SACT.ACC.cronbachalpha <- cronbachalpha$total$std.alpha

data_scores <- relocate(data_scores, SACT.ACC.splithalf, SACT.ACC.cronbachalpha,
                        .after = SACT.ACC)
# ------------------------------------------------------------------------------

# Save File --------------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
