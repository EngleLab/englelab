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
task <- "OSpan"
import_file <- paste(task, "raw.csv", sep = "_")
output_file <- paste(task, "Scores.csv", sep = "_")

# set data cleaning parameters
acc_criterion <- .50
outlier_criterion <- 3.5
# ------------------------------------------------------------------------------

# Import Data ------------------------------------------------------------------
data_import <- read_csv(here(import_dir, import_file))
# ------------------------------------------------------------------------------

# Score Data -------------------------------------------------------------------
data_scores <- data_import |>
  group_by(Subject) |>
  score_ospan() |>
  ungroup()
# ------------------------------------------------------------------------------

# Clean Data -------------------------------------------------------------------
data_scores <- data_scores |>
  remove_problematic(remove = "Operation.ACC <= acc_criterion",
                     log_file = here("data/logs/ospan_problematic.csv")) |>
  replace_outliers(variables = "OSpan.EditDistanceScore",
                   cutoff = outlier_criterion, with = "NA", pass = 2,
                   log_file = here("data/logs/ospan_outliers.csv")) |>
  filter(!is.na(OSpan.EditDistanceScore))
# ------------------------------------------------------------------------------

# Calculate Reliability --------------------------------------------------------
reliability <- data_import |>
  filter(Subject %in% data_scores$Subject)

splithalf <- reliability |>
  group_by(Subject) |>
  mutate(Split = ifelse(Trial %% 2, "odd", "even")) |>
  group_by(Subject, Split) |>
  score_ospan() |>
  pivot_wider(id_cols = "Subject",
              names_from = "Split",
              values_from = contains("OSpan")) |>
  ungroup() |>
  summarise(r_partial =
              cor(OSpan.PartialScore_odd, OSpan.PartialScore_even),
            r_editdistance =
              cor(OSpan.EditDistanceScore_odd,
                  OSpan.EditDistanceScore_even)) |>
  mutate(r_partial = (2 * r_partial) / (1 + r_partial),
         r_editdistance = (2 * r_editdistance) / (1 + r_editdistance))

data_scores$OSpan.PartialScore.splithalf <- splithalf$r_partial
data_scores$OSpan.EditDistanceScore.splithalf <- splithalf$r_editdistance

cronbachalpha_partial <- reliability |>
  distinct(Subject, Trial, .keep_all = TRUE) |>
  arrange(Subject, Block, SetSize) |>
  mutate(.by = Subject, Trial = row_number()) |>
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = contains("Partial.load")) |>
  alpha()

data_scores$OSpan.PartialScore.cronbachalpha <-
  cronbachalpha_partial$total$std.alpha

cronbachalpha_editdistance <- reliability |>
  distinct(Subject, Trial, .keep_all = TRUE) |>
  arrange(Subject, Block, SetSize) |>
  mutate(.by = Subject, Trial = row_number()) |>
  pivot_wider(id_cols = "Subject",
              names_from = "Trial",
              values_from = contains("EditDistance.load")) |>
  alpha()

data_scores$OSpan.EditDistanceScore.cronbachalpha <-
  cronbachalpha_editdistance$total$std.alpha
# ------------------------------------------------------------------------------

# Save File --------------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
