# ---- Setup -------------------------------------------------------------------
# packages
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# directories
import_dir <- "data/scored"
output_dir <- "data"

# file names
output_scores <- "TaskScores.csv"
output_reliabilities <- "Reliabilities.csv"
output_admintimes <- "AdminTimes.csv"
output_datacleaning <- "DataCleaning_log.csv"
# ------------------------------------------------------------------------------

# ---- Import Data -------------------------------------------------------------
files <- list.files(here(import_dir), pattern = "Scores", full.names = TRUE)
data_import <- files |>
  map(read_csv) |>
  reduce(full_join, by = "Subject")
# ------------------------------------------------------------------------------

# ---- Select Variables --------------------------------------------------------
data_scores <- data_import |>
  select(Subject) |>
  filter()

# list of final subjects
subjlist <- select(data_scores, Subject)
# ------------------------------------------------------------------------------

# ---- Reliabilities -----------------------------------------------------------
data_reliabilities <- data_import |>
  select(contains("splithalf"), contains("cronbach_alpha")) |>
  drop_na() |>
  distinct() |>
  pivot_longer(everything(),
               names_to = c("Task", "metric"),
               names_pattern = "(\\w+.\\w+).(\\w+)",
               values_to = "value") |>
  pivot_wider(id_col = Task,
              names_from = metric,
              values_from = value)
# ------------------------------------------------------------------------------

# ---- Admin Times -------------------------------------------------------------
data_merge <- data_import |>
  select(contains("AdminTime")) |>
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) |>
  pivot_longer(everything(),
               names_to = c("Task", "metric"),
               names_pattern = "(\\w+.\\w+).(\\w+)",
               values_to = "value") |>
  mutate(value = round(value, 3)) |>
  pivot_wider(id_col = Task,
              names_from = metric,
              names_prefix = "AdminTime.",
              values_from = "value")
# ------------------------------------------------------------------------------

# ---- Data Cleaning Log -------------------------------------------------------
# problematic subjects
data_problematic <- list.files(here("data/logs"), 
                                 pattern = "problematic", 
                                 full.names = TRUE) %>%
  map(read_csv) |>
  map(function(x) {
    reframe(x, tibble(Task = gsub("\\..*", "", colnames(select(x, 2))), 
                       Problematic_Removed = nrow(x)))
  }) |>
  bind_rows()

# outliers
data_outliers <- list.files(here("data/logs"), 
                                 pattern = "outliers", 
                                 full.names = TRUE) %>%
  map(read_csv) |>
  map(function(x) {
    reframe(x, tibble(Task = gsub("\\..*", "", colnames(select(x, 2))),
                      Outliers_Removed = nrow(x),
                      Outliers_Passes = max(pull(x, Pass))))
  }) |>
  bind_rows()

# merge
data_log <- merge(data_problematic, data_outliers, by = "Task", all = TRUE)
# ------------------------------------------------------------------------------

# ---- Save Data ---------------------------------------------------------------
write_csv(data_scores, here(output_dir, output_scores))
write_csv(data_reliabilities, here(output_dir, output_reliabilities))
write_csv(data_reliabilities, here(output_dir, output_admintimes))
write_csv(data_log, here(output_dir, output_datacleaning))
write_csv(subjlist, here(output_dir, "subjlist_final.csv"))
# ------------------------------------------------------------------------------

rm(list = ls())
