# Set up -----------------------------------------------------------------------
# load packages
library(here)
library(readr)
library(dplyr)
library(purrr)
library(englelab)

# set import/output directories
import_dir <- "data/raw/messy"
output_dir <- "data/raw"

# completed subjects
subjlist <- read_csv(here("data", "subjlist_completed.csv"))
# ------------------------------------------------------------------------------

# Antisaccade ------------------------------------------------------------------
# set import/output files
task <- "Antisaccade"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL")) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_ac <- raw_antisaccade(data_import)

# save file
write_csv(data_ac, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# Visual Arrays ----------------------------------------------------------------
# set import/output files
task <- "VA_Selective_Orientation"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL")) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_va <- raw_visualarrays(data_import)

# save file
write_csv(data_va, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# StroopDL-2 -------------------------------------------------------------------
# set import/output files
task <- "StroopDL_v2"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL")) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_stroop <- raw_stroopDL(data_import)

# save file
write_csv(data_stroop, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# SACT-2 -----------------------------------------------------------------------
# set import/output files
task <- "SACT_v2"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL")) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_sact <- raw_sact(data_import)

# save file
write_csv(data_sact, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
