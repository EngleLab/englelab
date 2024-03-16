# Set up -----------------------------------------------------------------------
# load packages
library(here)
library(readr)
library(dplyr)
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
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_ac <- raw_antisaccade(data_import)

# save file
write_csv(data_ac, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# Visual Arrays ----------------------------------------------------------------
# set import/output files
task <- "VA_Selective_Orientation"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_va <- raw_visualarrays(data_import)

# save file
write_csv(data_va, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# StroopDL-2 -------------------------------------------------------------------
# set import/output files
task <- "StroopDL_v2"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_stroop <- raw_stroopDL(data_import)

# save file
write_csv(data_stroop, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# SACT-2 -----------------------------------------------------------------------
# set import/output files
task <- "SACT_v2"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t", 
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_sact <- raw_sact(data_import)

# save file
write_csv(data_sact, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
