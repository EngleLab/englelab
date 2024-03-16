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

# SymSpan ----------------------------------------------------------------------
# set import/output files
task <- "SymSpan"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t",
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_symspan <- raw_symspan(data_import)

# save file
write_csv(data_symspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# RotSpan ----------------------------------------------------------------------
# set import/output files
task <- "RotSpan"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t",
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_rotspan <- raw_rotspan(data_import)

# save file
write_csv(data_rotspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# OSpan ----------------------------------------------------------------------
# set import/output files
task <- "OSpan"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")

# import data
data_import <- read_delim(here(import_dir, import_file), "\t",
                          escape_double = FALSE, trim_ws = TRUE,
                          guess_max = 10000) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_ospan <- raw_ospan(data_import)

# save file
write_csv(data_symspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
