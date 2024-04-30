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

# SymSpan ----------------------------------------------------------------------
# set import/output files
task <- "SymSpan"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL") |>
           mutate(across(starts_with("array"), ~ as.numeric(.x)))) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_symspan <- raw_symspan(data_import)

# save file
write_csv(data_symspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# RotSpan ----------------------------------------------------------------------
# set import/output files
task <- "RotSpan"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL") |>
           mutate(across(starts_with("array"), ~ as.numeric(.x)))) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_rotspan <- raw_rotspan(data_import)

# save file
write_csv(data_rotspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

# OSpan ----------------------------------------------------------------------
# set import/output files
task <- "OSpan"
output_file <- paste(task, "raw.csv", sep = "_")

# import data
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL") |>
           mutate(across(starts_with("array"), ~ as.numeric(.x)))) |>
  filter(Subject %in% subjlist$Subject)

# tidy data
data_ospan <- raw_ospan(data_import)

# save file
write_csv(data_symspan, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
