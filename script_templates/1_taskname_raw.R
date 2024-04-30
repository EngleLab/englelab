# ---- Setup -------------------------------------------------------------------
# packages
library(here)
library(readr)
library(dplyr)
library(purrr) # delete if not importing a batch of files

# directories
import_dir <- "data/raw/messy"
output_dir <- "data/raw"

# file names
task <- "taskname"
import_file <- paste(task, ".txt", sep = "")
output_file <- paste(task, "raw.csv", sep = "_")
# ------------------------------------------------------------------------------

# ---- Import Data -------------------------------------------------------------
# to import a single file
data_import <- read_delim(here(import_dir, import_file), delim = "\t",
                          escape_double = FALSE, trim_ws = TRUE)

# alternatively to import a batch of files...
# change the arguments in purrr::map_df() depending on type of data files
# this example is for files created from eprime and needs encoding = "UCS-2LE"
files <- list.files(here(import_dir, task), pattern = ".txt", full.names = TRUE)
data_import <- files |>
  map_df(~ read_delim(.x, locale = locale(encoding = "UCS-2LE"), delim = "\t",
                      escape_double = FALSE, trim_ws = TRUE, na = "NULL"))
# ------------------------------------------------------------------------------

# ---- Tidy Data ---------------------------------------------------------------
data_raw <- data_import |>
  rename() |>
  filter() |>
  mutate() |>
  select()
# ------------------------------------------------------------------------------

# ---- Save Data ---------------------------------------------------------------
write_csv(data_raw, here(output_dir, output_file))
# ------------------------------------------------------------------------------

rm(list = ls())
