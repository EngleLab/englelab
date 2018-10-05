## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)
library(englelab)

## Set import/output directories
directories <- readRDS(here::here("directories.rds"))
import.dir <- directories$emerge
output.dir <- directories$raw
##############

## OSPAN ####
task <- "OSpan"
## Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data file and save
data_raw <- raw.ospan(data_import, blocks = 2) %>%
  group_by(Subject) %>%
  mutate(SetSize = ifelse(SetSize==9, 8, SetSize),
         OSpan.MathACC = sum(Math.correct, na.rm = TRUE)/sum(SetSize))

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

################
################

## SSPAN ####
task <- "SymSpan"
## Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data file and save
data_raw <- raw.symspan(data_import, blocks = 2) %>%
  filter(Trial<=12) %>%
  group_by(Subject) %>%
  mutate(SymSpan.SymmetryACC = sum(Symm.correct, na.rm = TRUE)/sum(SetSize))

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

################
################

## RotSPAN ####
task <- "RotSpan"
## Import and remove duplicate subjects
import.file <- paste(task, ".txt", sep = "")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  duplicates.remove(taskname = task, output.folder = here(output.dir, "duplicates"))

## Clean up raw data file and save
data_raw <- raw.rotspan(data_import, blocks = 2) %>%
  filter(Trial<=12) %>%
  group_by(Subject) %>%
  mutate(RotSpan.Partial = RotSpan.Partial_Block1 + RotSpan.Partial_Block2,
         RotSpan.RotationACC = sum(Rotations.correct, na.rm = TRUE)/sum(SetSize))

output.file <- paste(task, "raw.txt", sep = "_")
write_delim(data_raw, path = here(output.dir, output.file), "\t", na = "")

################
################

rm(list=ls())
