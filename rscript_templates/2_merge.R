## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(haven)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$scored
output.dir <- directories$data
##############

## Import Files
data_Gf <- read_delim(here(import.dir, "Gf_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(Subject, RAPM, LetterSets, NumberSeries)

data_WMC <- read_delim(here(import.dir, "WMC_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(Subject, OSpan = OSpan.Partial, SymSpan = SymSpan.Partial, RotSpan = RotSpan.Partial)

data_AC <- read_delim(here(import.dir, "Attention_Scores.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(Subject, 
         Antisaccade = Antisaccade_ACC.mean, 
         FlankerEffect = FlankerEffect_RT, FlankerBin, FlankerDL.score, FlankerPR.score, 
         StroopEffect = StroopEffect_RT, StroopBin, StroopDL.score,
         VA4 = VA4_k.mean, PVT_RTQ20 = PVT_RTQ20.mean, PVT_RT = PVT_RT.mean, PVT_SD = PVT_RT.sd,
         SACT = SACT_ACC.mean, CuedLineDetection = CuedLineDetection_Threshold.last4rev)

## Merge into one date frame
data_all <- plyr::join_all(list(data_Gf, data_WMC, data_AC), by = "Subject", type = "full")

## Calculate composites
data_all <- data_all %>%
  composite(variables = c("OSpan", "SymSpan", "RotSpan"), standardized = TRUE, comp = "mean",
                   name = "WMC", missing.criteria = 2) %>%
  composite(variables = c("RAPM", "NumberSeries", "LetterSets"), standardized = TRUE, comp = "mean",
                   name = "Gf", missing.criteria = 2)

## Save Data
data_all[which(sapply(data_all, FUN = typeof)=="double")] <- round(data_all[which(sapply(data_all, FUN = typeof)=="double")], digits = 3)

write.table(data_all, file = here(output.dir, "SWARM.txt"), sep = "\t", row.names = FALSE, quote = FALSE, na = "")

## Trimmed data ####
data_all <- select(data_all, -Gf, -WMC)
variables <- colnames(data_all)[which(!(colnames(data_all)%in%c("Subject")))]
data_all <- data_all %>%
  trim(variables = variables, cutoff = 3.5) %>%
  composite(variables = c("OSpan", "SymSpan", "RotSpan"), standardized = TRUE, comp = "mean",
            name = "WMC", missing.criteria = 2) %>%
  composite(variables = c("RAPM", "NumberSeries", "LetterSets"), standardized = TRUE, comp = "mean",
            name = "Gf", missing.criteria = 2)

## Save data
data_all[which(sapply(data_all, FUN = typeof)=="double")] <- round(data_all[which(sapply(data_all, FUN = typeof)=="double")], digits = 3)
write.table(data_all, file = here(output.dir, "SWARM_trimmed.txt"), sep = "\t", row.names = FALSE, quote = FALSE, na = "")
write_sav(data_all, path = here(output.dir, "SWARM_trimmed.sav"))

rm(list=ls())

