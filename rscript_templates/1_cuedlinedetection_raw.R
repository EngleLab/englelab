preserve <- ls()
## Set these Variables ####
import.dir <- "../../Tasks/Session 3/5. ACT"
output.dir <- raw.dir
###########################

## Load packages ####
library(readr)
library(dplyr)
library(datawrangling)
#####################

## Import Data
filelist <- list.files(path = import.dir, pattern = "Staircase", full.names = TRUE)
data_import <- list()
j <- 0
for (i in 1:length(filelist)){
  data_import[[i-j]] <- read_delim(filelist[[i]], "\t", escape_double = FALSE, trim_ws = TRUE)
  row1 <- data_import[[i-j]]$Subject[1]
  trials <- max(data_import[[i-j]]$Trial, na.rm = TRUE)
  if (is.na(row1) | row1=="Subject" | is.na(trials) | trials< 72){
    j <- j + 1
  }
}
data_trial <- bind_rows(data_import) %>%
  filter(Subject!=14257)

filelist <- list.files(path = import.dir, pattern = "AttentionCue_Scores*", full.names = TRUE)
data_import <- list()
for (i in 1:length(filelist)){
  data_import[[i]] <- read_delim(filelist[[i]], "\t", escape_double = FALSE, trim_ws = TRUE)
}
data_score <- bind_rows(data_import) %>%
  distinct() %>%
  filter(Subject!=14257)

data_raw <- left_join(data_trial, data_score, by = "Subject") %>%
  filter(Subject %in% subj.list$Subject)

write_delim(data_raw, path = paste(raw.dir, "/CuedLineDetection_raw.txt", sep = ""), delim="\t", na="")

rm(list=ls()[which(!(ls() %in% preserve))])

rm(list=ls())