## Set up ####
## Load packages
library(readr)
library(dplyr)
library(tidyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "VA4"

# Set Accuracy Criteria
sd.criteria <- 3.5

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(TrialProc=="real")

data_remove <- data_import %>%
  group_by(Subject) %>% 
  summarise(ACC.mean = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  center(variables = "ACC.mean", standardized = TRUE) %>%
  filter(ACC.mean > sd.criteria | ACC.mean < (-1*sd.criteria))

data_ready <- remove.save(data_import, remove = data_remove, save = here(output.dir, "remove"), taskname = "VA4")

## Score
data_VA4 <- data_ready %>%
  unite(col = set_resp_correct, c("SetSize", "Response", "Answer"), sep = "_") %>%
  group_by(Subject, set_resp_correct) %>%
  summarise(accuracy = n()) %>%
  ungroup() %>%
  group_by(Subject) %>%
  spread(key = set_resp_correct, value = accuracy, fill = 0) %>%
  rename(VA4_SetSize5_Hits = `5_6_6`,
         VA4_SetSize5_CR = `5_5_5`,
         VA4_SetSize5_FA = `5_6_5`,
         VA4_SetSize5_Miss = `5_5_6`,
         VA4_SetSize7_Hits = `7_6_6`,
         VA4_SetSize7_CR = `7_5_5`,
         VA4_SetSize7_FA = `7_6_5`,
         VA4_SetSize7_Miss = `7_5_6`) %>%
  rowwise() %>%
  mutate(Same_5_Trials.n = sum(c(VA4_SetSize5_CR, VA4_SetSize5_FA)),
         Different_5_Trials.n = sum(c(VA4_SetSize5_Hits, VA4_SetSize5_Miss)),
         Same_7_Trials.n = sum(c(VA4_SetSize7_CR, VA4_SetSize7_FA)),
         Different_7_Trials.n = sum(c(VA4_SetSize7_Hits, VA4_SetSize7_Miss)),
         VA4_SetSize5_Hits = VA4_SetSize5_Hits/Different_5_Trials.n,
         VA4_SetSize5_CR = VA4_SetSize5_CR/Same_5_Trials.n,
         VA4_SetSize5_FA = VA4_SetSize5_FA/Same_5_Trials.n,
         VA4_SetSize5_Miss = VA4_SetSize5_Miss/Different_5_Trials.n,
         VA4_SetSize7_Hits = VA4_SetSize7_Hits/Different_7_Trials.n,
         VA4_SetSize7_CR = VA4_SetSize7_CR/Same_7_Trials.n,
         VA4_SetSize7_FA = VA4_SetSize7_FA/Same_7_Trials.n,
         VA4_SetSize7_Miss = VA4_SetSize7_Miss/Different_7_Trials.n,
         VA4_k.5 = 5*(VA4_SetSize5_Hits + VA4_SetSize5_CR - 1), 
         VA4_k.7 = 7*(VA4_SetSize7_Hits + VA4_SetSize7_CR -1), 
         VA4_k.mean = (VA4_k.5+VA4_k.7)/2) %>%
  select(-Same_5_Trials.n, -Different_5_Trials.n, -Same_7_Trials.n, -Different_7_Trials.n)

data_VA4 <- mutate(data_VA4, VA4_k.mean = ifelse(VA4_k.mean<0, 0, VA4_k.mean))

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_VA4, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())

