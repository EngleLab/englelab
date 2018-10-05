## Set up ####
## Load packages
library(readr)
library(dplyr)
library(here)
library(datawrangling)

## Set import/output directories
directories <- readRDS(here("directories.rds"))
import.dir <- directories$raw
output.dir <- directories$scored
##############

task <- "PVT"

## RT trimming criteria
rt.lowerLimit <- 200
rt.upperLimit <- 10000
sd.criteria <- 3.5

## Import Data
import.file <- paste(task, "raw.txt", sep = "_")
data_import <- read_delim(here(import.dir, import.file), "\t", escape_double = FALSE, trim_ws = TRUE)

## Score
data_import.real <- data_import %>% 
  filter(TrialProc=="real") %>%
  select(-Probe1Resp, -Probe2Resp, -Probe3Resp)

data_import.probe <- data_import %>%
  filter(TrialProc=="probe") %>%
  select(-RT)

data_pvt.real <- data_import.real %>%
  filter(!is.na(RT), RT >= rt.lowerLimit, RT <= rt.upperLimit) %>%
  trim(variables = "RT", context = "Subject", cutoff = sd.criteria, replace = "cutoff") %>%
  group_by(Subject) %>%
  mutate(Q20 = quantile(RT, probs = .80, na.rm = TRUE),
         Lapse = ifelse(!is.na(RT) & RT>=Q20, 1, 0),
         PVT_RT.mean = mean(RT, na.rm = TRUE),
         PVT_RT.sd = sd(RT, na.rm = TRUE))

data_pvt.probe <- data_import.probe %>%
  group_by(Subject) %>%
  mutate(OnTask = ifelse(Probe1Resp=="OnTask", 1, 0),
         TRI = ifelse(Probe1Resp=="TRI", 1, 0),
         ED = ifelse(Probe1Resp=="ED", 1, 0),
         MW = ifelse(Probe1Resp=="MW", 1, 0),
         InAtt = ifelse(Probe1Resp=="InAtt", 1, 0),
         PVT_OnTask.Prop = sum(OnTask, na.rm = TRUE)/n(),
         PVT_TRI.Prop = sum(TRI, na.rm = TRUE)/n(),
         PVT_ED.Prop = sum(ED, na.rm = TRUE)/n(),
         PVT_MW.Prop = sum(MW, na.rm = TRUE)/n(),
         PVT_InAtt.Prop = sum(InAtt, na.rm = TRUE)/n(),
         PVT_OverallP2.mean = mean(Probe2Resp, na.rm = TRUE),
         PVT_Arousal.mean = mean(Probe3Resp, na.rm = TRUE))

data_waitTime <- data_pvt.real %>% 
  group_by(Subject, WaitTime) %>%
  mutate(RT.mean = mean(RT, na.rm = TRUE),
         RT.sd = sd(RT, na.rm = TRUE)) %>%
  select(Subject, WaitTime, RT.mean, RT.sd) %>%
  ungroup() %>%
  distinct() %>%
  reshape(key = "WaitTime", values = c("RT.mean", "RT.sd"), by = "Subject")

colnames(data_waitTime)[which(colnames(data_waitTime)!="Subject")] <- paste("PVT", colnames(data_waitTime)[which(colnames(data_waitTime)!="Subject")], sep = "_")

data_lapse <- data_pvt.real %>% 
  group_by(Subject, Lapse) %>%
  mutate(RT.mean = mean(RT, na.rm = TRUE)) %>%
  select(Subject, Lapse, RT.mean) %>%
  ungroup() %>%
  distinct() %>%
  reshape(key = "Lapse", values = c("RT.mean"), by = "Subject") %>%
  select(Subject, PVT_RTQ20.mean = `1_RT.mean`)

data_mw <- data_pvt.probe %>% 
  group_by(Subject, Probe1Resp) %>%
  mutate(Probe2.mean = mean(Probe2Resp, na.rm = TRUE)) %>%
  select(Subject, Probe1Resp, Probe2.mean) %>%
  ungroup() %>%
  distinct() %>%
  reshape(key = "Probe1Resp", values = c("Probe2.mean"), by = "Subject")

colnames(data_mw)[which(colnames(data_mw)!="Subject")] <- paste("PVT", colnames(data_mw)[which(colnames(data_mw)!="Subject")], sep = "_")

data_pvt.real <- select(data_pvt.real, Subject, starts_with("PVT")) %>%
  distinct()

data_pvt.probe <- select(data_pvt.probe, Subject, starts_with("PVT")) %>%
  distinct()

data_pvt <- plyr::join_all(list(data.frame(data_pvt.real),
                                data.frame(data_pvt.probe),
                                data.frame(data_waitTime),
                                data.frame(data_lapse),
                                data.frame(data_mw)), by = "Subject", type = "full")

output.file <- paste(task, "Scores.txt", sep = "_")
write_delim(data_pvt, path = here(output.dir, output.file), delim = "\t", na = "")

rm(list=ls())
