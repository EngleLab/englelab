## Setup ####
## Load Packages
library(here)

## Specify the directory tree
directories <- list(scripts = "Behavioral Data/R Scripts",
                    data = "Analyses/Data Files",
                    raw = "Analyses/Data Files/Raw Data",
                    emerge = "Analyses/Data Files/Raw Data/E-Merge",
                    scored = "Analyses/Data Files/Scored Data")

saveRDS(directories, here("directories.rds"))
#############

## Install the latest versions of these packages ####
# uncomment line to install
#devtools::install_github("dr-JT/datawrangling")
#devtools::install_github("dr-JT/englelab")
#####################################################

############################
#------ Data Scoring ------#
############################

## Create raw data file from e-merged files ####
source(here(directories$scripts, "1_wmc_raw.R"), echo=TRUE)
source(here(directories$scripts, "1_gf_raw.R"), echo=TRUE)
################################################

## Score task data from raw data files ####
source(here(directories$scripts, "2_wmc_score.R"), echo=TRUE)
source(here(directories$scripts, "2_gf_score.R"), echo=TRUE)
###########################################

## Create single merged data file of Gtasks ####
source(here(directories$scripts, "2_merge.R"), echo=TRUE)
###################################################


#####################################
#------ Descriptives Analysis ------#
#####################################

library(rmarkdown)

#rmarkdown::render("./Data Files/Scripts/Figures_Descriptives.Rmd", output_dir = "./Data Files")
#rmarkdown::render("./Data Files/Scripts/SWARM Reliability.Rmd", output_dir = "./Data Files")


## Packages that need to be installed ####
#
# dplyr
# readr
# tidyr
# here
# plyr
# devtools
# haven
# rmarkdown
#
# datawrangling * from GitHub devtools::install_github("dr-JT/datawrangling")
# englelab * from GitHub devtools::install_github("dr-JT/englelab")
#
#
# To install packages: install.packages("packagename") or install.packages(c("packagename1", "packagename2", "etc"))
##########################################
