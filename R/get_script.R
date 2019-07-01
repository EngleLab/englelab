#' Download template R scripts
#'
#' This function will download R script templates for tasks commonly used in the EngleLab
#' @param type String. Do you want to download "raw" or "score" task template files?
#' @param to a directory where to download R scripts
#' @param gf Logical. Do you want to download template scripts for the gf taks?
#' @param wmc Logical. Do you want to download template scripts for the wmc taks?
#' @param ac Logical. Do you want to download template scripts for all the attention taks?
#' @param antisaccade Logical. Do you want to download a template file for the antisaccade task?
#' @param stroop Logical. Do you want to download a template file for the Stroop task?
#' @param stroopDL Logical. Do you want to download a template file for the Stroop task?
#' @param flanker Logical. Do you want to download a template file for the flanker task?
#' @param stroopDL Logical. Do you want to download a template file for the StroopDL task?
#' @param flankerDL Logical. Do you want to download a template file for the FlankerDL task?
#' @param va4 Logical. Do you want to download a template file for the Visual Arrays 4 task?
#' @param sact Logical. Do you want to download a template file for the SACT task?
#' @param path String. Home directory file path
#' @export
#'

get_script <- function(type = "all", to = "R Scripts", overwrite = FALSE,
                     gf = FALSE, wmc = FALSE, ac = FALSE,
                     antisaccade = FALSE, stroop = FALSE, flanker = FALSE,
                     stroopDL = FALSE, flankerDL = FALSE, va4 = FALSE,
                     sact = FALSE, path = "./"){

  ## Setup ####
  to <- paste(path, to, sep = "/")

  if (ac == TRUE) {
    antisaccade <- TRUE
    flanker <- TRUE
    stroop <- TRUE
    stroopDL <- TRUE
    flankerDL <- TRUE
    va4 <- TRUE
    sact <- TRUE
  }
  #####

  ## Download task templates
  if (gf == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_gf_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_gf_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/gf_raw.R",
                      paste(to, "0_gf_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_gf_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_gf_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/gf_score.R",
                      paste(to, "1_gf_score.R", sep = "/"))
      }
    }
  }

  if (wmc == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_wmc_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_wmc_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/wmc_raw.R",
                      paste(to, "0_wmc_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_wmc_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_wmc_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/wmc_score.R",
                      paste(to, "1_wmc_score.R", sep = "/"))
      }
    }
  }
  if (antisaccade == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_antisaccade_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_antisaccade_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/antisaccade_raw.R",
                      paste(to, "0_antisaccade_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_antisaccade_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_antisaccade_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/antisaccade_score.R",
                      paste(to, "1_antisaccade_score.R", sep = "/"))
      }
    }
  }

  if (stroop == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_stroop_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_stroop_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/stroop_raw.R",
                      paste(to, "0_stroop_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_stroop_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_stroop_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/stroop_score.R",
                      paste(to, "1_stroop_score.R", sep = "/"))
      }
    }
  }
  
  if (stroopDL == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_stroopDL_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_stroopDL_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/stroopDL_raw.R",
                      paste(to, "0_stroopDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_stroopDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_stroopDL_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/stroop_score.R",
                      paste(to, "1_stroopDL_score.R", sep = "/"))
      }
    }
  }

  if (flanker == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_flanker_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_flanker_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/flanker_raw.R",
                      paste(to, "0_flanker_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_flanker_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_flanker_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/flanker_score.R",
                      paste(to, "1_flanker_score.R", sep = "/"))
      }
    }
  }

  if (flankerDL == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_flankerDL_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_flankerDL_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/flankerDL_raw.R",
                      paste(to, "0_flankerDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_flankerDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_flankerDL_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/flankerDL_score.R",
                      paste(to, "1_flankerDL_score.R", sep = "/"))
      }
    }
  }

  if (va4 == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_va4_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_va4_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/va4_raw.R",
                      paste(to, "0_va4_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_va4_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_va4_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/va4_score.R",
                      paste(to, "1_va4_score.R", sep = "/"))
      }
    }
  }

  if (sact == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_sact_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_sact_raw.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/sact_raw.R",
                      paste(to, "0_sact_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_sact_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_sact_score.R already exists")
      } else {
        download.file("https://raw.githubusercontent.com/EngleLab/R-Templates/master/Tasks/sact_score.R",
                      paste(to, "1_sact_score.R", sep = "/"))
      }
    }
  }
}
