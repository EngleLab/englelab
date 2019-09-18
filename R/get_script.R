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
#' @param flanker Logical. Do you want to download a template file for the flanker task?
#' @param stroopDL Logical. Do you want to download a template file for the StroopDL task?
#' @param flankerDL Logical. Do you want to download a template file for the FlankerDL task?
#' @param va4 Logical. Do you want to download a template file for the Visual Arrays 4 task?
#' @param sact Logical. Do you want to download a template file for the SACT task?
#' @param path String. Home directory file path
#' @export
#'

get_script <- function(type = "all", to = "R Scripts", overwrite = FALSE,
                     wmc = FALSE, symspan = FALSE, ospan = FALSE, rotspan = FALSE,
                     gf = FALSE, rapm = FALSE, numberseries = FALSE, lettersets = FALSE,
                     ac = FALSE, antisaccade = FALSE, stroop = FALSE, flanker = FALSE,
                     stroopDL = FALSE, flankerDL = FALSE, va4 = FALSE,
                     sact = FALSE, path = "./"){

  ## Setup ####
  to <- paste(path, to, sep = "/")
  github_repo <-
    "https://raw.githubusercontent.com/EngleLab/englelab/master/scripts"

  if (!dir.exists(to)) dir.create(to)
  if (is.null(type)) type <- "none"

  if (wmc == TRUE & type != "score") {
    symspan <- TRUE
    ospan <- TRUE
    rotspan <- TRUE
  }
  if (gf == TRUE & type != "score") {
    rapm <- TRUE
    numberseries <- TRUE
    lettersets <- TRUE
  }
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
  if (symspan == TRUE) {
    exists <- file.exists(paste(to, "0_symspan_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_symspan_raw.R already exists")
    } else {
      download.file(paste(github_repo, "symspan_raw.R", sep = "/"),
                    paste(to, "0_symspan_raw.R", sep = "/"))
    }
  }
  if (ospan == TRUE) {
    exists <- file.exists(paste(to, "0_ospan_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_ospan_raw.R already exists")
    } else {
      download.file(paste(github_repo, "ospan_raw.R", sep = "/"),
                    paste(to, "0_ospan_raw.R", sep = "/"))
    }
  }
  if (rotspan == TRUE) {
    exists <- file.exists(paste(to, "0_rotspan_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_rotspan_raw.R already exists")
    } else {
      download.file(paste(github_repo, "rotspan_raw.R", sep = "/"),
                    paste(to, "0_rotspan_raw.R", sep = "/"))
    }
  }
  if (wmc == TRUE) {
    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_wmc_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_wmc_score.R already exists")
      } else {
        download.file(paste(github_repo, "wmc_score.R", sep = "/"),
                      paste(to, "1_wmc_score.R", sep = "/"))
      }
    }
  }

  if (rapm == TRUE) {
    exists <- file.exists(paste(to, "0_rapm_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_rapm_raw.R already exists")
    } else {
      download.file(paste(github_repo, "rapm_raw.R", sep = "/"),
                    paste(to, "0_rapm_raw.R", sep = "/"))
    }
  }
  if (numberseries == TRUE) {
    exists <- file.exists(paste(to, "0_numberseries_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_numberseries_raw.R already exists")
    } else {
      download.file(paste(github_repo, "numberseries_raw.R", sep = "/"),
                    paste(to, "0_numberseries_raw.R", sep = "/"))
    }
  }
  if (lettersets == TRUE) {
    exists <- file.exists(paste(to, "0_lettersets_raw.R", sep = "/"))
    if (exists == TRUE & overwrite == FALSE) {
      message("Did not download file. 0_lettersets_raw.R already exists")
    } else {
      download.file(paste(github_repo, "lettersets_raw.R", sep = "/"),
                    paste(to, "0_lettersets_raw.R", sep = "/"))
    }
  }
  if (gf == TRUE) {
    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_gf_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_gf_score.R already exists")
      } else {
        download.file(paste(github_repo, "gf_score.R", sep = "/"),
                      paste(to, "1_gf_score.R", sep = "/"))
      }
    }
  }

  if (antisaccade == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "0_antisaccade_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 0_antisaccade_raw.R already exists")
      } else {
        download.file(paste(github_repo, "antisaccade_raw.R", sep = "/"),
                      paste(to, "0_antisaccade_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_antisaccade_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_antisaccade_score.R already exists")
      } else {
        download.file(paste(github_repo, "antisaccade_score.R", sep = "/"),
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
        download.file(paste(github_repo, "stroop_raw.R", sep = "/"),
                      paste(to, "0_stroop_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_stroop_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_stroop_score.R already exists")
      } else {
        download.file(paste(github_repo, "stroop_score.R", sep = "/"),
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
        download.file(paste(github_repo, "stroopDL_raw.R", sep = "/"),
                      paste(to, "0_stroopDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_stroopDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_stroopDL_score.R already exists")
      } else {
        download.file(paste(github_repo, "stroopDL_score.R", sep = "/"),
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
        download.file(paste(github_repo, "flanker_raw.R", sep = "/"),
                      paste(to, "0_flanker_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_flanker_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_flanker_score.R already exists")
      } else {
        download.file(paste(github_repo, "flanker_score.R", sep = "/"),
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
        download.file(paste(github_repo, "flankerDL_raw.R", sep = "/"),
                      paste(to, "0_flankerDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_flankerDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_flankerDL_score.R already exists")
      } else {
        download.file(paste(github_repo, "flankerDL_score.R", sep = "/"),
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
        download.file(paste(github_repo, "va4_raw.R", sep = "/"),
                      paste(to, "0_va4_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_va4_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_va4_score.R already exists")
      } else {
        download.file(paste(github_repo, "va4_score.R", sep = "/"),
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
        download.file(paste(github_repo, "sact_raw.R", sep = "/"),
                      paste(to, "0_sact_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "1_sact_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. 1_sact_score.R already exists")
      } else {
        download.file(paste(github_repo, "sact_score.R", sep = "/"),
                      paste(to, "1_sact_score.R", sep = "/"))
      }
    }
  }
}
