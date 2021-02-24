#' Download template R scripts
#'
#' This function will download R script templates for tasks commonly used in the EngleLab
#' @param type String. Do you want to download "raw" or "score" task template files?
#' @param to a directory where to download R scripts
#' @param wmc Logical. Download scripts for the wmc taks?
#' @param ospan Logical. Download script for the operation span task?
#' @param symspan Logical. Download script for the symmetry span task?
#' @param rotspan Logical. Download script for the rotation span task?
#' @param ac Logical. Download scripts for all the attention taks?
#' @param antisaccade Logical. Download script for the antisaccade task?
#' @param stroop Logical. Download script for the Stroop task?
#' @param flanker Logical. Download script for the flanker task?
#' @param stroopDL Logical. Download script for the StroopDL task?
#' @param flankerDL Logical. Download script for the FlankerDL task?
#' @param visualarrays Logical. Download script for the Visual Arrays task?
#' @param sact Logical. Download script for the SACT task?
#' @param path String. Home directory file path
#' @export
#'

get_script <- function(type = "raw", to = "R Scripts", overwrite = FALSE,
                     wmc = FALSE, symspan = FALSE, ospan = FALSE, rotspan = FALSE,
                     ac = FALSE, antisaccade = FALSE, stroop = FALSE, flanker = FALSE,
                     stroopDL = FALSE, flankerDL = FALSE, visualarrays = FALSE,
                     sact = FALSE, path = "./"){

  ## Setup ####
  to <- paste(path, to, sep = "/")
  github_repo <-
    "https://raw.githubusercontent.com/EngleLab/englelab/master/scripts"

  if (!dir.exists(to)) dir.create(to)
  if (is.null(type)) type <- "none"

  if (wmc == TRUE) {
    symspan <- TRUE
    ospan <- TRUE
    rotspan <- TRUE
  }
  if (ac == TRUE) {
    antisaccade <- TRUE
    flanker <- TRUE
    stroop <- TRUE
    stroopDL <- TRUE
    flankerDL <- TRUE
    visualarrays <- TRUE
    sact <- TRUE
  }
  #####

  ## Download task templates
  if (symspan == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "symspan_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. symspan_raw.R already exists")
      } else {
        download.file(paste(github_repo, "symspan_raw.R", sep = "/"),
                      paste(to, "symspan_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "symspan_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. symspan_score.R already exists")
      } else {
        download.file(paste(github_repo, "symspan_score.R", sep = "/"),
                      paste(to, "symspan_score.R", sep = "/"))
      }
    }
  }

  if (rotspan == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "rotspan_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. rotspan_raw.R already exists")
      } else {
        download.file(paste(github_repo, "rotspan_raw.R", sep = "/"),
                      paste(to, "rotspan_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "rotspan_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. rotspan_score.R already exists")
      } else {
        download.file(paste(github_repo, "rotspan_score.R", sep = "/"),
                      paste(to, "rotspan_score.R", sep = "/"))
      }
    }
  }

  if (ospan == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "ospan_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. ospan_raw.R already exists")
      } else {
        download.file(paste(github_repo, "ospan_raw.R", sep = "/"),
                      paste(to, "ospan_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "ospan_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. ospan_score.R already exists")
      } else {
        download.file(paste(github_repo, "ospan_score.R", sep = "/"),
                      paste(to, "ospan_score.R", sep = "/"))
      }
    }
  }

  if (antisaccade == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "antisaccade_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. antisaccade_raw.R already exists")
      } else {
        download.file(paste(github_repo, "antisaccade_raw.R", sep = "/"),
                      paste(to, "antisaccade_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "antisaccade_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. antisaccade_score.R already exists")
      } else {
        download.file(paste(github_repo, "antisaccade_score.R", sep = "/"),
                      paste(to, "antisaccade_score.R", sep = "/"))
      }
    }
  }

  if (stroopDL == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "stroopDL_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. stroopDL_raw.R already exists")
      } else {
        download.file(paste(github_repo, "stroopDL_raw.R", sep = "/"),
                      paste(to, "stroopDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "stroopDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. stroopDL_score.R already exists")
      } else {
        download.file(paste(github_repo, "stroopDL_score.R", sep = "/"),
                      paste(to, "stroopDL_score.R", sep = "/"))
      }
    }
  }

  if (flankerDL == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "flankerDL_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. flankerDL_raw.R already exists")
      } else {
        download.file(paste(github_repo, "flankerDL_raw.R", sep = "/"),
                      paste(to, "flankerDL_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "flankerDL_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. flankerDL_score.R already exists")
      } else {
        download.file(paste(github_repo, "flankerDL_score.R", sep = "/"),
                      paste(to, "flankerDL_score.R", sep = "/"))
      }
    }
  }

  if (visualarrays == TRUE) {
    if (type == "raw" | type == "all") {
      exists <- file.exists(paste(to, "visualarrays_raw.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. visualarrays_raw.R already exists")
      } else {
        download.file(paste(github_repo, "visualarrays_raw.R", sep = "/"),
                      paste(to, "visualarrays_raw.R", sep = "/"))
      }
    }

    if (type == "score" | type == "all") {
      exists <- file.exists(paste(to, "visualarrays_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. visualarrays_score.R already exists")
      } else {
        download.file(paste(github_repo, "visualarrays_score.R", sep = "/"),
                      paste(to, "visualarrays_score.R", sep = "/"))
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
      exists <- file.exists(paste(to, "sact_score.R", sep = "/"))
      if (exists == TRUE & overwrite == FALSE) {
        message("Did not download file. sact_score.R already exists")
      } else {
        download.file(paste(github_repo, "sact_score.R", sep = "/"),
                      paste(to, "sact_score.R", sep = "/"))
      }
    }
  }
}
