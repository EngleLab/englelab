#' Setup directory and templates for a new research study
#'
#' This function can be used to automatically setup your study directory by
#' creating folders and template scripts
#' @param wmc_scripts Logical. Download WMC task template files?
#' @param ac_scripts Logical. Download all attention task template files?
#' @param path String. Home directory file path
#' @export
#'

create_project <- function(wmc_scripts = TRUE, ac_scripts = TRUE,
                           path = ".") {

  path <- paste(path, "/", sep = "")
  dir.create(paste(path, "R", sep = ""))
  dir.create(paste(path, "data", sep = ""))
  dir.create(paste(path, "data/raw", sep = ""))
  dir.create(paste(path, "data/raw/messy", sep = ""))
  dir.create(paste(path, "data/scored", sep = ""))
  dir.create(paste(path, "analyses", sep = ""))
  dir.create(paste(path, "analyses/exploratory", sep = ""))
  dir.create(paste(path, "analyses/figures", sep = ""))
  dir.create(paste(path, "tasks", sep = ""))

  get_template(to = "R/templates", path = path, overwrite = FALSE,
               main_script = TRUE, raw_script = TRUE, score_script = TRUE,
               merge_script = TRUE, analysis_script = TRUE,
               wmc_scripts = wmc_scripts, ac_scripts = ac_scripts)

  github_repo <-
    "https://raw.githubusercontent.com/EngleLab/englelab/master/script_templates"

  download.file(paste(github_repo, "_required_packages.R",
                      sep = ""),
                paste(path, "R/", "_required_packages.R", sep = ""))
}

