project_template <- function(path, wmc_scripts, ac_scripts) {
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create project
  create_project(path = path, main_script = main_script, raw_script = raw_script,
                 score_script = score_script, merge_script = merge_script,
                 analysis_script = analysis_script,
                 data_raw = data_raw, data_scored = data_scored,
                 documents = documents, analyses = analyses, tasks = tasks)
}
