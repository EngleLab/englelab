#' Calculates a score for adaptive threshold procedures
#'
#'
#' @param x dataframe
#' @param taskname string to add as a prefix to scored columns
#' @param adaptive_variable the column name that represents the variable that
#'     is changing across the adaptive procedure (e.g., stimulus duration).
#' @param scoring_method the scoring method to use. Default: Last 4 Reversals.
#'     Options are:
#'     Last n Reversals (substitute n for a digit, e.g., Last 4 Reversals. This
#'     represents the median of the last n reversals);
#'     Last Trial (the value of the adaptive variable on the last trial);
#'     All Reversals (median of all reversals);
#'     Overall Median (median across all trials)
#' @export
#'

score_adaptivethreshold <- function(x, taskname = "",
                                    adaptive_variable = "",
                                    scoring_method = "Last 4 Reversals") {

  if (stringr::str_detect(scoring_method, "[0-9]+")) {
    last_n <- as.numeric(stringr::str_extract(scoring_method, "[0-9]+"))
    x <- dplyr::filter(x, !is.na(ReversalMade))
    x <- dplyr::mutate(x,
                       Trial_New = as.numeric(dplyr::row_number()),
                       var_last_reversals =
                         dplyr::case_when(
                           ReversalNumber >=
                             max(ReversalNumber, na.rm = TRUE) - (last_n - 1) ~
                             get(adaptive_variable),
                           TRUE ~ as.numeric(NA)),
                       last_reversals_firsttrial =
                         dplyr::case_when(
                           ReversalNumber ==
                             max(ReversalNumber, na.rm = TRUE) - (last_n - 1) ~
                             Trial_New,
                           TRUE ~ as.numeric(NA)),
                       last_reversals_lasttrial =
                         dplyr::case_when(
                           ReversalNumber ==
                             max(ReversalNumber, na.rm = TRUE) ~
                             Trial_New,
                           TRUE ~ as.numeric(NA)))
    x <- tidyr::fill(x, last_reversals_firsttrial, last_reversals_lasttrial, .direction = "updown")
    x <- dplyr::mutate(x,
                       last_reversals_trials =
                         ifelse(Trial_New >= last_reversals_firsttrial &
                                  Trial_New <= last_reversals_lasttrial, 1, 0))
    x <- dplyr::mutate(x,
                       last_reversals.acc =
                         dplyr::case_when(
                           last_reversals_trials == 1 ~ Accuracy))
    x <- dplyr::summarise(x,
                          threshold =
                            median(var_last_reversals, na.rm = TRUE),
                          acc =
                            mean(last_reversals.acc, na.rm = TRUE),
                          trials =
                            sum(last_reversals_trials, na.rm = TRUE),
                          minmax =
                            max(var_last_reversals, na.rm = TRUE) -
                            min(var_last_reversals, na.rm = TRUE))

    colnames(x)[which(colnames(x) == "threshold")] <-
      paste(taskname, "_Last", last_n, "Rev.", adaptive_variable, sep = "")
    colnames(x)[which(colnames(x) == "acc")] <-
      paste(taskname, "_Last", last_n, "Rev.ACC", sep = "")
    colnames(x)[which(colnames(x) == "trials")] <-
      paste(taskname, "_Last", last_n, "Rev.n", sep = "")
    colnames(x)[which(colnames(x) == "minmax")] <-
      paste(taskname, "_Last", last_n, "Rev.MinMax", sep = "")

  } else if (scoring_method == "Last Trial") {
    x <- dplyr::summarise(x, threshold = dplyr::last(get(adaptive_variable)))

    colnames(x)[which(colnames(x) == "threshold")] <-
      paste(taskname, "_LastTrial.", adaptive_variable, sep = "")

  } else if (scoring_method == "All Reversals") {
    x <- dplyr::group_by(x, ReversalMade, .add = TRUE)
    x <- dplyr::summarise(x,
                          threshold =
                            median(get(adaptive_variable), na.rm = TRUE),
                          reversals = dplyr::n(),
                          acc = mean(Accuracy, na.rm = TRUE),
                          minmax =
                            max(get(adaptive_variable), na.rm = TRUE) -
                            min(get(adaptive_variable), na.rm = TRUE))
    x <- dplyr::filter(x, ReversalMade == 1)
    x <- dplyr::select(x, -ReversalMade)

    colnames(x)[which(colnames(x) == "threshold")] <-
      paste(taskname, "_AllRev.", adaptive_variable, sep = "")
    colnames(x)[which(colnames(x) == "reversals")] <-
      paste(taskname, "_AllRev.n", sep = "")
    colnames(x)[which(colnames(x) == "acc")] <-
      paste(taskname, "_AllRev.ACC", sep = "")
    colnames(x)[which(colnames(x) == "minmax")] <-
      paste(taskname, "_AllRev.MinMax", sep = "")

  } else if (scoring_method == "Overall Median") {
    x <- dplyr::summarise(x, threshold = median(get(adaptive_variable)))

    colnames(x)[which(colnames(x) == "threshold")] <-
      paste(taskname, "_Overall.", adaptive_variable, sep = "")

  }

  return(x)
}


