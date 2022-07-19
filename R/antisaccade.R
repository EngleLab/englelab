#' Raw Tidy Data for Antisaccade
#'
#' Converts the messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col c(): list of additional columns to include
#' @export
#'

raw_antisaccade <- function(x, include_col = c()) {

  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  proc_names <- unique(x$TrialProc)
  if ("pracproc" %in% proc_names) {
    x <- dplyr::filter(x, TrialProc == "TrialProc" | TrialProc == "pracproc")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialProc" ~ "real",
                                          TrialProc == "pracproc" ~ "practice"),
                       Trial =
                         dplyr::case_when(TrialProc == "real" ~
                                            TrialList.Sample,
                                          TrialProc == "practice" ~
                                            practice.Sample))
  } else {
    x <- dplyr::select(x, -TrialProc)
    x <- dplyr::rename(x, TrialProc = `Running[Trial]`)
    x <- dplyr::filter(x,
                       TrialProc == "TrialList" | TrialProc == "PracticeList2")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialList" ~
                                            "real",
                                          TrialProc == "PracticeList2" ~
                                            "practice"),
                       Trial =
                         dplyr::case_when(TrialProc == "real" ~
                                            TrialList.Sample,
                                          TrialProc == "practice" ~
                                            PracticeList2.Sample))
  }

  x <- dplyr::mutate(x,
                     Target =
                       dplyr::case_when(!is.na(right_targ) ~ right_targ,
                                        !is.na(left_targ) ~ left_targ))

  if ("AdminTime" %in% colnames(x)) {
    x <- dplyr::group_by(x, Subject)
    x <- dplyr::mutate(x, AdminTime = dplyr::last(AdminTime) / 60000)
    x <- dplyr::ungroup(x)
    x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                       RT = Mask.RT, Target, FixationDuration,
                       include_col, AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                       RT = Mask.RT, Target, FixationDuration,
                       include_col, SessionDate, SessionTime)
  }

  return(x)
}
