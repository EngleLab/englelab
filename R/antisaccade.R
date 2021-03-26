#' Creates a "tidy" raw dataframe for the Antisaccade task
#'
#' @param x dataframe (an imported .emrge file)
#' @param taskVersion depricated. Doesn't do anything anymore
#' @export
#'

raw_antisaccade <- function(x, taskVersion = NULL){
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
                       AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                       RT = Mask.RT, Target, FixationDuration,
                       SessionDate, SessionTime)
  }
  return(x)
}


#' Calculate Antisaccacde accuracy scores from a messy raw dataframe
#'
#' This function skips the 'raw_antisaccade()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_antisaccade()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_antisaccade <- function(x){
  message("Depricated")
}
