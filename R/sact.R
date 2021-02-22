#' Creates a "tidy" raw dataframe for the SACT task
#'
#' @param x dataframe (an imported .emrge file)
#' @param taskVersion is this a "new" or "old" taskVersion of the task?
#' @export
#'

raw_sact <- function(x, taskVersion = "new"){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::filter(x, TrialProc == "TrialProc" |
                       TrialProc == "PracticeTrialProc")
  x <- dplyr::mutate(x,
                     TrialProc =
                       dplyr::case_when(TrialProc == "TrialProc" ~
                                          "real",
                                        TrialProc == "PracticeTrialProc" ~
                                          "pratice"))

  if ("AdminTime" %in% colnames(x)) {
    x <- dplyr::group_by(x, Subject)
    x <- dplyr::mutate(x, AdminTime = dplyr::last(AdminTime) / 60000)
    x <- dplyr::ungroup(x)
    x <- dplyr::select(x, Subject, TrialProc, Trial, WaitTime,
                       RT = ResponseRT, Accuracy = Response.ACC,
                       Response = ResponseMade,
                       AdminTime, SessionDate, SessionTime)
  } else {
    if ("WaitTime" %in% colnames(x)) {
      x <- dplyr::select(x, Subject, TrialProc, Trial, WaitTime,
                         RT = ResponseRT, Accuracy = Response.ACC,
                         Response = ResponseMade,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, TrialProc, Trial,
                         WaitTime = `WaitTime[Trial]`,
                         RT = ResponseRT, Accuracy = Response.ACC,
                         Response = ResponseMade,
                         SessionDate, SessionTime)
    }
  }
  return(x)
}


#' Calculate SACT accuracy scores from a messy raw dataframe
#'
#' This function skips the 'raw_sact()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_sact()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_sact <- function(x){
  message("Depricated")
}
