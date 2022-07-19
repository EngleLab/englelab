#' Raw Tidy Data for SACT
#'
#' Converts the messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col c(): list of additional columns to include
#' @export
#'

raw_sact <- function(x, include_col = c()) {

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
                       include_col, AdminTime, SessionDate, SessionTime)
  } else {
    if ("WaitTime" %in% colnames(x)) {
      x <- dplyr::select(x, Subject, TrialProc, Trial, WaitTime,
                         RT = ResponseRT, Accuracy = Response.ACC,
                         Response = ResponseMade,
                         include_col, SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, TrialProc, Trial,
                         WaitTime = `WaitTime[Trial]`,
                         RT = ResponseRT, Accuracy = Response.ACC,
                         Response = ResponseMade,
                         include_col, SessionDate, SessionTime)
    }
  }

  return(x)
}
