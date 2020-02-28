#' Creates a "tidy" raw dataframe for the SACT task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

raw_sact <- function(x){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::filter(x, TrialProc == "TrialProc" |
                       TrialProc == "PracticeTrialProc")
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "TrialProc" ~ "real",
                                                  TrialProc == "PracticeTrialProc" ~ "pratice"),
                     InstructionsTime = InstructionsTime/1000/60,
                     PracticeTime = PracticeTime/1000/60,
                     TaskTime = TaskTime/1000/60,
                     AdminTime = AdminTime/1000/60)

  if ("InstructionsTime" %in% colnames(x)) {
    x <- dplyr::select(x, Subject, TrialProc, Trial, WaitTime,
                       RT = ResponseRT, Accuracy = Response.ACC,
                       Response = ResponseMade,  SACT.ACC = SACTscore,
                       InstructionsTime, PracticeTime, TaskTime, AdminTime,
                       SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Trial, WaitTime,
                       RT = ResponseRT, Accuracy = Response.ACC,
                       Response = ResponseMade, SACT.ACC = SACTscore, AdminTime,
                       SessionDate, SessionTime)
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
  x <- dplyr::select(x, Subject, SACT.ACC = SACTscore, AdminTime)
  x <- dplyr::distinct(x)
  x <- dplyr::mutate(x, AdminTime = AdminTime/1000/60)
  return(x)
}
