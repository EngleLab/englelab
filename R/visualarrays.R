#' Creates a "tidy" raw dataframe for the Visual Arrays task
#'
#' @param x dataframe (an imported .emrge file)
#' @param taskVersion is this a "new" or "old" taskVersion of the task?
#' @export
#'

raw_visualarrays <- function(x, taskVersion = "new"){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "showproc" ~
                                                    "real",
                                                  TrialProc == "pracproc" ~
                                                    "practice",
                                                  TrialProc == "PracProc" ~
                                                    "practice"),
                     Accuracy = VisResponse.ACC,
                     Response =
                       dplyr::case_when(VisResponse.RESP == 5 |
                                          VisResponse.RESP == "s" ~ "same",
                                        VisResponse.RESP == 6 |
                                          VisResponse.RESP == "d" ~ "different"),
                     CorrectResponse =
                       dplyr::case_when(VisResponse.CRESP == 5 |
                                          VisResponse.CRESP == "s" ~ "same",
                                        VisResponse.CRESP == 6 |
                                          VisResponse.CRESP == "d" ~ "different"),
                     CorrectRejection =
                       dplyr::case_when(CorrectResponse == "same" &
                                          Response == "same" ~ 1,
                                        TRUE ~ 0),
                     FalseAlarm =
                       dplyr::case_when(CorrectResponse == "same" &
                                          Response == "different" ~ 1,
                                        TRUE ~ 0),
                     Miss =
                       dplyr::case_when(CorrectResponse == "different" &
                                          Response == "same" ~ 1,
                                        TRUE ~ 0),
                     Hit =
                       dplyr::case_when(CorrectResponse == "different" &
                                          Response == "different" ~ 1,
                                        TRUE ~ 0))

  if ("AdminTime" %in% colnames(x)) {
    x <- dplyr::group_by(x, Subject)
    x <- dplyr::mutate(x, AdminTime = dplyr::last(AdminTime) / 60000)
    x <- dplyr::ungroup(x)
    x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                       Accuracy, Response, CorrectResponse,
                       CorrectRejection, FalseAlarm, Miss, Hit,
                       AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                       Accuracy, Response, CorrectResponse,
                       CorrectRejection, FalseAlarm, Miss, Hit,
                       SessionDate, SessionTime)
  }

  x <- dplyr::filter(x, TrialProc == "real" |
                       TrialProc == "practice")

  return(x)
}


#' Calculate Visual Array k scores from a messy raw dataframe
#'
#' This function will calculate a k score from the raw data
#' outputed by raw_visualarrays().
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_visualarrays <- function(x){
  x <- dplyr::group_by(x, SetSize, .add = TRUE)
  x <- dplyr::summarise(x,
                        CR.n = sum(CorrectRejection, na.rm = TRUE),
                        FA.n = sum(FalseAlarm, na.rm = TRUE),
                        M.n = sum(Miss, na.rm = TRUE),
                        H.n = sum(Hit, na.rm = TRUE))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
                     CR = CR.n / (CR.n + FA.n),
                     H = H.n / (H.n + M.n),
                     k = SetSize * (H + CR - 1))
  return(x)
}
