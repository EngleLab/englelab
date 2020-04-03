#' Creates a "tidy" raw dataframe for the Visual Arrays task
#'
#' @param x dataframe (an imported .emrge file)
#' @param taskVersion is this a "new" or "old" taskVersion of the task?
#' @export
#'

raw_visualarrays <- function(x, taskVersion = "new"){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::filter(x, TrialProc == "showproc" |
                       TrialProc == "pracproc")
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "showproc" ~ "real",
                                                  TrialProc == "pracproc" ~ "practice"),
                     Accuracy = VisResponse.ACC,
                     Response = dplyr::case_when(VisResponse.RESP == 5 ~ "same",
                                                 VisResponse.RESP == 6 ~ "different"),
                     CorrectResponse = dplyr::case_when(VisResponse.CRESP == 5 ~ "same",
                                                        VisResponse.CRESP == 6 ~ "different"),
                     CorrectRejection =
                       dplyr::case_when(CorrectResponse == "same" & Response == "same" ~
                                          1,
                                        TRUE ~ 0),
                     FalseAlarm =
                       dplyr::case_when(CorrectResponse == "same" & Response == "different" ~
                                          1,
                                        TRUE ~ 0),
                     Miss =
                       dplyr::case_when(CorrectResponse == "different" & Response == "same" ~
                                          1,
                                        TRUE ~ 0),
                     Hit =
                       dplyr::case_when(CorrectResponse == "different" & Response == "different" ~
                                          1,
                                        TRUE ~ 0))

  if (taskVersion == "new") {
    x <- dplyr::mutate(x, AdminTime = AdminTime/1000/60)
  }

  x_score <- dplyr::filter(x, TrialProc == "real")
  x_score <- dplyr::group_by(x_score, Subject, SetSize)
  x_score <- dplyr::summarise(x_score,
                              CR = sum(CorrectRejection, na.rm = TRUE),
                              FA = sum(FalseAlarm, na.rm = TRUE),
                              M = sum(Miss, na.rm = TRUE),
                              H = sum(Hit, na.rm = TRUE))
  x_score <- dplyr::ungroup(x_score)
  x_score <- dplyr::mutate(x_score,
                           Different = M + H,
                           Same = CR + FA,
                           CR = CR / Same,
                           FA = FA / Same,
                           M = M / Different,
                           H = H / Different,
                           VA_k = SetSize*(H + CR - 1))
  x_score <- tidyr::pivot_wider(x_score,
                                id_cols = "Subject",
                                names_from = "SetSize",
                                values_from = "VA_k")
  x_score <- dplyr::rename(x_score, VA_k.5 = `5`, VA_k.7 = `7`)
  x_score <- dplyr::mutate(x_score, VA_k = (VA_k.5 + VA_k.7) / 2)

  x <- merge(x, x_score, by = "Subject", all = TRUE)

  if ("InstructionsTime" %in% colnames(x)) {
    x <- dplyr::mutate(x,
                       InstructionsTime = InstructionsTime/1000/60,
                       PracticeTime = PracticeTime/1000/60,
                       TaskTime = TaskTime/1000/60)
    x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                       Accuracy, Response, CorrectResponse,
                       CorrectRejection, FalseAlarm, Miss, Hit,
                       VA_k.5, VA_k.7, VA_k,
                       InstructionsTime, PracticeTime, TaskTime,
                       AdminTime, SessionDate, SessionTime)
  } else {
    if (taskVersion == "new") {
      x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                         Accuracy, Response, CorrectResponse,
                         CorrectRejection, FalseAlarm, Miss, Hit,
                         A_k.5, VA_k.7, VA_k,
                         AdminTime, SessionDate, SessionTime)
    } else if (taskVersion == "old") {
      x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                         Accuracy, Response, CorrectResponse,
                         CorrectRejection, FalseAlarm, Miss, Hit,
                         A_k.5, VA_k.7, VA_k,
                         SessionDate, SessionTime)
    }

  }

  return(x)
}


#' Calculate Visual Array k scores from a messy raw dataframe
#'
#' This function skips the 'raw_visualarrays()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_visualarrays()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_visualarrays <- function(x){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::filter(x, TrialProc == "showproc" |
                       TrialProc == "pracproc")
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "showproc" ~ "real",
                                                  TrialProc == "pracproc" ~ "practice"),
                     Accuracy = VisResponse.ACC,
                     Response = dplyr::case_when(VisResponse.RESP == 5 ~ "same",
                                                 VisResponse.RESP == 6 ~ "different"),
                     CorrectResponse = dplyr::case_when(VisResponse.CRESP == 5 ~ "same",
                                                        VisResponse.CRESP == 6 ~ "different"),
                     CorrectRejection =
                       dplyr::case_when(CorrectResponse == "same" & Response == "same" ~
                                          1,
                                        TRUE ~ 0),
                     FalseAlarm =
                       dplyr::case_when(CorrectResponse == "same" & Response == "different" ~
                                          1,
                                        TRUE ~ 0),
                     Miss =
                       dplyr::case_when(CorrectResponse == "different" & Response == "same" ~
                                          1,
                                        TRUE ~ 0),
                     Hit =
                       dplyr::case_when(CorrectResponse == "different" & Response == "different" ~
                                          1,
                                        TRUE ~ 0))

  x <- dplyr::filter(x, TrialProc == "real")
  x <- dplyr::group_by(x, Subject, SetSize)
  x <- dplyr::summarise(x,
                        CR = sum(CorrectRejection, na.rm = TRUE),
                        FA = sum(FalseAlarm, na.rm = TRUE),
                        M = sum(Miss, na.rm = TRUE),
                        H = sum(Hit, na.rm = TRUE))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
                     Different = M + H,
                     Same = CR + FA,
                     CR = CR / Same,
                     FA = FA / Same,
                     M = M / Different,
                     H = H / Different,
                     VA_k = SetSize*(H + CR - 1))
  x <- tidyr::pivot_wider(x,
                          id_cols = "Subject",
                          names_from = "SetSize",
                          values_from = "VA_k")
  x <- dplyr::rename(x, VA_k.5 = `5`, VA_k.7 = `7`)
  x <- dplyr::mutate(x, VA_k = (VA_k.5 + VA_k.7) / 2)

  return(x)
}
