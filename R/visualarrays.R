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
#' @param taskname string to add as a prefix to columns
#' @export
#'

score_visualarrays <- function(x, taskname = "VAorient_S"){
  x <- dplyr::group_by(x, Subject, SetSize, .add = TRUE)

  if ("AdminTime" %in% colnames(x)) {
    x <- dplyr::summarise(x,
                          CR.n = sum(CorrectRejection, na.rm = TRUE),
                          FA.n = sum(FalseAlarm, na.rm = TRUE),
                          M.n = sum(Miss, na.rm = TRUE),
                          H.n = sum(Hit, na.rm = TRUE),
                          ACC = mean(Accuracy, na.rm = TRUE),
                          AdminTime = dplyr::first(AdminTime)
                          )
  } else {
    x <- dplyr::summarise(x,
                          CR.n = sum(CorrectRejection, na.rm = TRUE),
                          FA.n = sum(FalseAlarm, na.rm = TRUE),
                          M.n = sum(Miss, na.rm = TRUE),
                          H.n = sum(Hit, na.rm = TRUE),
                          ACC = mean(Accuracy, na.rm = TRUE)
                          )
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
                     CorrectRejections = CR.n / (CR.n + FA.n),
                     FalseAlarms = FA.n / (CR.n + FA.n),
                     Hits = H.n / (H.n + M.n),
                     Misses = M.n / (H.n + M.n),
                     k = SetSize * (Hits - FalseAlarms)
                     )
  x <- dplyr::select(x, -CR.n, -FA.n, -M.n, -H.n)

  x <- tidyr::pivot_wider(x, id_cols = Subject,
                          names_from = SetSize,
                          names_glue = "{SetSize}.{.value}",
                          values_from = c(k, ACC, CorrectRejections, FalseAlarms,
                                          Hits, Misses)
                          )
  x <- dplyr::rowwise(x)
  x <- dplyr::mutate(x,
                     k = mean(c(`3.k`, `5.k`)),
                     ACC = mean(c(`3.ACC`, `5.ACC`)),
                     CorrectRejections =
                       mean(c(`3.CorrectRejections`, `5.CorrectRejections`)),
                     FalseAlarms = mean(c(`3.FalseAlarms`, `5.FalseAlarms`)),
                     Hits = mean(c(`3.Hits`, `5.Hits`)),
                     Misses = mean(c(`3.Misses`, `5.Misses`))
                     )
  x <- dplyr::ungroup(x)
  x <- dplyr::relocate(x, k, `5.k`, `3.k`, .after = Subject)
  x <- dplyr::relocate(x, ACC, `5.ACC`, `3.ACC`, .after = `3.k`)
  x <- dplyr::relocate(x,
                       CorrectRejections, `5.CorrectRejections`, `3.CorrectRejections`,
                       .after = `3.ACC`
                       )
  x <- dplyr::relocate(x,
                       FalseAlarms, `5.FalseAlarms`, `3.FalseAlarms`,
                       .after = `3.CorrectRejections`
                       )
  x <- dplyr::relocate(x,
                       Hits, `5.Hits`, `3.Hits`,
                       .after = `3.FalseAlarms`
                       )
  x <- dplyr::relocate(x,
                       Misses, `5.Misses`, `3.Misses`,
                       .after = `3.Hits`
                       )
  x <- dplyr::rename_with(x, ~paste(taskname, ., sep = ""), -Subject)
  return(x)
}
