#' Raw Tidy Data for Visual Arrays
#'
#' Converts the messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col list of additional columns to include
#' @export
#'

raw_visualarrays <- function(x, include_col = c()) {
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "showproc" ~
                                                    "real",
                                                  TrialProc == "pracproc" ~
                                                    "practice",
                                                  TrialProc == "PracProc" ~
                                                    "practice"),
                     Accuracy = VisResponse.ACC,
                     RT = VisResponse.RT,
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
                       Accuracy, RT, Response, CorrectResponse,
                       CorrectRejection, FalseAlarm, Miss, Hit,
                       include_col, AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Trial, SetSize,
                       Accuracy, RT, Response, CorrectResponse,
                       CorrectRejection, FalseAlarm, Miss, Hit,
                       include_col, SessionDate, SessionTime)
  }

  x <- dplyr::filter(x, TrialProc == "real" |
                       TrialProc == "practice")

  return(x)
}


#' Calculate Visual Array k Scores
#'
#' Calculate Visual Array k scores from the output of `raw_visualarrays()`
#' @param x dataframe
#' @param id_col Subject id column name (required)
#' @param taskname string to add as a prefix to columns. Useful if your task
#'     includes multiple conditions (excluding set size)
#' @export
#'

score_visualarrays <- function(x, id_col = "Subject", taskname = "VAorient_S") {

  x <- dplyr::filter(x, TrialProc == "real")

  grouped_vars <- colnames(dplyr::group_keys(x))
  grouped_vars <- grouped_vars[which(grouped_vars != id_col)]
  grouped_vars_names <- paste("{", grouped_vars, "}", sep = "")
  grouped_vars_names <- stringr::str_flatten(grouped_vars_names, "_")

  if ("AdminTime" %in% colnames(x)) {
    x <- dplyr::summarise(x,
                          CR.n = sum(CorrectRejection, na.rm = TRUE),
                          FA.n = sum(FalseAlarm, na.rm = TRUE),
                          M.n = sum(Miss, na.rm = TRUE),
                          H.n = sum(Hit, na.rm = TRUE),
                          ACC = mean(Accuracy, na.rm = TRUE),
                          RT = mean(RT, na.rm = TRUE),
                          AdminTime = dplyr::first(AdminTime))
  } else {
    x <- dplyr::summarise(x,
                          CR.n = sum(CorrectRejection, na.rm = TRUE),
                          FA.n = sum(FalseAlarm, na.rm = TRUE),
                          M.n = sum(Miss, na.rm = TRUE),
                          H.n = sum(Hit, na.rm = TRUE),
                          ACC = mean(Accuracy, na.rm = TRUE),
                          RT = mean(RT, na.rm = TRUE))
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
                     CorrectRejections = CR.n / (CR.n + FA.n),
                     FalseAlarms = FA.n / (CR.n + FA.n),
                     Hits = H.n / (H.n + M.n),
                     Misses = M.n / (H.n + M.n),
                     k = SetSize * (Hits - FalseAlarms))
  x <- dplyr::select(x, -CR.n, -FA.n, -M.n, -H.n)

  if ("AdminTime" %in% colnames(x)) {
    x <- tidyr::pivot_wider(x, id_cols = id_col,
                            names_from = grouped_vars,
                            names_glue =
                              paste(grouped_vars_names, "{.value}", sep = "."),
                            values_from =
                              c(k, ACC, RT, CorrectRejections, FalseAlarms,
                                Hits, Misses, AdminTime))
    x <- dplyr::rename(x, AdminTime = dplyr::last_col())
    x <- dplyr::select(x, -dplyr::contains(".AdminTime"))
  } else {
    x <- tidyr::pivot_wider(x, id_cols = id_col,
                            names_from = grouped_vars,
                            names_glue =
                              paste(grouped_vars_names, "{.value}", sep = "."),
                            values_from =
                              c(k, ACC, RT, CorrectRejections, FalseAlarms,
                                Hits, Misses))
  }
  x <- dplyr::mutate(x,
                     k = rowMeans(dplyr::across(dplyr::contains("k"))),
                     ACC = rowMeans(dplyr::across(dplyr::contains("ACC"))),
                     RT = rowMeans(dplyr::across(dplyr::contains("RT"))),
                     CorrectRejections =
                       rowMeans(dplyr::across(
                         dplyr::contains("CorrectRejections"))),
                     FalseAlarms =
                       rowMeans(dplyr::across(dplyr::contains("FalseAlarms"))),
                     Hits = rowMeans(dplyr::across(dplyr::contains("Hits"))),
                     Misses = rowMeans(dplyr::across(dplyr::contains("Misses"))))

  x <- dplyr::ungroup(x)
  x <- dplyr::relocate(x,
                       Misses, dplyr::contains("Misses"),
                       .before = last_col())
  x <- dplyr::relocate(x,
                       Hits, dplyr::contains("Hits"),
                       .before = Misses)
  x <- dplyr::relocate(x,
                       FalseAlarms, dplyr::contains("FalseAlarms"),
                       .before = Hits)
  x <- dplyr::relocate(x,
                       CorrectRejections, dplyr::contains("CorrectRejections"),
                       .before = FalseAlarms)
  x <- dplyr::relocate(x, ACC, dplyr::contains("ACC"),
                       .before = CorrectRejections)
  x <- dplyr::relocate(x, RT, dplyr::contains("RT"),
                       .after = ACC)
  x <- dplyr::relocate(x, k, contains("k"), .after = id_col)
  x <- dplyr::rename_with(x, ~paste(taskname, ., sep = "."), -id_col)
  x <- dplyr::rename_with(x,
                          ~stringr::str_replace(., "\\.", "_"), matches("[1-9]"))

  return(x)
}
