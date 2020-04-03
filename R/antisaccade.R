#' Creates a "tidy" raw dataframe for the Antisaccade task
#'
#' @param x dataframe (an imported .emrge file)
#' @param taskVersion is this a "new" or "old" taskVersion of the task?
#' @export
#'

raw_antisaccade <- function(x, taskVersion = "new"){
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  proc_names <- unique(x$TrialProc)
  if ("pracproc" %in% proc_names) {
    x <- dplyr::group_by(x, Subject)
    if (taskVersion == "new") {
      x <- dplyr::mutate(x, zoo::na.locf(AdminTime, fromLast = TRUE))
    }
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x,
                       TrialProc == "TrialProc" | TrialProc == "pracproc")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialProc" ~ "real",
                                          TrialProc == "pracproc" ~ "practice"),
                       Trial =
                         dplyr::case_when(TrialProc == "real" ~ TrialList.Sample,
                                          TrialProc == "practice" ~ practice.Sample))
  } else {
    x <- dplyr::select(x, -TrialProc)
    x <- dplyr::rename(x, TrialProc = `Running[Trial]`)
    x <- dplyr::filter(x,
                       TrialProc == "TrialList" | TrialProc == "PracticeList2")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialList" ~ "real",
                                          TrialProc == "PracticeList2" ~ "practice"),
                       Trial =
                         dplyr::case_when(TrialProc == "real" ~ TrialList.Sample,
                                          TrialProc == "practice" ~ PracticeList2.Sample))
  }

  x <- dplyr::mutate(x,
                     Target = dplyr::case_when(!is.na(right_targ) ~ right_targ,
                                               !is.na(left_targ) ~ left_targ))

  if (taskVersion == "new") {
    x <- dplyr::mutate(x, AdminTime = AdminTime/1000/60)
  }

  x_score <- dplyr::filter(x, TrialProc == "real")
  x_score <- dplyr::group_by(x_score, Subject)
  x_score <- dplyr::summarise(x_score, Antisaccade.ACC = mean(Mask.ACC, na.rm = TRUE))
  x_score <- dplyr::ungroup(x_score)

  x <- merge(x, x_score, by = "Subject", all = TRUE)

  if ("InstructionsTime" %in% colnames(x)) {
    x <- dplyr::mutate(x,
                       InstructionsTime = InstructionsTime/1000/60,
                       PracticeTime = PracticeTime/1000/60,
                       TaskTime = TaskTime/1000/60)
    x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                       RT = Mask.RT, Target, FixationDuration, Antisaccade.ACC,
                       InstructionsTime, PracticeTime, TaskTime, AdminTime,
                       SessionDate, SessionTime)
  } else {
    if (taskVersion == "new") {
      x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                         RT = Mask.RT, Target, FixationDuration = t4, Antisaccade.ACC,
                         AdminTime, SessionDate, SessionTime)
    } else if (taskVersion == "old") {
      x <- dplyr::select(x, Subject, TrialProc, Trial, Accuracy = Mask.ACC,
                         RT = Mask.RT, Target, FixationDuration = t4, Antisaccade.ACC,
                         SessionDate, SessionTime)
    }

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
  x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
  proc_names <- unique(x$TrialProc)
  if ("pracproc" %in% proc_names) {
    x <- dplyr::group_by(x, Subject)
    if (taskVersion == "new") {
      x <- dplyr::mutate(x, zoo::na.locf(AdminTime, fromLast = TRUE))
    }
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x,
                       TrialProc == "TrialProc" | TrialProc == "pracproc")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialProc" ~ "real",
                                          TrialProc == "pracproc" ~ "practice"),
                       Trial =
                         dplyr::case_when(TrailProc == "real" ~ TrialList.Sample,
                                          TrialProc == "practice" ~ practice.sample))
  } else {
    x <- dplyr::select(x, -TrialProc)
    x <- dplyr::rename(x, TrialProc = `Running[Trial]`)
    x <- dplyr::filter(x,
                       TrialProc == "TrialList" | TrialProc == "PracticeList2")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(TrialProc == "TrialList" ~ "real",
                                          TrialProc == "PracticeList2" ~ "practice"),
                       Trial =
                         dplyr::case_when(TrialProc == "real" ~ TrialList.Sample,
                                          TrialProc == "practice" ~ PracticeList2.Sample))
  }

  x <- dplyr::mutate(x,
                     Target = dplyr::case_when(!is.na(right_targ) ~ right_targ,
                                               !is.na(left_targ) ~ left_targ))

  x <- dplyr::filter(x, TrialProc == "real")
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::summarise(x,
                        Antisaccade.ACC = mean(Mask.ACC, na.rm = TRUE),
                        AdminTime = mean(AdminTime, na.rm = TRUE)/1000/60)
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, Subject, Antisaccade.ACC, AdminTime)
  return(x)
}