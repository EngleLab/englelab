#' Raw Tidy Data for Three-Minute Squared Tasks
#'
#' Converts a messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col c(): list of additional columns to include
#' @export
#'

raw_squared <- function(x, include_col = c()) {

  if (stringr::str_detect(x$ExperimentName[1], "Flanker")) {
    x <- x |>
      dplyr::mutate(.by = Subject,
                    Task = "Flanker Squared",
                    TaskProc = 
                      dplyr::case_when(Procedure == "PracticeProc" ~ "practice",
                                      Procedure == "TaskProc" ~ "task"),
                    TrialScore = dplyr::case_when(PracticeTask.ACC == 1 ~ 1,
                                            PracticeTask.ACC == 0 ~ -1,
                                            Task.ACC == 1 ~ 1,
                                            Task.ACC == 0 ~ -1,
                                            TRUE ~ as.numeric(NA)),
                    Response = dplyr::case_when(PracticeTask.RESP == "{resp1}" ~ resp1,
                                          PracticeTask.RESP == "{resp2}" ~ resp2,
                                          Task.RESP == "{resp1}" ~ resp1,
                                          Task.RESP == "{resp2}" ~ resp2,
                                          TRUE ~ as.character(NA)),
                    Response = stringr::str_replace_all(Response, "f", "L"),
                    Response = stringr::str_replace_all(Response, "g", "R"),
                    CorrectResponse = dplyr::case_when(correctanswer == "{resp1}" ~ resp1,
                                                correctanswer == "{resp2}" ~ resp2,
                                                TRUE ~ as.character(NA)),
                    CorrectResponse = stringr::str_replace_all(CorrectResponse, "f", "L"),
                    CorrectResponse = stringr::str_replace_all(CorrectResponse, "g", "R"),
                    Stimulus = stringr::str_replace_all(stim, "f", "L"),
                    Stimulus = stringr::str_replace_all(Stimulus, "g", "R"),
                    ResponseOption1 = stringr::str_replace_all(resp1, "f", "L"),
                    ResponseOption1 = stringr::str_replace_all(ResponseOption1, "g", "R"),
                    ResponseOption2 = stringr::str_replace_all(resp2, "f", "L"),
                    ResponseOption2 = stringr::str_replace_all(ResponseOption2, "g", "R"),
                    Trial = dplyr::case_when(TaskProc == "practice" ~ PracticeTrialNumber,
                                      TaskProc == "task" ~ TrialNumber),
                    Accuracy = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.ACC,
                                          TaskProc == "task" ~ Task.ACC),
                    RT = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.RT,
                                    TaskProc == "task" ~ Task.RT),
                    RunningScore = dplyr::case_when(TaskProc == "practice" ~ PracticeScore,
                                              TaskProc == "task" ~ Score),
                    TaskTime = 
                      (dplyr::last(BlockDuration) + 
                        max(PracticeBlockDuration, na.rm = TRUE)) / 1000) |>
      dplyr::filter(!is.na(Response)) |>
      dplyr::select(Subject, Task, 
                    TaskProc, Trial, TrialType, 
                    Accuracy, RT, Response, TrialScore, RunningScore,
                    CorrectResponse, Stimulus, ResponseOption1, ResponseOption2, 
                    TaskTime, SessionDate, SessionTime, dplyr::any_of(include_col))
  } else if (stringr::str_detect(x$ExperimentName[1], "Simon")) {
    x <- x |>
      dplyr::mutate(.by = Subject,
                    Task = "Simon Squared",
                    TaskProc = 
                      dplyr::case_when(Procedure == "PracticeProc" ~ "practice",
                                       Procedure == "TaskProc" ~ "task"),
                    TrialScore = dplyr::case_when(PracticeTask.ACC == 1 ~ 1,
                                            PracticeTask.ACC == 0 ~ -1,
                                            Task.ACC == 1 ~ 1,
                                            Task.ACC == 0 ~ -1,
                                            TRUE ~ as.numeric(NA)),
                    Response = dplyr::case_when(PracticeTask.RESP == "{resp1}" ~ resp1,
                                          PracticeTask.RESP == "{resp2}" ~ resp2,
                                          Task.RESP == "{resp1}" ~ resp1,
                                          Task.RESP == "{resp2}" ~ resp2,
                                          TRUE ~ as.character(NA)),
                    CorrectResponse = dplyr::case_when(correctanswer == "{resp1}" ~ resp1,
                                                correctanswer == "{resp2}" ~ resp2,
                                                TRUE ~ as.character(NA)),
                    StimulusLocation = dplyr::case_when(!is.na(left) ~ "LEFT",
                                                  !is.na(right) ~ "RIGHT",
                                                  TRUE ~ as.character(NA)),
                    Stimulus = dplyr::case_when(StimulusLocation == "LEFT" ~ left,
                                          StimulusLocation == "RIGHT" ~ right,
                                          TRUE ~ as.character(NA)),
                    Stimulus = dplyr::case_when(Stimulus == "`" ~ "LEFT",
                                          Stimulus == "_" ~ "RIGHT",
                                          TRUE ~ as.character(NA)),
                    Trial = dplyr::case_when(TaskProc == "practice" ~ PracticeTrialNumber,
                                      TaskProc == "task" ~ TrialNumber),
                    Accuracy = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.ACC,
                                          TaskProc == "task" ~ Task.ACC),
                    RT = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.RT,
                                    TaskProc == "task" ~ Task.RT),
                    RunningScore = dplyr::case_when(TaskProc == "practice" ~ PracticeScore,
                                              TaskProc == "task" ~ Score),
                    TaskTime = 
                      (dplyr::last(BlockDuration) + 
                        max(PracticeBlockDuration, na.rm = TRUE)) / 1000) |>
      dplyr::filter(!is.na(Response)) |>
      dplyr::select(Subject, Task, 
                    TaskProc, Trial, TrialType, 
                    Accuracy, RT, Response, TrialScore, RunningScore,
                    CorrectResponse, Stimulus, StimulusLocation, 
                    ResponseOption1 = resp1, ResponseOption2 = resp2, 
                    TaskTime, SessionDate, SessionTime, dplyr::any_of(include_col))
  } else if (stringr::str_detect(x$ExperimentName[1], "Stroop")) {
    x <- x |>
      dplyr::mutate(.by = Subject,
                    Task = "Stroop Squared",
                    TaskProc = dplyr::case_when(Procedure == "PracticeProc" ~ "practice",
                                          Procedure == "TaskProc" ~ "task"),
                    TrialScore = dplyr::case_when(PracticeTask.ACC == 1 ~ 1,
                                            PracticeTask.ACC == 0 ~ -1,
                                            Task.ACC == 1 ~ 1,
                                            Task.ACC == 0 ~ -1,
                                            TRUE ~ as.numeric(NA)),
                    ResponseOption1_Color = dplyr::case_when(resp1color == 16130254 ~ "blue",
                                                      resp1color == 25532 ~ "red",
                                                      TRUE ~ as.character(NA)),
                    ResponseOption2_Color = dplyr::case_when(resp2color == 16130254 ~ "blue",
                                                      resp2color == 25532 ~ "red",
                                                      TRUE ~ as.character(NA)),
                    Response_Word = dplyr::case_when(PracticeTask.RESP == "{resp1}" ~ resp1,
                                              PracticeTask.RESP == "{resp2}" ~ resp2,
                                              Task.RESP == "{resp1}" ~ resp1,
                                              Task.RESP == "{resp2}" ~ resp2,
                                              TRUE ~ as.character(NA)),
                    CorrectResponse_Word = dplyr::case_when(correctanswer == "{resp1}" ~ resp1,
                                                      correctanswer == "{resp2}" ~ resp2,
                                                      TRUE ~ as.character(NA)),
                    Response_Color = dplyr::case_when(PracticeTask.RESP == "{resp1}" ~ ResponseOption1_Color,
                                                PracticeTask.RESP == "{resp2}" ~ ResponseOption2_Color,
                                                Task.RESP == "{resp1}" ~ ResponseOption1_Color,
                                                Task.RESP == "{resp2}" ~ ResponseOption2_Color,
                                                TRUE ~ as.character(NA)),
                    CorrectResponse_Color = 
                      dplyr::case_when(correctanswer == "{resp1}" ~ ResponseOption1_Color,
                                correctanswer == "{resp2}" ~ ResponseOption2_Color,
                                TRUE ~ as.character(NA)),
                    Stimulus_Color = dplyr::case_when(stimcolor == 16130254 ~ "blue",
                                                stimcolor == 25532 ~ "red",
                                                TRUE ~ as.character(NA)),
                    Accuracy = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.ACC,
                                          TaskProc == "task" ~ Task.ACC),
                    RT = dplyr::case_when(TaskProc == "practice" ~ PracticeTask.RT,
                                    TaskProc == "task" ~ Task.RT),
                    Trial = dplyr::case_when(TaskProc == "practice" ~ PracticeTrialNumber,
                                      TaskProc == "task" ~ TrialNumber),
                    RunningScore = dplyr::case_when(TaskProc == "practice" ~ PracticeScore,
                                              TaskProc == "task" ~ Score),
                    TaskTime = 
                      (dplyr::last(BlockDuration) + 
                        max(PracticeBlockDuration, na.rm = TRUE)) / 1000) |>
      dplyr::filter(!is.na(Response_Word)) |>
      dplyr::select(Subject, Task, TaskProc, Trial, TrialType, 
                    Accuracy, RT, TrialScore, RunningScore, 
                    Response_Word, Response_Color, CorrectResponse_Word,
                    CorrectResponse_Color, Stimulus_Word = stim, Stimulus_Color, 
                    ResponseOption1_Word = resp1, ResponseOption1_Color, 
                    ResponseOption2_Word = resp2, ResponseOption2_Color, 
                    TaskTime, SessionDate, SessionTime, dplyr::any_of(include_col))
  }

  return(x)
}


#' Score Three-Minute Squared Tasks
#'
#' Calculates task scores for the Squared Tasks from the output of raw_squared().
#'
#' @param x dataframe
#' @export
#'

score_squared <- function(x) {
  x <- x |>
    dplyr::filter(TaskProc == "task") |>
    dplyr::summarise(.by = Subject,
                     Points = sum(TrialScore, na.rm = TRUE),
                     ACC = sum(Accuracy, na.rm = TRUE),
                     RT = mean(RT, na.rm = TRUE),
                     Trials = dplyr::n(),
                     AdminTime = mean(TaskTime, na.rm = TRUE)) |>
    dplyr::rename_with(\(col) 
                        paste0(stringr::str_replace(x$Task[1], " ", "_"), ".", col), 
                       -Subject) |>
    dplyr::arrange(Subject)
  
  return(x)
}