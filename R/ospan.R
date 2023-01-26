#' Raw Tidy Data for Symmetry Span
#'
#' Converts the messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col c(): list of additional columns to include
#' @param taskVersion old or new version. Required for different types of
#'     data files from older or newer versions of the complex-span tasks.
#'     First leave out the argument, if it does not work
#'     then try taskVersion = "old".
#' @export
#'

raw_ospan <- function(x, include_col = c(), taskVersion = "new") {

  exit_task_error <- FALSE

  if (x$ExperimentName[1] == "OspanShort") {
    taskVersion <- "oswald"
  }

  if (taskVersion == "new") {
    x <- dplyr::mutate(x,
                       `Procedure[Block]` =
                         ifelse(is.na(`Procedure[Block]`),
                                "TaskProc", `Procedure[Block]`))
    x <- dplyr::filter(x, `Procedure[Block]` == "TaskProc")
    if (!("AvgMathTime" %in% colnames(x))) {
      x <- dplyr::mutate(x, AvgMathTime = NA)
    }
  }

  if (taskVersion == "old" | taskVersion == "oswald") {
    x <- dplyr::filter(x, `Procedure[Block]` == "SessionProc")
    x <- dplyr::mutate(x, MathACC = NA, AvgMathTime = NA)
  }

  if (!("AvgMathTime" %in% colnames(x))) {
    x <- dplyr::mutate(x, AvgMathTime = NA)
  }

  if (NA %in% unique(x$`Running[Trial]`)) {
    x <- tidyr::fill(x, `Running[Trial]`, .direction = "down")
    x <- dplyr::mutate(x, `Procedure[SubTrial]` =
                         ifelse(is.na(`Procedure[SubTrial]`),
                                "recall", `Procedure[SubTrial]`))
    exit_task_error <- TRUE
  }

  x <- dplyr::rename(x, SetSize = setsz)
  x <-
    dplyr::mutate(x,
                  SubTrialProc =
                    dplyr::case_when(`Procedure[SubTrial]` == "TrialProc" |
                                       `Procedure[SubTrial]` == "TrialProc1" |
                                       `Procedure[SubTrial]` == "TrialProc2" ~
                                       "ProcessingTask",
                                     `Procedure[SubTrial]` == "recall" |
                                       `Procedure[SubTrial]` == "recall1" |
                                       `Procedure[SubTrial]` == "recall2" ~
                                       "Recall",
                                     TRUE ~ as.character(NA)),
                  Block = dplyr::case_when(`Running[Trial]` == "BlockList1" ~ 1,
                                           `Running[Trial]` == "BlockList2" ~ 2,
                                           `Running[Trial]` == "BlockList3" ~ 3,
                                           TRUE ~ as.double(NA)))

  if (taskVersion == "oswald") {
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(BlockList.Sample <= 3 ~ 1,
                                                BlockList.Sample > 3 ~ 2,
                                                TRUE ~ as.double(NA)))
  }

  blocks <- length(unique(x$Block))

  if (taskVersion != "oswald") {
    if (blocks == 1) {

    } else if (blocks == 2) {
      x <-
        dplyr::mutate(x,
                      OPERATION.RT =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(OPERATION.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(OPERATION1.RT),
                                         TRUE ~ as.double(NA)),
                      showProblem.RT =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(showProblem.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(showProblem1.RT),
                                         TRUE ~ as.double(NA)),
                      CollectClick.RT =
                        dplyr::case_when(SubTrialProc == "Recall" &
                                           Block == 1 ~
                                           as.double(CollectClick.RT),
                                         SubTrialProc == "Recall" &
                                           Block == 2 ~
                                           as.double(CollectClick2.RT),
                                         TRUE ~ as.double(NA)),
                      OPERATION.ACC =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(OPERATION.ACC),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(OPERATION1.ACC),
                                         TRUE ~ as.double((NA))))
    } else if (blocks == 3) {
      x <-
        dplyr::mutate(x,
                      OPERATION.RT =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(OPERATION.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(OPERATION1.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 3 ~ as.double(OPERATION2.RT),
                                         TRUE ~ as.double(NA)),
                      showProblem.RT =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(showProblem.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(showProblem1.RT),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 3 ~ as.double(showProblem2.RT),
                                         TRUE ~ as.double(NA)),
                      CollectClick.RT =
                        dplyr::case_when(SubTrialProc == "Recall" &
                                           Block == 1 ~
                                           as.double(CollectClick.RT),
                                         SubTrialProc == "Recall" &
                                           Block == 2 ~
                                           as.double(CollectClick2.RT),
                                         SubTrialProc == "Recall" &
                                           Block == 3 ~
                                           as.double(CollectClick3.RT),
                                         TRUE ~ as.double(NA)),
                      OPERATION.ACC =
                        dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                           Block == 1 ~ as.double(OPERATION.ACC),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 2 ~ as.double(OPERATION1.ACC),
                                         SubTrialProc == "ProcessingTask" &
                                           Block == 3 ~ as.double(OPERATION2.ACC),
                                         TRUE ~ as.double((NA))))
    }
  }

  x <- dplyr::group_by(x, Subject, Block, Trial)
  x <- dplyr::mutate(x,
                     RT =
                       dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                          as.double(OPERATION.RT) +
                                          as.double(showProblem.RT),
                                        SubTrialProc == "Recall" ~
                                          as.double(CollectClick.RT),
                                        TRUE ~ as.double(NA)),
                     clear_items =
                       dplyr::case_when(SubTrialProc == "Recall" &
                                          WordSelection == "Clear" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "clear" ~ 1,
                                        TRUE ~ as.numeric(NA)),
                     clear_items =
                       zoo::na.locf(clear_items, fromLast = TRUE, na.rm = FALSE),
                     clear_items =
                       ifelse(SubTrialProc == "ProcessingTask",
                              as.numeric(NA), clear_items),
                     button_click =
                       dplyr::case_when(SubTrialProc == "Recall" &
                                          WordSelection == "Enter" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "Exit" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "Clear" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "clear" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "InvalidResponse" ~
                                          1,
                                        TRUE ~ 0),
                     recall_response_type =
                       dplyr::case_when(SubTrialProc == "Recall" &
                                          button_click == 0 &
                                          clear_items == 1 ~ "clear",
                                        SubTrialProc == "Recall" &
                                          button_click == 1 ~ "button",
                                        SubTrialProc == "Recall" &
                                          button_click == 0 ~ "recall",
                                        TRUE ~ as.character(NA)),
                     recall_response =
                       dplyr::case_when(SubTrialProc == "Recall" ~
                                          as.character(WordSelection),
                                        TRUE ~ as.character(NA)),
                     AvgMathTime =
                       ifelse(!is.na(AvgMathTime) &
                                AvgMathTime == "?",
                              as.numeric(NA), AvgMathTime),
                     remove =
                       dplyr::case_when(SubTrialProc == "Recall" &
                                          is.na(WordSelection) ~ 1,
                                        TRUE ~ 0))

  x <- dplyr::filter(x, remove == 0)

  if (exit_task_error == TRUE) {
    x <- dplyr::group_by(x, Subject, Block, Trial, SubTrialProc)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       SetSize =
                         dplyr::case_when(is.na(SetSize) &
                                            SubTrialProc == "ProcessingTask" ~
                                            as.double(max(SubTrial)),
                                          TRUE ~ as.double(SetSize)))
    x <- dplyr::ungroup(x)
    x <- tidyr::fill(x, SetSize, .direction = "down")
  }

  x <- dplyr::group_by(x, Subject, Block, Trial, recall_response_type)
  x <- dplyr::mutate(x, recall_position = dplyr::row_number())

  x <- dplyr::group_by(x, Subject, Block, Trial)
  suppressWarnings({
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       position_1 =
                         ifelse(SubTrial == 1, letterstimuli, as.character(NA)),
                       position_1 = zoo::na.locf(position_1, na.rm = FALSE),
                       position_2 =
                         ifelse(SubTrial == 2, letterstimuli, as.character(NA)),
                       position_2 = zoo::na.locf(position_2, na.rm = FALSE),
                       position_3 =
                         ifelse(SubTrial == 3, letterstimuli, as.character(NA)),
                       position_3 = zoo::na.locf(position_3, na.rm = FALSE),
                       position_4 =
                         ifelse(SubTrial == 4, letterstimuli, as.character(NA)),
                       position_4 = zoo::na.locf(position_4, na.rm = FALSE),
                       position_5 =
                         ifelse(SubTrial == 5, letterstimuli, as.character(NA)),
                       position_5 = zoo::na.locf(position_5, na.rm = FALSE),
                       position_6 =
                         ifelse(SubTrial == 6, letterstimuli, as.character(NA)),
                       position_6 = zoo::na.locf(position_6, na.rm = FALSE),
                       position_7 =
                         ifelse(SubTrial == 7, letterstimuli, as.character(NA)),
                       position_7 = zoo::na.locf(position_7, na.rm = FALSE),
                       position_8 =
                         ifelse(SubTrial == 8, letterstimuli, as.character(NA)),
                       position_8 = zoo::na.locf(position_8, na.rm = FALSE),
                       position_9 =
                         ifelse(SubTrial == 9, letterstimuli, as.character(NA)),
                       position_9 = zoo::na.locf(position_9, na.rm = FALSE),
                       memory_item =
                         dplyr::case_when(recall_response_type == "recall" &
                                            recall_position == 1 ~
                                            as.character(position_1),
                                          recall_response_type == "recall" &
                                            recall_position == 2 ~
                                            as.character(position_2),
                                          recall_response_type == "recall" &
                                            recall_position == 3 ~
                                            as.character(position_3),
                                          recall_response_type == "recall" &
                                            recall_position == 4 ~
                                            as.character(position_4),
                                          recall_response_type == "recall" &
                                            recall_position == 5 ~
                                            as.character(position_5),
                                          recall_response_type == "recall" &
                                            recall_position == 6 ~
                                            as.character(position_6),
                                          recall_response_type == "recall" &
                                            recall_position == 7 ~
                                            as.character(position_7),
                                          recall_response_type == "recall" &
                                            recall_position == 8 ~
                                            as.character(position_8),
                                          recall_response_type == "recall" &
                                            recall_position == 9 ~
                                            as.character(position_9),
                                          TRUE ~ as.character(NA)),
                       CorrectResponse =
                         ifelse(SubTrialProc == "ProcessingTask",
                                as.character(`CorrectAnswer[SubTrial]`),
                                ifelse(SubTrialProc == "Recall",
                                       as.character(memory_item),
                                       as.character(NA))),
                       Accuracy =
                         dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                            as.double(OPERATION.ACC),
                                          SubTrialProc == "Recall" &
                                            CorrectResponse == WordSelection ~
                                            as.double(1),
                                          SubTrialProc == "Recall" &
                                            CorrectResponse != WordSelection ~
                                            as.double(0),
                                          TRUE ~ as.double((NA))),
                       Response =
                         dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                            Accuracy == 1 ~ CorrectResponse,
                                          SubTrialProc == "ProcessingTask" &
                                            Accuracy == 0 &
                                            CorrectResponse == "TRUE" ~ "FALSE",
                                          SubTrialProc == "ProcessingTask" &
                                            Accuracy == 0 &
                                            CorrectResponse == "FALSE" ~ "TRUE",
                                          SubTrialProc == "Recall" ~
                                            as.character(WordSelection),
                                          TRUE ~ as.character(NA)),
                       Response = dplyr::case_when(Response == "Blank" ~ "-",
                                                   Response == "blank" ~ "-",
                                                   TRUE ~ Response),
                       MemoryItem = letterstimuli,
                       Processing.correct =
                         ifelse(SubTrialProc == "ProcessingTask",
                                Accuracy, as.numeric(NA)),
                       Processing.correct =
                         stats::ave(Processing.correct,
                                    FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.correct =
                         ifelse(SubTrialProc == "Recall",
                                Accuracy, as.numeric(NA)),
                       Recall.correct =
                         stats::ave(Recall.correct,
                                    FUN = function(x) sum(x, na.rm = TRUE)),
                       Partial.unit = Recall.correct / SetSize,
                       Absolute.unit = ifelse(Recall.correct == SetSize, 1, 0),
                       Partial.load = Recall.correct,
                       Absolute.load =
                         ifelse(Recall.correct == SetSize, Recall.correct, 0))
  })

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, Subject, Block, Trial, SetSize, Processing.correct,
                     Recall.correct, Partial.unit, Partial.load,
                     Absolute.unit, Absolute.load, SubTrial, SubTrialProc,
                     RT, Accuracy, Response, CorrectResponse, MemoryItem,
                     include_col, SessionDate, SessionTime, recall_response_type)
  x <- dplyr::distinct(x)

  # add columns with sequence of target memory and recalled items
  x_tr <- dplyr::mutate(x,
                        Response =
                          dplyr::case_when(is.na(Response) ~ as.character(NA),
                                           Response == "TRUE" ~ "TRUE",
                                           Response == "FALSE" ~ "FALSE",
                                           TRUE ~ Response))
  x_tr <- dplyr::filter(x_tr, recall_response_type == "recall" |
                          SubTrialProc == "ProcessingTask")
  x_tr <- dplyr::group_by(x_tr, Subject, Block, Trial, SubTrialProc)
  x_tr <- dplyr::mutate(x_tr, SubTrial = dplyr::row_number())
  x_tr <- dplyr::ungroup(x_tr)
  x_tr <- tidyr::pivot_wider(x_tr,
                             id_cols = c(Subject, Block, Trial),
                             names_from = c(SubTrialProc, SubTrial),
                             values_from = c(MemoryItem, Response))
  x_tr <- tidyr::unite(x_tr, "MemoryTargets",
                       dplyr::contains("MemoryItem_ProcessingTask"),
                       sep = "", na.rm = TRUE)
  x_tr <- tidyr::unite(x_tr, "Recalled",
                       dplyr::contains("Response_Recall"),
                       sep = "", na.rm = TRUE)
  x_tr <- dplyr::select(x_tr, Subject, Block, Trial, MemoryTargets, Recalled)

  x <- merge(x, x_tr, by = c("Subject", "Block", "Trial"))
  x <- dplyr::relocate(x, MemoryTargets, Recalled, .after = SetSize)

  # add columns for edit distance load and unit scores
  x <- englelab::edit_distance(x)
  x <- dplyr::relocate(x, EditDistance.unit, EditDistance.load,
                       .after = Processing.correct)

  # remove Recall.correct. It is redundant with Partial.load
  x <- dplyr::select(x, -Recall.correct, -recall_response_type)

  return(x)
}


#' Calculate Operation Span Scores
#'
#' Calculate various span scores from the output of `raw_ospan()`
#' @param x dataframe
#' @export
#'

score_ospan <- function(x) {

  message("If you use the Edit Distance scores, then cite: ",
          "Gonthier, C. (2022). An easy way to improve scoring of memory span tasks ",
          "The edit distance, beyond “correct recall in the correct serial position.” ",
          "Behavior Research Methods, 16. https://doi.org/10.3758/s13428-022-01908-2")

  if ("Running[Trial]" %in% colnames(x)) {
    x <- englelab::raw_ospan(x)
  }

  x_recall <- dplyr::distinct(x, Subject, Block, Trial, SetSize,
                              EditDistance.unit, EditDistance.load,
                              Partial.unit, Partial.load,
                              Absolute.unit, Absolute.load)
  x_recall <- dplyr::summarise(x_recall,
                               OSpan.EditDistanceScore = sum(EditDistance.load),
                               OSpan.EditDistanceUnit = mean(EditDistance.unit),
                               OSpan.EditDistanceLoad =
                                 sum(EditDistance.load) / sum(SetSize),
                               OSpan.PartialScore = sum(Partial.load),
                               OSpan.PartialUnit = mean(Partial.unit),
                               OSpan.PartialLoad =
                                 sum(Partial.load) / sum(SetSize),
                               Ospan.AbsoluteScore = sum(Absolute.load),
                               OSpan.AbsoluteUnit = mean(Absolute.unit),
                               OSpan.AbsoluteLoad =
                                 sum(Absolute.load) / sum(SetSize),
                               OSpan.Trials = dplyr::n(),
                               OSpan.MemoryItems = sum(SetSize))
  x_processing <- dplyr::filter(x, SubTrialProc == "ProcessingTask")
  x_processing <- dplyr::summarise(x_processing,
                                   Math.ACC = mean(Accuracy, na.rm = TRUE),
                                   Math.RT_mean = mean(RT, na.rm = TRUE),
                                   Math.RT_sd = sd(RT, na.rm = TRUE))
  x <- tryCatch(dplyr::full_join(x_recall, x_processing),
                error = function(c){
                  if (!FALSE) {dplyr::bind_cols(x_recall, x_processing)}
                  else {dplyr::full_join(x_recall, x_processing)}
                })
  x <- dplyr::relocate(x, OSpan.Trials, OSpan.MemoryItems,
                       .after = dplyr::last_col())

  return(x)
}
