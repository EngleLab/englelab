#' Raw Tidy Data for Rotation Span
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

raw_rotspan <- function(x, include_col = c(), taskVersion = "new") {

  if (taskVersion == "old") {
    x <- dplyr::mutate(x, RotationACC = NA, AvgRotationTime = NA)
  }
  x <- dplyr::filter(x, `Procedure[Block]` == "realBoth")
  if (!("AvgRotationTime" %in% colnames(x))) {
    x <- dplyr::mutate(x, AvgRotationTime = NA)
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
                                           TRUE ~ as.numeric(NA)))

  blocks <- length(unique(x$Block))

  if (blocks == 1) {

  } else if (blocks == 2) {
    x <-
      dplyr::mutate(x,
                    CheckResponse.RT =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~
                                         as.double(CheckResponse.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~
                                         as.double(CheckResponse1.RT),
                                       TRUE ~ as.double(NA)),
                    CheckResponse.ACC =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~
                                         as.integer(CheckResponse.ACC),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~
                                         as.integer(CheckResponse1.ACC),
                                       TRUE ~ as.integer((NA))))
  } else if (blocks == 3) {
    x <-
      dplyr::mutate(x,
                    CheckResponse.RT =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~
                                         as.double(CheckResponse.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~
                                         as.double(CheckResponse1.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 3 ~
                                         as.double(CheckResponse2.RT),
                                       TRUE ~ as.double(NA)),
                    CheckResponse.ACC =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~
                                         as.integer(CheckResponse.ACC),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~
                                         as.integer(CheckResponse1.ACC),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 3 ~
                                         as.integer(CheckResponse2.ACC),
                                       TRUE ~ as.integer((NA))))
  }

  x <- dplyr::group_by(x, Subject, Block, Trial)
  x <- dplyr::mutate(x,
                     RT =
                       dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                          as.double(CheckResponse.RT),
                                        TRUE ~ as.double(NA)),
                     AvgRotationTime =
                       ifelse(!is.na(AvgRotationTime) &
                                AvgRotationTime == "?",
                              as.numeric, AvgRotationTime))

  x <- dplyr::group_by(x, Subject, Block, Trial)
  x <- dplyr::do(x, dplyr::add_row(.,
                                   SubTrial = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                                NA,NA,NA,NA,NA,NA,NA)))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate_at(x, .vars = c("Subject", "Block", "Trial"),
                        .funs = zoo::na.locf, na.rm = FALSE)
  x <- dplyr::group_by(x, Subject, Block, Trial)
  suppressMessages({
    x <- dplyr::mutate_at(x, dplyr::vars(-dplyr::group_cols()),
                          .funs = zoo::na.locf, na.rm = FALSE)
  })
  x <- dplyr::mutate(x,
                     RT = ifelse(SubTrialProc == "Recall", as.numeric(NA), RT),
                     ArrowId =
                       ifelse(SubTrialProc == "Recall", as.numeric(NA), ArrowId),
                     SubTrial = dplyr::row_number())
  x <- dplyr::filter(x, SubTrial <= (SetSize * 2))
  suppressWarnings({
    x <-
      dplyr::mutate(x,
                    serial.position = SubTrial - SetSize,
                    ArrowSelection =
                      dplyr::case_when(serial.position == 1 & Box1 == 1 ~ 1,
                                       serial.position == 2 & Box1 == 2 ~ 1,
                                       serial.position == 3 & Box1 == 3 ~ 1,
                                       serial.position == 4 & Box1 == 4 ~ 1,
                                       serial.position == 5 & Box1 == 5 ~ 1,
                                       serial.position == 6 & Box1 == 6 ~ 1,
                                       serial.position == 7 & Box1 == 7 ~ 1,
                                       serial.position == 1 & Box2 == 1 ~ 2,
                                       serial.position == 2 & Box2 == 2 ~ 2,
                                       serial.position == 3 & Box2 == 3 ~ 2,
                                       serial.position == 4 & Box2 == 4 ~ 2,
                                       serial.position == 5 & Box2 == 5 ~ 2,
                                       serial.position == 6 & Box2 == 6 ~ 2,
                                       serial.position == 7 & Box2 == 7 ~ 2,
                                       serial.position == 1 & Box3 == 1 ~ 3,
                                       serial.position == 2 & Box3 == 2 ~ 3,
                                       serial.position == 3 & Box3 == 3 ~ 3,
                                       serial.position == 4 & Box3 == 4 ~ 3,
                                       serial.position == 5 & Box3 == 5 ~ 3,
                                       serial.position == 6 & Box3 == 6 ~ 3,
                                       serial.position == 7 & Box3 == 7 ~ 3,
                                       serial.position == 1 & Box4 == 1 ~ 4,
                                       serial.position == 2 & Box4 == 2 ~ 4,
                                       serial.position == 3 & Box4 == 3 ~ 4,
                                       serial.position == 4 & Box4 == 4 ~ 4,
                                       serial.position == 5 & Box4 == 5 ~ 4,
                                       serial.position == 6 & Box4 == 6 ~ 4,
                                       serial.position == 7 & Box4 == 7 ~ 4,
                                       serial.position == 1 & Box5 == 1 ~ 5,
                                       serial.position == 2 & Box5 == 2 ~ 5,
                                       serial.position == 3 & Box5 == 3 ~ 5,
                                       serial.position == 4 & Box5 == 4 ~ 5,
                                       serial.position == 5 & Box5 == 5 ~ 5,
                                       serial.position == 6 & Box5 == 6 ~ 5,
                                       serial.position == 7 & Box5 == 7 ~ 5,
                                       serial.position == 1 & Box6 == 1 ~ 6,
                                       serial.position == 2 & Box6 == 2 ~ 6,
                                       serial.position == 3 & Box6 == 3 ~ 6,
                                       serial.position == 4 & Box6 == 4 ~ 6,
                                       serial.position == 5 & Box6 == 5 ~ 6,
                                       serial.position == 6 & Box6 == 6 ~ 6,
                                       serial.position == 7 & Box6 == 7 ~ 6,
                                       serial.position == 1 & Box7 == 1 ~ 7,
                                       serial.position == 2 & Box7 == 2 ~ 7,
                                       serial.position == 3 & Box7 == 3 ~ 7,
                                       serial.position == 4 & Box7 == 4 ~ 7,
                                       serial.position == 5 & Box7 == 5 ~ 7,
                                       serial.position == 6 & Box7 == 6 ~ 7,
                                       serial.position == 7 & Box7 == 7 ~ 7,
                                       serial.position == 1 & Box8 == 1 ~ 8,
                                       serial.position == 2 & Box8 == 2 ~ 8,
                                       serial.position == 3 & Box8 == 3 ~ 8,
                                       serial.position == 4 & Box8 == 4 ~ 8,
                                       serial.position == 5 & Box8 == 5 ~ 8,
                                       serial.position == 6 & Box8 == 6 ~ 8,
                                       serial.position == 7 & Box8 == 7 ~ 8,
                                       serial.position == 1 & Box9 == 1 ~ 9,
                                       serial.position == 2 & Box9 == 2 ~ 9,
                                       serial.position == 3 & Box9 == 3 ~ 9,
                                       serial.position == 4 & Box9 == 4 ~ 9,
                                       serial.position == 5 & Box9 == 5 ~ 9,
                                       serial.position == 6 & Box9 == 6 ~ 9,
                                       serial.position == 7 & Box9 == 7 ~ 9,
                                       serial.position == 1 & Box10 == 1 ~ 10,
                                       serial.position == 2 & Box10 == 2 ~ 10,
                                       serial.position == 3 & Box10 == 3 ~ 10,
                                       serial.position == 4 & Box10 == 4 ~ 10,
                                       serial.position == 5 & Box10 == 5 ~ 10,
                                       serial.position == 6 & Box10 == 6 ~ 10,
                                       serial.position == 7 & Box10 == 7 ~ 10,
                                       serial.position == 1 & Box11 == 1 ~ 11,
                                       serial.position == 2 & Box11 == 2 ~ 11,
                                       serial.position == 3 & Box11 == 3 ~ 11,
                                       serial.position == 4 & Box11 == 4 ~ 11,
                                       serial.position == 5 & Box11 == 5 ~ 11,
                                       serial.position == 6 & Box11 == 6 ~ 11,
                                       serial.position == 7 & Box11 == 7 ~ 11,
                                       serial.position == 1 & Box12 == 1 ~ 12,
                                       serial.position == 2 & Box12 == 2 ~ 12,
                                       serial.position == 3 & Box12 == 3 ~ 12,
                                       serial.position == 4 & Box12 == 4 ~ 12,
                                       serial.position == 5 & Box12 == 5 ~ 12,
                                       serial.position == 6 & Box12 == 6 ~ 12,
                                       serial.position == 7 & Box12 == 7 ~ 12,
                                       serial.position == 1 & Box13 == 1 ~ 13,
                                       serial.position == 2 & Box13 == 2 ~ 13,
                                       serial.position == 3 & Box13 == 3 ~ 13,
                                       serial.position == 4 & Box13 == 4 ~ 13,
                                       serial.position == 5 & Box13 == 5 ~ 13,
                                       serial.position == 6 & Box13 == 6 ~ 13,
                                       serial.position == 7 & Box13 == 7 ~ 13,
                                       serial.position == 1 & Box14 == 1 ~ 14,
                                       serial.position == 2 & Box14 == 2 ~ 14,
                                       serial.position == 3 & Box14 == 3 ~ 14,
                                       serial.position == 4 & Box14 == 4 ~ 14,
                                       serial.position == 5 & Box14 == 5 ~ 14,
                                       serial.position == 6 & Box14 == 6 ~ 14,
                                       serial.position == 7 & Box14 == 7 ~ 14,
                                       serial.position == 1 & Box15 == 1 ~ 15,
                                       serial.position == 2 & Box15 == 2 ~ 15,
                                       serial.position == 3 & Box15 == 3 ~ 15,
                                       serial.position == 4 & Box15 == 4 ~ 15,
                                       serial.position == 5 & Box15 == 5 ~ 15,
                                       serial.position == 6 & Box15 == 6 ~ 15,
                                       serial.position == 7 & Box15 == 7 ~ 15,
                                       serial.position == 1 & Box16 == 1 ~ 16,
                                       serial.position == 2 & Box16 == 2 ~ 16,
                                       serial.position == 3 & Box16 == 3 ~ 16,
                                       serial.position == 4 & Box16 == 4 ~ 16,
                                       serial.position == 5 & Box16 == 5 ~ 16,
                                       serial.position == 6 & Box16 == 6 ~ 16,
                                       serial.position == 7 & Box16 == 7 ~ 16,
                                       TRUE ~ as.numeric(NA)),
                    position_1 = ifelse(SubTrial == 1, ArrowId, as.numeric(NA)),
                    position_1 = max(position_1, na.rm = TRUE),
                    position_2 = ifelse(SubTrial == 2, ArrowId, as.numeric(NA)),
                    position_2 = max(position_2, na.rm = TRUE),
                    position_3 = ifelse(SubTrial == 3, ArrowId, as.numeric(NA)),
                    position_3 = max(position_3, na.rm = TRUE),
                    position_4 = ifelse(SubTrial == 4, ArrowId, as.numeric(NA)),
                    position_4 = max(position_4, na.rm = TRUE),
                    position_5 = ifelse(SubTrial == 5, ArrowId, as.numeric(NA)),
                    position_5 = max(position_5, na.rm = TRUE),
                    position_6 = ifelse(SubTrial == 6, ArrowId, as.numeric(NA)),
                    position_6 = max(position_6, na.rm = TRUE),
                    position_7 = ifelse(SubTrial == 7, ArrowId, as.numeric(NA)),
                    position_7 = max(position_7, na.rm = TRUE),
                    memory_item =
                      dplyr::case_when(serial.position == 1 ~
                                         as.integer(position_1),
                                       serial.position == 2 ~
                                         as.integer(position_2),
                                       serial.position == 3 ~
                                         as.integer(position_3),
                                       serial.position == 4 ~
                                         as.integer(position_4),
                                       serial.position == 5 ~
                                         as.integer(position_5),
                                       serial.position == 6 ~
                                         as.integer(position_6),
                                       serial.position == 7 ~
                                         as.integer(position_7),
                                       TRUE ~ as.integer(NA)),
                    CorrectResponse =
                      ifelse(SubTrialProc == "ProcessingTask",
                             as.character(`correctAnswer[SubTrial]`),
                             ifelse(SubTrialProc == "Recall",
                                    as.character(memory_item), as.character(NA))),
                    Accuracy =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                         as.integer(CheckResponse.ACC),
                                       SubTrialProc == "Recall" &
                                         CorrectResponse == ArrowSelection ~
                                         as.integer(1),
                                       SubTrialProc == "Recall" &
                                         CorrectResponse != ArrowSelection ~
                                         as.integer(0),
                                       TRUE ~ as.integer((NA))),
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
                                         as.character(ArrowSelection),
                                       TRUE ~ as.character(NA)),
                    MemoryItem = ArrowId,
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
                     include_col, SessionDate, SessionTime)
  x <- dplyr::distinct(x)

  # add columns with sequence of target memory and recalled items
  x_tr <- dplyr::mutate(x,
                        MemoryItem = LETTERS[MemoryItem],
                        Response =
                          dplyr::case_when(is.na(Response) ~ as.character(NA),
                                           Response == "TRUE" ~ "TRUE",
                                           Response == "FALSE" ~ "FALSE",
                                           Response == "blank" ~ "-",
                                           TRUE ~ LETTERS[as.numeric(Response)]))
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
  x <- dplyr::select(x, -Recall.correct)

  return(x)
}




#' Calculate Rotation Span Scores
#'
#' Calculate various span scores from the output of `raw_rotspan()`
#' @param x dataframe
#' @export
#'

score_rotspan <- function(x)
  {

  if ("Running[Trial]" %in% colnames(x)) {
    x <- englelab::raw_rotspan(x)
  }

  x_recall <- dplyr::distinct(x, Subject, Block, Trial, SetSize,
                              EditDistance.unit, EditDistance.load,
                              Partial.unit, Partial.load,
                              Absolute.unit, Absolute.load)
  x_recall <- dplyr::summarise(x_recall,
                               RotSpan.EditDistanceScore = sum(EditDistance.load),
                               RotSpan.EditDistanceUnit = mean(EditDistance.unit),
                               RotSpan.EditDistanceLoad =
                                 sum(EditDistance.load) / sum(SetSize),
                               RotSpan.PartialScore = sum(Partial.load),
                               RotSpan.PartialUnit = mean(Partial.unit),
                               RotSpan.PartialLoad =
                                 sum(Partial.load) / sum(SetSize),
                               RotSpan.AbsoluteScore = sum(Absolute.load),
                               RotSpan.AbsoluteUnit = mean(Absolute.unit),
                               RotSpan.AbsoluteLoad =
                                 sum(Absolute.load) / sum(SetSize),
                               RotSpan.Trials = n(),
                               RotSpan.MemoryItems = sum(SetSize))
  x_processing <- dplyr::filter(x, SubTrialProc == "ProcessingTask")
  x_processing <- dplyr::summarise(x_processing,
                                   Rotation.ACC = mean(Accuracy, na.rm = TRUE),
                                   Rotation.RT_mean = mean(RT, na.rm = TRUE),
                                   Rotation.RT_sd = sd(RT, na.rm = TRUE))
  x <- tryCatch(dplyr::full_join(x_recall, x_processing),
                error = function(c){
                  if (!FALSE) {dplyr::bind_cols(x_recall, x_processing)}
                  else {dplyr::full_join(x_recall, x_processing)}
                })
  x <- dplyr::relocate(x, RotSpan.Trials, RotSpan.MemoryItems,
                       .after = dplyr::last_col())

  return(x)
}

