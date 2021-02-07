#' Creates a "tidy" raw dataframe for the Operation Span task
#'
#' @param x dataframe
#' @param blocks depricated. No need to indicate number of blocks
#' @param taskVersion old or new version. Required for different types of
#'     data files. First try leaving out the argument, if it does not work
#'     then try taskVersion = "old".
#' @param keep_col List of extra columns to keep
#' @export
#'

raw_ospan <- function(x, blocks = NULL, taskVersion = "new", keep_col = c()){
  if (taskVersion == "new"){
    x <- dplyr::filter(x, `Procedure[Block]` == "TaskProc")
    if (!("AvgMathTime" %in% colnames(x))) {
      x <- dplyr::mutate(x, AvgMathTime = NA)
    }
  } else if (taskVersion == "old"){
    x <- dplyr::filter(x, `Procedure[Block]` == "SessionProc")
    x <- dplyr::mutate(x, MathACC = NA, AvgMathTime = NA)
  }

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

  blocks <- length(unique(x$Block))

  if (blocks == 1){
    x <- dplyr::mutate(x, Trial = ifelse(!is.na(BlockList1.Sample),
                                         BlockList1.Sample))
  } else if (blocks == 2) {
    x <-
      dplyr::mutate(x,
                    Trial = dplyr::case_when(Block == 1 ~ BlockList1.Sample,
                                             Block == 2 ~ BlockList2.Sample,
                                             TRUE ~ as.numeric(NA)),
                    OPERATION.RT =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~ as.double(OPERATION.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~ as.double(OPERATION1.RT),
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
                    Trial = dplyr::case_when(Block == 1 ~ BlockList1.Sample,
                                             Block == 2 ~ BlockList2.Sample,
                                             Block == 3 ~ BlockList3.Sample,
                                             TRUE ~ as.numeric(NA)),
                    OPERATION.RT =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~ as.double(OPERATION.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~ as.double(OPERATION1.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 3 ~ as.double(OPERATION2.RT),
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
  x <- dplyr::group_by(x, Subject, Block, Trial)
  x <- dplyr::mutate(x,
                     RT =
                       dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                          as.double(OPERATION.RT),
                                        SubTrialProc == "Recall" ~
                                          as.double(CollectClick.RT),
                                        TRUE ~ as.double(NA)),
                     erase = ifelse((SubTrialProc == "Recall" &
                                       WordSelection == "clear") |
                                      (SubTrialProc == "Recall" &
                                         WordSelection == "Clear"), 1, NA),
                     erase =
                       zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                     erase =
                       ifelse(SubTrialProc == "ProcessingTask", NA, erase),
                     remove =
                       dplyr::case_when(SubTrialProc == "Recall" &
                                          is.na(WordSelection) ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "Enter" ~ 1,
                                        SubTrialProc == "Recall" &
                                          WordSelection == "InvalidResponse" ~
                                          1,
                                        TRUE ~ as.double(NA)),
                     AvgMathTime =
                       ifelse(!is.na(AvgMathTime) &
                                AvgMathTime == "?", NA, AvgMathTime))

  x <- dplyr::filter(x, is.na(erase), is.na(remove))
  x <- dplyr::group_by(x, Subject, Block, Trial)
  x <- dplyr::mutate(x,
                     SubTrial = dplyr::row_number(),
                     serial.position = SubTrial - setsz,
                     position_1 = ifelse(SubTrial == 1, letterstimuli, NA),
                     position_1 = zoo::na.locf(position_1, na.rm = FALSE),
                     position_2 = ifelse(SubTrial == 2, letterstimuli, NA),
                     position_2 = zoo::na.locf(position_2, na.rm = FALSE),
                     position_3 = ifelse(SubTrial == 3, letterstimuli, NA),
                     position_3 = zoo::na.locf(position_3, na.rm = FALSE),
                     position_4 = ifelse(SubTrial == 4, letterstimuli, NA),
                     position_4 = zoo::na.locf(position_4, na.rm = FALSE),
                     position_5 = ifelse(SubTrial == 5, letterstimuli, NA),
                     position_5 = zoo::na.locf(position_5, na.rm = FALSE),
                     position_6 = ifelse(SubTrial == 6, letterstimuli, NA),
                     position_6 = zoo::na.locf(position_6, na.rm = FALSE),
                     position_7 = ifelse(SubTrial == 7, letterstimuli, NA),
                     position_7 = zoo::na.locf(position_7, na.rm = FALSE),
                     position_8 = ifelse(SubTrial == 8, letterstimuli, NA),
                     position_8 = zoo::na.locf(position_8, na.rm = FALSE),
                     position_9 = ifelse(SubTrial == 9, letterstimuli, NA),
                     position_9 = zoo::na.locf(position_9, na.rm = FALSE),
                     memory_item =
                       dplyr::case_when(serial.position == 1 ~
                                          as.character(position_1),
                                        serial.position == 2 ~
                                          as.character(position_2),
                                        serial.position == 3 ~
                                          as.character(position_3),
                                        serial.position == 4 ~
                                          as.character(position_4),
                                        serial.position == 5 ~
                                          as.character(position_5),
                                        serial.position == 6 ~
                                          as.character(position_6),
                                        serial.position == 7 ~
                                          as.character(position_7),
                                        serial.position == 8 ~
                                          as.character(position_8),
                                        serial.position == 9 ~
                                          as.character(position_9),
                                        TRUE ~ as.character(NA)),
                     CorrectResponse =
                       ifelse(SubTrialProc == "ProcessingTask",
                              as.character(`CorrectAnswer[SubTrial]`),
                              ifelse(SubTrialProc == "Recall",
                                     as.character(memory_item), NA)),
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
                     MemoryItem = letterstimuli,
                     Processing.correct =
                       ifelse(SubTrialProc == "ProcessingTask", Accuracy, NA),
                     Processing.correct =
                       stats::ave(Processing.correct,
                                  FUN = function(x) sum(x, na.rm = TRUE)),
                     Recall.correct =
                       ifelse(SubTrialProc == "Recall", Accuracy, NA),
                     Recall.correct =
                       stats::ave(Recall.correct,
                                  FUN = function(x) sum(x, na.rm = TRUE)),
                     Partial.unit = Recall.correct / SetSize,
                     Absolute.unit = ifelse(Recall.correct == SetSize, 1, 0),
                     Partial.load = Recall.correct,
                     Absolute.load =
                       ifelse(Recall.correct == SetSize, Recall.correct, 0))
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, Recall.correct,
                     Processing.correct, SubTrial, SubTrialProc,
                     RT, Accuracy, Response, CorrectResponse, MemoryItem,
                     keep_col, SessionDate, SessionTime)
  x <- dplyr::distinct(x)
  return(x)
}


#' Calculate Operation Span scores from a messy raw dataframe
#'
#' This function skips the 'raw_ospan()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_ospan()'
#' @param x dataframe
#' @param blocks depricated. No need to indicate number of blocks.
#'     Use group_by(Subject, Block) instead
#' @param keep_col List of extra columns to keep
#' @export
#'

score_ospan <- function(x, blocks = "", keep_col = c()){
  if ("Running[Trial]" %in% colnames(x)) {
    x <- englelab::raw_ospan(x, keep_col = keep_col)
  }

  x <- dplyr::distinct(x, Subject, Block, Trial, Recall.total, SetSize,
                       Partial.unit, Absolute.unit, Partial.load, Absolute.load)
  x <- dplyr::summarise(x,
                        OSpan.PartialUnit = sum(Partial.unit) / n(),
                        OSpan.AbsoluteUnit = sum(Absolute.unit) / n(),
                        OSpan.PartialLoad = sum(Partial.load) / sum(SetSize),
                        OSpan.AbsoluteLoad = sum(Absolute.load) / sum(SetSize),
                        OSpan.Trials = n(),
                        OSpan.MemoryItems = sum(SetSize))
  return(x)
}
