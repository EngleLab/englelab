#' Creates a "tidy" raw dataframe for the Symmetry Span task
#'
#' @param x dataframe
#' @param blocks depricated. No need to indicate number of blocks
#' @param taskVersion old or new version. Required for different types of
#'     data files. First try leaving out the argument, if it does not work
#'     then try taskVersion = "old".
#' @param keep_col List of extra columns to keep
#' @export
#'

raw_symspan <- function(x, blocks = NULL, taskVersion = "new", keep_col = c()){
  if (taskVersion == "new"){
    x <- dplyr::filter(x, `Procedure[Block]` == "TaskProc")
    if (!("AvgSymmetryTime" %in% colnames(x))) {
      x <- dplyr::mutate(x, AvgSymmetryTime = NA)
    }
  } else if (taskVersion == "old"){
    x <- dplyr::filter(x, `Procedure[Block]` == "SessionProc")
    x <- dplyr::mutate(x, SymmetryACC = NA, AvgSymmetryTime = NA)
  }
  x <- rename(x, SetSize = setsz)
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

  if (blocks == 1){
    x <- dplyr::mutate(x, Trial = ifelse(!is.na(BlockList1.Sample),
                                         BlockList1.Sample))
  } else if (blocks == 2) {
    x <-
      dplyr::mutate(x,
                    Trial = dplyr::case_when(Block == 1 ~ BlockList1.Sample,
                                             Block == 2 ~ BlockList2.Sample,
                                             TRUE ~ as.numeric(NA)),
                    CheckResponse.RT =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" &
                                         Block == 1 ~
                                         as.double(CheckResponse.RT),
                                       SubTrialProc == "ProcessingTask" &
                                         Block == 2 ~
                                         as.double(CheckResponse1.RT),
                                       TRUE ~ as.double(NA)),
                    CollectClick.RT =
                      dplyr::case_when(SubTrialProc == "Recall" &
                                         Block == 1 ~
                                         as.double(CollectClick.RT),
                                       SubTrialProc == "Recall" &
                                         Block == 2 ~
                                         as.double(CollectClick2.RT),
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
                    Trial = dplyr::case_when(Block == 1 ~ BlockList1.Sample,
                                             Block == 2 ~ BlockList2.Sample,
                                             Block == 3 ~ BlockList3.Sample,
                                             TRUE ~ as.numeric(NA)),
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
                                        SubTrialProc == "Recall" ~
                                          as.double(CollectClick.RT),
                                        TRUE ~ as.double(NA)),
                     erase = ifelse(SubTrialProc == "Recall" &
                                      WordSelection == "clear", 1, NA),
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
                                        TRUE ~ as.numeric(NA)),
                     AvgSymmetryTime =
                       ifelse(!is.na(AvgSymmetryTime) &
                                AvgSymmetryTime == "?", NA, AvgSymmetryTime))

  x <- dplyr::filter(x, is.na(erase), is.na(remove))
  x <- dplyr::group_by(x, Subject, Block, Trial)
  suppressWarnings({
    x <-
      dplyr::mutate(x,
                    SubTrial = dplyr::row_number(),
                    serial.position = SubTrial - SetSize,
                    position_1 = ifelse(SubTrial == 1, MatrixId, NA),
                    position_1 = max(position_1, na.rm = TRUE),
                    position_2 = ifelse(SubTrial == 2, MatrixId, NA),
                    position_2 = max(position_2, na.rm = TRUE),
                    position_3 = ifelse(SubTrial == 3, MatrixId, NA),
                    position_3 = max(position_3, na.rm = TRUE),
                    position_4 = ifelse(SubTrial == 4, MatrixId, NA),
                    position_4 = max(position_4, na.rm = TRUE),
                    position_5 = ifelse(SubTrial == 5, MatrixId, NA),
                    position_5 = max(position_5, na.rm = TRUE),
                    position_6 = ifelse(SubTrial == 6, MatrixId, NA),
                    position_6 = max(position_6, na.rm = TRUE),
                    position_7 = ifelse(SubTrial == 7, MatrixId, NA),
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
                      dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                         as.character(`CorrectAnswer[SubTrial]`),
                                       SubTrialProc == "Recall" ~
                                         as.character(memory_item),
                                       TRUE ~ as.character(NA)),
                    Accuracy =
                      dplyr::case_when(SubTrialProc == "ProcessingTask" ~
                                         as.integer(CheckResponse.ACC),
                                       SubTrialProc == "Recall" &
                                         CorrectResponse == WordSelection ~
                                         as.integer(1),
                                       SubTrialProc == "Recall" &
                                         CorrectResponse != WordSelection ~
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
                                         as.character(WordSelection),
                                       TRUE ~ as.character(NA)),
                    MemoryItem = MatrixId,
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
  })

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, Subject, Block, Trial, SetSize, Processing.correct,
                     Recall.correct, Partial.unit, Absolute.unit,
                     Partial.load, Absolute.load, SubTrial, SubTrialProc,
                     RT, Accuracy, Response, CorrectResponse, MemoryItem,
                     keep_col, SessionDate, SessionTime)
  x <- dplyr::distinct(x)
  return(x)
}


#' Calculate Symmetry Span scores from a messy raw dataframe
#'
#' This function skips the 'raw_symspan()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_symspan()'
#' @param x dataframe
#' @param blocks depricated. No need to indicate number of blocks.
#'     Use group_by(Subject, Block) instead
#' @param keep_col List of extra columns to keep
#' @export
#'

score_symspan <- function(x, blocks = NULL, keep_col = c()){
  if ("Running[Trial]" %in% colnames(x)) {
    x <- englelab::raw_symspan(x, keep_col = keep_col)
  }

  x_recall <- dplyr::distinct(x, Subject, Block, Trial, Recall.correct, SetSize,
                              Partial.unit, Absolute.unit,
                              Partial.load, Absolute.load)
  x_recall <- dplyr::summarise(x_recall,
                               SymSpan.PartialUnit = sum(Partial.unit) / n(),
                               SymSpan.AbsoluteUnit = sum(Absolute.unit) / n(),
                               SymSpan.PartialLoad =
                                 sum(Partial.load) / sum(SetSize),
                               SymSpan.AbsoluteLoad =
                                 sum(Absolute.load) / sum(SetSize),
                               SymSpan.Trials = n(),
                               SymSpan.MemoryItems = sum(SetSize))

  x_processing <- dplyr::filter(x, SubTrialProc == "ProcessingTask")
  x_processing <- dplyr::summarise(x_processing,
                                   Symmetry.RT_mean = mean(RT, na.rm = TRUE),
                                   Symmetry.RT_sd = sd(RT, na.rm = TRUE),
                                   Symmetry.ACC = mean(Accuracy, na.rm = TRUE))

  x <- dplyr::full_join(x_recall, x_processing)
  x <- dplyr::relocate(x, SymSpan.Trials, SymSpan.MemoryItems,
                       .after = last_col())
  return(x)
}


