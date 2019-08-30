#' Creates a "tidy" raw dataframe for the SymSpan task
#'
#' @param x dataframe (an imported .emrge file)
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the
#'     Procedure[Block] variable has a different label. (Default = "new")
#' @export
#'

raw_symspan <- function(x, blocks = "", taskVersion = "new"){
  if (taskVersion=="new"){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
  } else if (taskVersion=="old"){
    x <- dplyr::filter(x, `Procedure[Block]`=="SessionProc")
    x <- dplyr::mutate(x, SymmetryACC = NA)
  }

  if (blocks==1){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                TRUE ~ as.numeric(NA)),
                       Trial = ifelse(!is.na(BlockList1.Sample),
                                      BlockList1.Sample))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" ~ as.double(CheckResponse.RT),
                                             SubTrialProc=="Recall" ~ as.double(CollectClick.RT),
                                             TRUE ~ as.double(NA)),
                       erase = ifelse(SubTrialProc=="Recall" & WordSelection=="clear", 1, NA),
                       erase = zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                       erase = ifelse(SubTrialProc=="ProcessingTask", NA, erase),
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ 1,
                                                 TRUE ~ as.numeric(NA)),
                       AvgSymmetryTime = ifelse(!is.na(AvgSymmetryTime)&AvgSymmetryTime=="?", NA, AvgSymmetryTime))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, MatrixId, NA),
                       position_1 = max(position_1, na.rm = TRUE),
                       position_2 = ifelse(SubTrial==2, MatrixId, NA),
                       position_2 = max(position_2, na.rm = TRUE),
                       position_3 = ifelse(SubTrial==3, MatrixId, NA),
                       position_3 = max(position_3, na.rm = TRUE),
                       position_4 = ifelse(SubTrial==4, MatrixId, NA),
                       position_4 = max(position_4, na.rm = TRUE),
                       position_5 = ifelse(SubTrial==5, MatrixId, NA),
                       position_5 = max(position_5, na.rm = TRUE),
                       position_6 = ifelse(SubTrial==6, MatrixId, NA),
                       position_6 = max(position_6, na.rm = TRUE),
                       position_7 = ifelse(SubTrial==7, MatrixId, NA),
                       position_7 = max(position_7, na.rm = TRUE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.integer(position_1),
                                                      serial.position==2 ~ as.integer(position_2),
                                                      serial.position==3 ~ as.integer(position_3),
                                                      serial.position==4 ~ as.integer(position_4),
                                                      serial.position==5 ~ as.integer(position_5),
                                                      serial.position==6 ~ as.integer(position_6),
                                                      serial.position==7 ~ as.integer(position_7),
                                                      TRUE ~ as.integer(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" ~ as.integer(CheckResponse.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = MatrixId,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.AbsoluteUnit = SspanAbsoluteUnitScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.PartialUnit = SspanPartialUnitScore,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    }

  } else if (blocks==2){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                `Running[Trial]`=="BlockList2" ~ 2,
                                                TRUE ~ as.numeric(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                TRUE ~ as.numeric(NA)))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" |
                                                         `Procedure[SubTrial]`=="TrialProc1" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" |
                                                         `Procedure[SubTrial]`=="recall1" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(CheckResponse.RT),
                                             SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(CheckResponse1.RT),
                                             SubTrialProc=="Recall" & Block==1 ~ as.double(CollectClick.RT),
                                             SubTrialProc=="Recall" & Block==2 ~ as.double(CollectClick2.RT),
                                             TRUE ~ as.double(NA)),
                       erase = ifelse(SubTrialProc=="Recall" & WordSelection=="clear", 1, NA),
                       erase = zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                       erase = ifelse(SubTrialProc=="ProcessingTask", NA, erase),
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ 1,
                                                 TRUE ~ as.numeric(NA)),
                       AvgSymmetryTime = ifelse(!is.na(AvgSymmetryTime)&AvgSymmetryTime=="?", NA, AvgSymmetryTime))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, MatrixId, NA),
                       position_1 = max(position_1, na.rm = TRUE),
                       position_2 = ifelse(SubTrial==2, MatrixId, NA),
                       position_2 = max(position_2, na.rm = TRUE),
                       position_3 = ifelse(SubTrial==3, MatrixId, NA),
                       position_3 = max(position_3, na.rm = TRUE),
                       position_4 = ifelse(SubTrial==4, MatrixId, NA),
                       position_4 = max(position_4, na.rm = TRUE),
                       position_5 = ifelse(SubTrial==5, MatrixId, NA),
                       position_5 = max(position_5, na.rm = TRUE),
                       position_6 = ifelse(SubTrial==6, MatrixId, NA),
                       position_6 = max(position_6, na.rm = TRUE),
                       position_7 = ifelse(SubTrial==7, MatrixId, NA),
                       position_7 = max(position_7, na.rm = TRUE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.integer(position_1),
                                                      serial.position==2 ~ as.integer(position_2),
                                                      serial.position==3 ~ as.integer(position_3),
                                                      serial.position==4 ~ as.integer(position_4),
                                                      serial.position==5 ~ as.integer(position_5),
                                                      serial.position==6 ~ as.integer(position_6),
                                                      serial.position==7 ~ as.integer(position_7),
                                                      TRUE ~ as.integer(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.integer(CheckResponse.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.integer(CheckResponse1.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = MatrixId,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.AbsoluteUnit = SspanAbsoluteUnitScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.PartialUnit = SspanPartialUnitScore,
                         SymSpan.PartialUnit_Block1 = SspanPartialUnitScoreBlock1,
                         SymSpan.PartialUnit_Block2 = SspanPartialUnitScoreBlock2,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    }

  } else if (blocks==3){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                `Running[Trial]`=="BlockList2" ~ 2,
                                                `Running[Trial]`=="BlockList3" ~ 3,
                                                TRUE ~ as.numeric(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                Block==3 ~ BlockList3.Sample,
                                                TRUE ~ as.numeric(NA)))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" |
                                                         `Procedure[SubTrial]`=="TrialProc1" |
                                                         `Procedure[SubTrial]`=="TrialProc2" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" |
                                                         `Procedure[SubTrial]`=="recall1" |
                                                         `Procedure[SubTrial]`=="recall2" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(CheckResponse.RT),
                                             SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(CheckResponse1.RT),
                                             SubTrialProc=="ProcessingTask" & Block==3 ~ as.double(CheckResponse2.RT),
                                             SubTrialProc=="Recall" & Block==1 ~ as.double(CollectClick.RT),
                                             SubTrialProc=="Recall" & Block==2 ~ as.double(CollectClick2.RT),
                                             SubTrialProc=="Recall" & Block==3 ~ as.double(CollectClick3.RT),
                                             TRUE ~ as.double(NA)),
                       erase = ifelse(SubTrialProc=="Recall" & WordSelection=="clear", 1, NA),
                       erase = zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                       erase = ifelse(SubTrialProc=="ProcessingTask", NA, erase),
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ 1,
                                                 TRUE ~ as.numeric(NA)),
                       AvgSymmetryTime = ifelse(!is.na(AvgSymmetryTime)&AvgSymmetryTime=="?", NA, AvgSymmetryTime))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, MatrixId, NA),
                       position_1 = max(position_1, na.rm = TRUE),
                       position_2 = ifelse(SubTrial==2, MatrixId, NA),
                       position_2 = max(position_2, na.rm = TRUE),
                       position_3 = ifelse(SubTrial==3, MatrixId, NA),
                       position_3 = max(position_3, na.rm = TRUE),
                       position_4 = ifelse(SubTrial==4, MatrixId, NA),
                       position_4 = max(position_4, na.rm = TRUE),
                       position_5 = ifelse(SubTrial==5, MatrixId, NA),
                       position_5 = max(position_5, na.rm = TRUE),
                       position_6 = ifelse(SubTrial==6, MatrixId, NA),
                       position_6 = max(position_6, na.rm = TRUE),
                       position_7 = ifelse(SubTrial==7, MatrixId, NA),
                       position_7 = max(position_7, na.rm = TRUE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.integer(position_1),
                                                      serial.position==2 ~ as.integer(position_2),
                                                      serial.position==3 ~ as.integer(position_3),
                                                      serial.position==4 ~ as.integer(position_4),
                                                      serial.position==5 ~ as.integer(position_5),
                                                      serial.position==6 ~ as.integer(position_6),
                                                      serial.position==7 ~ as.integer(position_7),
                                                      TRUE ~ as.integer(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.integer(CheckResponse.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.integer(CheckResponse1.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==3 ~ as.integer(CheckResponse2.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = MatrixId,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.AbsoluteUnit = SspanAbsoluteUnitScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.Partial_Block3 = SspanPartialScoreBlock3,
                         SymSpan.PartialUnit = SspanPartialUnitScore,
                         SymSpan.PartialUnit_Block1 = SspanPartialUnitScoreBlock1,
                         SymSpan.PartialUnit_Block2 = SspanPartialUnitScoreBlock2,
                         SymSpan.PartialUnit_Block3 = SspanPartialUnitScoreBlock3,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.Partial_Block3 = SspanPartialScoreBlock3,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.AvgSymmetryTime = AvgSymmetryTime,
                         SessionDate, SessionTime)
    }

  } else if (blocks==""){
    warning('Need to specify the number of blocks')
  } else if (blocks>3|blocks<1){
    warning('Invalid number of blocks specified')
  }
  x <- dplyr::distinct(x)
  return(x)
}


#' Calculate SymSpan scores from a messy raw dataframe
#'
#' This function skips the 'raw_symspan()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_symspan()'
#' @param x dataframe (an imported .emrge file)
#' @param blocks number of blocks administered. From 1-3
#' @export
#'

score_symspan <- function(x, blocks = ""){
  if (blocks==1){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.AvgSymmetryTime = AvgSymmetryTime)
    x <- dplyr::distinct(x)
  } else if (blocks==2){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.AvgSymmetryTime = AvgSymmetryTime)
    x <- dplyr::distinct(x)
  } else if (blocks==3){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                       SymSpan.Partial_Block3 = SspanPartialScoreBlock3,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.AvgSymmetryTime = AvgSymmetryTime)
    x <- dplyr::distinct(x)
  } else if (blocks==""){
    warning('Need to specify the number of blocks')
  } else if (blocks>3|blocks<1){
    warning('Invalid number of blocks specified')
  }
  return(x)
}


