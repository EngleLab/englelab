#' Creates a "tidy" raw dataframe for the OSpan task
#'
#' @param x dataframe (an imported .emrge file)
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the
#'     Procedure[Block] variable has a different label. (Default = "new")
#' @export
#'

raw_ospan <- function(x, blocks = "", taskVersion = "new"){
  if (taskVersion=="new"){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
  } else if (taskVersion=="old"){
    x <- dplyr::filter(x, `Procedure[Block]`=="SessionProc")
    x <- dplyr::mutate(x, MathACC = NA)
  }

  if (blocks==1){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                TRUE ~ as.double(NA)),
                       Trial = ifelse(!is.na(BlockList1.Sample),
                                      BlockList1.Sample))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" ~ as.double(OPERATION.RT),
                                             SubTrialProc=="Recall" ~ as.double(CollectClick.RT),
                                             TRUE ~ as.double(NA)),
                       erase = ifelse(SubTrialProc=="Recall" & WordSelection=="clear", 1, NA),
                       erase = zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                       erase = ifelse(SubTrialProc=="ProcessingTask", NA, erase),
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ 1,
                                                 TRUE ~ as.double(NA)),
                       MathDuration = ifelse(!is.na(MathDuration)&MathDuration=="?", NA, MathDuration))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, letterstimuli, NA),
                       position_1 = zoo::na.locf(position_1, na.rm = FALSE),
                       position_2 = ifelse(SubTrial==2, letterstimuli, NA),
                       position_2 = zoo::na.locf(position_2, na.rm = FALSE),
                       position_3 = ifelse(SubTrial==3, letterstimuli, NA),
                       position_3 = zoo::na.locf(position_3, na.rm = FALSE),
                       position_4 = ifelse(SubTrial==4, letterstimuli, NA),
                       position_4 = zoo::na.locf(position_4, na.rm = FALSE),
                       position_5 = ifelse(SubTrial==5, letterstimuli, NA),
                       position_5 = zoo::na.locf(position_5, na.rm = FALSE),
                       position_6 = ifelse(SubTrial==6, letterstimuli, NA),
                       position_6 = zoo::na.locf(position_6, na.rm = FALSE),
                       position_7 = ifelse(SubTrial==7, letterstimuli, NA),
                       position_7 = zoo::na.locf(position_7, na.rm = FALSE),
                       position_8 = ifelse(SubTrial==8, letterstimuli, NA),
                       position_8 = zoo::na.locf(position_8, na.rm = FALSE),
                       position_9 = ifelse(SubTrial==9, letterstimuli, NA),
                       position_9 = zoo::na.locf(position_9, na.rm = FALSE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.character(position_1),
                                                      serial.position==2 ~ as.character(position_2),
                                                      serial.position==3 ~ as.character(position_3),
                                                      serial.position==4 ~ as.character(position_4),
                                                      serial.position==5 ~ as.character(position_5),
                                                      serial.position==6 ~ as.character(position_6),
                                                      serial.position==7 ~ as.character(position_7),
                                                      serial.position==8 ~ as.character(position_8),
                                                      serial.position==9 ~ as.character(position_9),
                                                      TRUE ~ as.character(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" ~ as.double(OPERATION.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.double(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.double(0),
                                                   TRUE ~ as.double((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.AbsoluteUnit = OspanAbsoluteUnitScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.PartialUnit = OspanPartialUnitScore,
                         OSpan.PartialUnit_Block1 = OspanPartialUnitScoreBlock1,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    }
  } else if (blocks==2){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ as.double(1),
                                                `Running[Trial]`=="BlockList2" ~ as.double(2),
                                                TRUE ~ as.double(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                TRUE ~ as.double(NA)))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" |
                                                         `Procedure[SubTrial]`=="TrialProc1" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" |
                                                         `Procedure[SubTrial]`=="recall1" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(OPERATION.RT),
                                             SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(OPERATION1.RT),
                                             SubTrialProc=="Recall" & Block==1 ~ as.double(CollectClick.RT),
                                             SubTrialProc=="Recall" & Block==2 ~ as.double(CollectClick2.RT),
                                             TRUE ~ as.double(NA)),
                       erase = ifelse(SubTrialProc=="Recall" & WordSelection=="clear", 1, NA),
                       erase = zoo::na.locf(erase, fromLast = TRUE, na.rm = FALSE),
                       erase = ifelse(SubTrialProc=="ProcessingTask", NA, erase),
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ as.double(1),
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ as.double(1),
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ as.double(1),
                                                 TRUE ~ as.double(NA)),
                       MathDuration = ifelse(!is.na(MathDuration)&MathDuration=="?", NA, MathDuration))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, letterstimuli, NA),
                       position_1 = zoo::na.locf(position_1, na.rm = FALSE),
                       position_2 = ifelse(SubTrial==2, letterstimuli, NA),
                       position_2 = zoo::na.locf(position_2, na.rm = FALSE),
                       position_3 = ifelse(SubTrial==3, letterstimuli, NA),
                       position_3 = zoo::na.locf(position_3, na.rm = FALSE),
                       position_4 = ifelse(SubTrial==4, letterstimuli, NA),
                       position_4 = zoo::na.locf(position_4, na.rm = FALSE),
                       position_5 = ifelse(SubTrial==5, letterstimuli, NA),
                       position_5 = zoo::na.locf(position_5, na.rm = FALSE),
                       position_6 = ifelse(SubTrial==6, letterstimuli, NA),
                       position_6 = zoo::na.locf(position_6, na.rm = FALSE),
                       position_7 = ifelse(SubTrial==7, letterstimuli, NA),
                       position_7 = zoo::na.locf(position_7, na.rm = FALSE),
                       position_8 = ifelse(SubTrial==8, letterstimuli, NA),
                       position_8 = zoo::na.locf(position_8, na.rm = FALSE),
                       position_9 = ifelse(SubTrial==9, letterstimuli, NA),
                       position_9 = zoo::na.locf(position_9, na.rm = FALSE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.character(position_1),
                                                      serial.position==2 ~ as.character(position_2),
                                                      serial.position==3 ~ as.character(position_3),
                                                      serial.position==4 ~ as.character(position_4),
                                                      serial.position==5 ~ as.character(position_5),
                                                      serial.position==6 ~ as.character(position_6),
                                                      serial.position==7 ~ as.character(position_7),
                                                      serial.position==8 ~ as.character(position_8),
                                                      serial.position==9 ~ as.character(position_9),
                                                      TRUE ~ as.character(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(OPERATION.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(OPERATION1.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.double(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.double(0),
                                                   TRUE ~ as.double((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.AbsoluteUnit = OspanAbsoluteUnitScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                         OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                         OSpan.PartialUnit = OspanPartialUnitScore,
                         OSpan.PartialUnit_Block1 = OspanPartialUnitScoreBlock1,
                         OSpan.PartialUnit_Block2 = OspanPartialUnitScoreBlock2,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                         OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==3){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                `Running[Trial]`=="BlockList2" ~ 2,
                                                `Running[Trial]`=="BlockList3" ~ 3,
                                                TRUE ~ as.double(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                Block==3 ~ BlockList3.Sample,
                                                TRUE ~ as.double(NA)))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrialProc = dplyr::case_when(`Procedure[SubTrial]`=="TrialProc" |
                                                         `Procedure[SubTrial]`=="TrialProc1" |
                                                         `Procedure[SubTrial]`=="TrialProc2" ~ "ProcessingTask",
                                                       `Procedure[SubTrial]`=="recall" |
                                                         `Procedure[SubTrial]`=="recall1" |
                                                         `Procedure[SubTrial]`=="recall2" ~ "Recall",
                                                       TRUE ~ as.character(NA)),
                       RT = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(OPERATION.RT),
                                             SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(OPERATION1.RT),
                                             SubTrialProc=="ProcessingTask" & Block==3 ~ as.double(OPERATION2.RT),
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
                                                 TRUE ~ as.double(NA)),
                       MathDuration = ifelse(!is.na(MathDuration)&MathDuration=="?", NA, MathDuration))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, letterstimuli, NA),
                       position_1 = zoo::na.locf(position_1, na.rm = FALSE),
                       position_2 = ifelse(SubTrial==2, letterstimuli, NA),
                       position_2 = zoo::na.locf(position_2, na.rm = FALSE),
                       position_3 = ifelse(SubTrial==3, letterstimuli, NA),
                       position_3 = zoo::na.locf(position_3, na.rm = FALSE),
                       position_4 = ifelse(SubTrial==4, letterstimuli, NA),
                       position_4 = zoo::na.locf(position_4, na.rm = FALSE),
                       position_5 = ifelse(SubTrial==5, letterstimuli, NA),
                       position_5 = zoo::na.locf(position_5, na.rm = FALSE),
                       position_6 = ifelse(SubTrial==6, letterstimuli, NA),
                       position_6 = zoo::na.locf(position_6, na.rm = FALSE),
                       position_7 = ifelse(SubTrial==7, letterstimuli, NA),
                       position_7 = zoo::na.locf(position_7, na.rm = FALSE),
                       position_8 = ifelse(SubTrial==8, letterstimuli, NA),
                       position_8 = zoo::na.locf(position_8, na.rm = FALSE),
                       position_9 = ifelse(SubTrial==9, letterstimuli, NA),
                       position_9 = zoo::na.locf(position_9, na.rm = FALSE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.character(position_1),
                                                      serial.position==2 ~ as.character(position_2),
                                                      serial.position==3 ~ as.character(position_3),
                                                      serial.position==4 ~ as.character(position_4),
                                                      serial.position==5 ~ as.character(position_5),
                                                      serial.position==6 ~ as.character(position_6),
                                                      serial.position==7 ~ as.character(position_7),
                                                      serial.position==8 ~ as.character(position_8),
                                                      serial.position==9 ~ as.character(position_9),
                                                      TRUE ~ as.character(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.double(OPERATION.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.double(OPERATION1.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==3 ~ as.double(OPERATION2.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.double(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.double(0),
                                                   TRUE ~ as.double((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.total = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.total = stats::ave(Processing.total, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.total = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.total = stats::ave(Recall.total, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.AbsoluteUnit = OspanAbsoluteUnitScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                         OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                         OSpan.Partial_Block3 = OspanPartialScoreBlock3,
                         OSpan.PartialUnit = OspanPartialUnitScore,
                         OSpan.PartialUnit_Block1 = OspanPartialUnitScoreBlock1,
                         OSpan.PartialUnit_Block2 = OspanPartialUnitScoreBlock2,
                         OSpan.PartialUnit_Block3 = OspanPartialUnitScoreBlock3,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.total, Recall.total,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                         OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                         OSpan.Partial_Block3 = OspanPartialScoreBlock3,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
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


#' Calculate OSpan scores from a messy raw dataframe
#'
#' This function skips the 'raw_ospan()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_ospan()'
#' @param x dataframe (an imported .emrge file)
#' @param blocks number of blocks administered. From 1-3
#' @export
#'

score_ospan <- function(x, blocks = ""){
  if (blocks==1){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       OSpan.Absolute = OspanAbsoluteScore,
                       OSpan.Partial = OspanPartialScore,
                       OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                       OSpan.MathACC = MathACC,
                       OSpan.MathDuration = MathDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==2){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       OSpan.Absolute = OspanAbsoluteScore,
                       OSpan.Partial = OspanPartialScore,
                       OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                       OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                       OSpan.MathACC = MathACC,
                       OSpan.MathDuration = MathDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==3){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       OSpan.Absolute = OspanAbsoluteScore,
                       OSpan.Partial = OspanPartialScore,
                       OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                       OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                       OSpan.Partial_Block3 = OspanPartialScoreBlock3,
                       OSpan.MathACC = MathACC,
                       OSpan.MathDuration = MathDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==""){
    warning('Need to specify the number of blocks')
  } else if (blocks>3|blocks<1){
    warning('Invalid number of blocks specified')
  }
  return(x)
}
