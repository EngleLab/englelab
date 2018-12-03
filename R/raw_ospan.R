#' A Task Scoring Function
#'
#' Creates a raw data file of the OSPAN task from an E-Merged file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the Procedure[Block] variable has a different label. (Default = "new")
#' @keywords score
#' @export
#' @examples
#' raw_ospan(data, blocks = 2)

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
                                                TRUE ~ as.numeric(NA)),
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
                                                 TRUE ~ as.numeric(NA)),
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
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" ~ as.integer(OPERATION.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.correct = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.correct = stats::ave(Processing.correct, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.correct = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.correct = stats::ave(Recall.correct, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
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
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    }
  } else if (blocks==2){
    x <- dplyr::mutate(x,
                       Block = dplyr::case_when(`Running[Trial]`=="BlockList1" ~ 1,
                                                `Running[Trial]`=="BlockList2" ~ 2,
                                                TRUE ~ as.numeric(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                TRUE ~ as.integer(NA)))
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
                       remove = dplyr::case_when(SubTrialProc=="Recall" & is.na(WordSelection) ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="Enter" ~ 1,
                                                 SubTrialProc=="Recall" & WordSelection=="InvalidResponse" ~ 1,
                                                 TRUE ~ as.numeric(NA)),
                       MathDuration = ifelse(!is.na(MathDuration)&MathDuration=="?", NA, MathDuration))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, letterstimuli, NA),
                       position_1 = max(position_1, na.rm = TRUE),
                       position_2 = ifelse(SubTrial==2, letterstimuli, NA),
                       position_2 = max(position_2, na.rm = TRUE),
                       position_3 = ifelse(SubTrial==3, letterstimuli, NA),
                       position_3 = max(position_3, na.rm = TRUE),
                       position_4 = ifelse(SubTrial==4, letterstimuli, NA),
                       position_4 = max(position_4, na.rm = TRUE),
                       position_5 = ifelse(SubTrial==5, letterstimuli, NA),
                       position_5 = max(position_5, na.rm = TRUE),
                       position_6 = ifelse(SubTrial==6, letterstimuli, NA),
                       position_6 = max(position_6, na.rm = TRUE),
                       position_7 = ifelse(SubTrial==7, letterstimuli, NA),
                       position_7 = max(position_7, na.rm = TRUE),
                       position_8 = ifelse(SubTrial==8, letterstimuli, NA),
                       position_8 = max(position_8, na.rm = TRUE),
                       position_9 = ifelse(SubTrial==9, letterstimuli, NA),
                       position_9 = max(position_9, na.rm = TRUE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.integer(position_1),
                                                      serial.position==2 ~ as.integer(position_2),
                                                      serial.position==3 ~ as.integer(position_3),
                                                      serial.position==4 ~ as.integer(position_4),
                                                      serial.position==5 ~ as.integer(position_5),
                                                      serial.position==6 ~ as.integer(position_6),
                                                      serial.position==7 ~ as.integer(position_7),
                                                      serial.position==8 ~ as.integer(position_8),
                                                      serial.position==9 ~ as.integer(position_9),
                                                      TRUE ~ as.integer(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.integer(OPERATION.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.integer(OPERATION1.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.correct = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.correct = stats::ave(Processing.correct, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.correct = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.correct = stats::ave(Recall.correct, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
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
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
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
                                                TRUE ~ as.numeric(NA)),
                       Trial = dplyr::case_when(Block==1 ~ BlockList1.Sample,
                                                Block==2 ~ BlockList2.Sample,
                                                Block==3 ~ BlockList3.Sample,
                                                TRUE ~ as.integer(NA)))
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
                                                 TRUE ~ as.numeric(NA)),
                       MathDuration = ifelse(!is.na(MathDuration)&MathDuration=="?", NA, MathDuration))

    x <- dplyr::filter(x, is.na(erase), is.na(remove))
    x <- dplyr::group_by(x, Subject, Block, Trial)
    x <- dplyr::mutate(x,
                       SubTrial = dplyr::row_number(),
                       serial.position = SubTrial - setsz,
                       position_1 = ifelse(SubTrial==1, letterstimuli, NA),
                       position_1 = max(position_1, na.rm = TRUE),
                       position_2 = ifelse(SubTrial==2, letterstimuli, NA),
                       position_2 = max(position_2, na.rm = TRUE),
                       position_3 = ifelse(SubTrial==3, letterstimuli, NA),
                       position_3 = max(position_3, na.rm = TRUE),
                       position_4 = ifelse(SubTrial==4, letterstimuli, NA),
                       position_4 = max(position_4, na.rm = TRUE),
                       position_5 = ifelse(SubTrial==5, letterstimuli, NA),
                       position_5 = max(position_5, na.rm = TRUE),
                       position_6 = ifelse(SubTrial==6, letterstimuli, NA),
                       position_6 = max(position_6, na.rm = TRUE),
                       position_7 = ifelse(SubTrial==7, letterstimuli, NA),
                       position_7 = max(position_7, na.rm = TRUE),
                       position_8 = ifelse(SubTrial==8, letterstimuli, NA),
                       position_8 = max(position_8, na.rm = TRUE),
                       position_9 = ifelse(SubTrial==9, letterstimuli, NA),
                       position_9 = max(position_9, na.rm = TRUE),
                       memory_item = dplyr::case_when(serial.position==1 ~ as.integer(position_1),
                                                      serial.position==2 ~ as.integer(position_2),
                                                      serial.position==3 ~ as.integer(position_3),
                                                      serial.position==4 ~ as.integer(position_4),
                                                      serial.position==5 ~ as.integer(position_5),
                                                      serial.position==6 ~ as.integer(position_6),
                                                      serial.position==7 ~ as.integer(position_7),
                                                      serial.position==8 ~ as.integer(position_8),
                                                      serial.position==9 ~ as.integer(position_9),
                                                      TRUE ~ as.integer(NA)),
                       CorrectResponse = ifelse(SubTrialProc=="ProcessingTask",
                                                as.character(`CorrectAnswer[SubTrial]`),
                                                ifelse(SubTrialProc=="Recall",
                                                       as.character(memory_item), NA)),
                       Accuracy = dplyr::case_when(SubTrialProc=="ProcessingTask" & Block==1 ~ as.integer(OPERATION.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==2 ~ as.integer(OPERATION1.ACC),
                                                   SubTrialProc=="ProcessingTask" & Block==3 ~ as.integer(OPERATION2.ACC),
                                                   SubTrialProc=="Recall" & CorrectResponse==WordSelection ~ as.integer(1),
                                                   SubTrialProc=="Recall" & CorrectResponse!=WordSelection ~ as.integer(0),
                                                   TRUE ~ as.integer((NA))),
                       Response = dplyr::case_when(SubTrialProc=="ProcessingTask" & Accuracy==1 ~ CorrectResponse,
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="TRUE" ~ "FALSE",
                                                   SubTrialProc=="ProcessingTask" & Accuracy==0 & CorrectResponse=="FALSE" ~ "TRUE",
                                                   SubTrialProc=="Recall" ~ as.character(WordSelection),
                                                   TRUE ~ as.character(NA)),
                       MemoryItem = letterstimuli,
                       Processing.correct = ifelse(SubTrialProc=="ProcessingTask", Accuracy, NA),
                       Processing.correct = stats::ave(Processing.correct, FUN = function(x) sum(x, na.rm = TRUE)),
                       Recall.correct = ifelse(SubTrialProc=="Recall", Accuracy, NA),
                       Recall.correct = stats::ave(Recall.correct, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Block, Trial, SetSize = setsz, SubTrial, SubTrialProc,
                         RT, Accuracy, Response,
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
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
                         CorrectResponse, MemoryItem, Processing.correct, Recall.correct,
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
