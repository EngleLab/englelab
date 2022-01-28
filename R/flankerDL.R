#' Creates a "tidy" raw dataframe for the FlankerDL task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

raw_flankerDL <- function(x){
  x <- dplyr::rename(x,
                     TrialProc = `Procedure[Trial]`,
                     ResponseDeadline = ArrowDuration,
                     FixationDuration = DurationOfFixation)
  x <- dplyr::filter(x, TrialProc == "TrialProc" |
                       TrialProc == "PracTrialProc" |
                       TrialProc == "MapTrialProc")
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x,
                     TrialProc =
                       dplyr::case_when(TrialProc == "TrialProc" ~
                                          "real",
                                        TrialProc == "MapTrialProc" ~
                                          "practice",
                                        TrialProc == "PracTrialProc" ~
                                          "practice"),
                     Block = dplyr::case_when(TrialProc == "real" ~
                                                TrialList.Cycle,
                                              TrialProc == "practice" ~
                                                as.numeric(NA)),
                     RT =
                       dplyr::case_when(TrialProc == "real" &
                                          SlideTarget.RT == 0 ~
                                          MissedDeadline.RT + ResponseDeadline,
                                        TrialProc == "real" &
                                          SlideTarget.RT > 0 ~ SlideTarget.RT,
                                        TrialProc == "practice" ~
                                          PracSlideTarget1.RT),
                     Accuracy =
                       dplyr::case_when(TrialProc == "real" &
                                          is.na(SlideTarget.RESP) ~
                                          MissedDeadline.ACC,
                                        TrialProc == "real" &
                                          !is.na(SlideTarget.RESP) ~
                                          SlideTarget.ACC,
                                        TrialProc == "practice" ~
                                          PracSlideTarget1.ACC),
                     Response =
                       dplyr::case_when(TrialProc == "real" &
                                          is.na(SlideTarget.RESP) ~
                                          MissedDeadline.RESP,
                                        TrialProc == "real" &
                                          !is.na(SlideTarget.RESP) ~
                                          SlideTarget.RESP,
                                        TrialProc == "practice" ~
                                          PracSlideTarget1.RESP),
                     Response = dplyr::case_when(Response == "z" ~ "left",
                                                 Response == "{/}" ~ "right",
                                                 TRUE ~ as.character(NA)),
                     MissedDeadline = ifelse(TrialProc == "real" &
                                               is.na(SlideTarget.RESP), 1, 0),
                     TrialCriteria.ACC =
                       dplyr::case_when(TrialProc == "real" ~ SlideTarget.ACC,
                                        TrialProc == "practice" ~ 0),
                     TargetArrowDirection =
                       dplyr::case_when(TrialProc == "real" ~
                                          TargetDirection,
                                        TrialProc == "practice" ~
                                          TargerDirection),
                     StartTime = min(PracSlideFixationStart.OnsetTime, na.rm = TRUE),
                     FinishTime = max(SlideFixationEnd1.OnsetTime, na.rm = TRUE),
                     AdminTime = (FinishTime - StartTime) / 60000)
  x <- dplyr::ungroup(x)

  x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition = FlankerType, RT, MissedDeadline, Accuracy,
                       Response, TrialCriteria.ACC, TargetArrowDirection,
                       FixationDuration, FlankerDLScore = ArrowDLTime,
                       FlankerMissedDeadlines = ArrowMissedDeadlines,
                       FlankerDLTotalAccuracy = ArrowDLTotalAccuracy,
                       FlankerDLCorrectRT = ArrowDLCorrectRT,
                       FlankerDLTotalRT = ArrowDLTotalRT,
                       AdminTime, SessionDate, SessionTime)

  x_block <- dplyr::filter(x, TrialProc == "real")
  x_block <- dplyr::group_by(x, Subject, Block)
  x_block <- dplyr::mutate(x_block,
                           Block.Correct = sum(TrialCriteria.ACC, na.rm = TRUE),
                           BlockCriteria.Reach = ifelse(Block.Correct > 14, 1, 0))
  x_block <- dplyr::filter(x_block, Trial == 1, !is.na(Block))
  x_block <- dplyr::group_by(x_block, Subject, TrialProc)
  x_block <- dplyr::mutate(x_block,
                           PrevBlockCriteria.Reach = dplyr::lag(BlockCriteria.Reach, 1, 0),
                           Reversal = ifelse(PrevBlockCriteria.Reach != BlockCriteria.Reach,
                                             1, 0))
  x_block <- dplyr::group_by(x_block, Subject, Reversal)
  x_block <- dplyr::mutate(x_block,
                           ReversalNumb = ifelse(Reversal == 1, dplyr::row_number(), NA))
  x_block <- dplyr::ungroup(x_block)
  x_block <- dplyr::select(x_block, Subject, TrialProc, Block, Block.Correct,
                           BlockCriteria.Reach, Reversal,
                           ReversalNumb)

  x <- merge(x, x_block, by = c("Subject", "TrialProc", "Block"), all = TRUE)
  x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy, Response,
                       TrialCriteria.ACC, TargetArrowDirection,
                       FixationDuration, Block.Correct, BlockCriteria.Reach,
                       Reversal, ReversalNumb, FlankerDLScore,
                       FlankerMissedDeadlines, FlankerDLTotalAccuracy,
                       FlankerDLCorrectRT, FlankerDLTotalRT,
                       AdminTime, SessionDate, SessionTime)

  return(x)
}


#' Calculate FlankerDL threshold scores from a messy raw dataframe
#'
#' This function skips the 'raw_flankerDL()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_flankerDL()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_flankerDL <- function(x){
  if ("FlankerDLScore" %in% colnames(x)) {
    message("Depricated. Use raw_flankerDL to get scores for this version of the task")
  } else {

  }
}
