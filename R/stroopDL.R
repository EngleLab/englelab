#' Creates a "tidy" raw dataframe for the StroopDL task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

raw_stroopDL <- function(x){
  x <- dplyr::rename(x,
                     TrialProc = `Procedure[Trial]`,
                     ResponseDeadline = `StroopDuration[SubTrial]`,
                     FixationDuration = `DurationOfFixation[SubTrial]`)
  x <- dplyr::filter(x, TrialProc == "BlockProc" |
                       TrialProc == "stroopPRAC1" |
                       TrialProc == "stroopPRAC2")
  x <- dplyr::mutate(x,
                     TrialProc = dplyr::case_when(TrialProc == "BlockProc" ~ "real",
                                                  TrialProc == "stroopPRAC1" ~ "practice1",
                                                  TrialProc == "stroopPRAC2" ~ "practice2"),
                     Block = dplyr::case_when(TrialProc == "real" ~ TrialList.Cycle,
                                              TrialProc == "practice1" ~ as.numeric(NA),
                                              TrialProc == "practice2" ~ as.numeric(NA)),
                     Trial = dplyr::case_when(TrialProc == "real" ~ SubTrial,
                                              TrialProc == "practice1" ~ PractList1.Sample,
                                              TrialProc == "practice2" ~ PractList2.Sample),
                     Condition = dplyr::case_when(TrialProc == "real" ~ trialType,
                                                  TrialProc == "practice1" ~ pracTYPE,
                                                  TrialProc == "practice2" ~ pracTYPE),
                     Condition = dplyr::case_when(Condition == "Cong" ~ "congruent",
                                                  Condition == "Filler" ~ "congruent",
                                                  Condition == "Incong" ~ "incongruent",
                                                  TRUE ~ as.character(NA)),
                     RT =
                       dplyr::case_when(TrialProc == "real" & is.na(stim.RESP) ~
                                          MissedDL.RT + ResponseDeadline,
                                        TrialProc == "real" & !is.na(stim.RESP) ~
                                          stim.RT,
                                        TrialProc == "practice1" ~ pracSTIM.RT,
                                        TrialProc == "practice2" ~ PracStim2.RT),
                     Accuracy =
                       dplyr::case_when(TrialProc == "real" & is.na(stim.RESP) ~
                                          MissedDL.ACC,
                                        TrialProc == "real" & !is.na(stim.RESP) ~
                                          stim.ACC,
                                        TrialProc == "practice1" ~ pracSTIM.ACC,
                                        TrialProc == "practice2" ~ PracStim2.ACC),
                     Response =
                       dplyr::case_when(TrialProc == "real" & is.na(stim.RESP) ~
                                          as.numeric(MissedDL.RESP),
                                        TrialProc == "real" & !is.na(stim.RESP) ~
                                          stim.RESP,
                                        TrialProc == "practice1" ~ pracSTIM.RESP,
                                        TrialProc == "practice2" ~ PracStim2.RESP),
                     Response = dplyr::case_when(Response == 1 ~ "GREEN",
                                                 Response == 2 ~ "BLUE",
                                                 Response == 3 ~ "RED",
                                                 TRUE ~ as.character(NA)),
                     MissedDeadline = ifelse(TrialProc == "real" & is.na(stim.RESP),
                                             1, 0),
                     TrialCriteria.ACC = dplyr::case_when(TrialProc == "real" ~ stim.ACC,
                                                          TrialProc == "practice1" ~ 0,
                                                          TrialProc == "practice2" ~ 0),
                     Word = dplyr::case_when(TrialProc == "real" ~ word,
                                             TrialProc == "practice1" ~ pracWORD,
                                             TrialProc == "practice2" ~ pracWORD),
                     Hue = dplyr::case_when(TrialProc == "real" ~ hue,
                                            TrialProc == "practice1" ~ pracHUE,
                                            TrialProc == "practice2" ~ pracHUE),
                     AdminTime = AdminTime/1000/60)

  if ("InstructionsTime" %in% colnames(x)) {
    x <- dplyr::mutate(x,
                       InstructionsTime = InstructionsTime/1000/60,
                       PracticeTime = PracticeTime/1000/60,
                       TaskTime = TaskTime/1000/60)
    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy,
                       Response, TrialCriteria.ACC, Word, Hue,
                       FixationDuration, StroopDLScore,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       InstructionsTime, PracticeTime, TaskTime,
                       AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy,
                       Response, TrialCriteria.ACC, Word, Hue,
                       FixationDuration, StroopDLScore,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       AdminTime, SessionDate, SessionTime)
  }

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

  if ("InstructionsTime" %in% colnames(x)) {
    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy, Response,
                       TrialCriteria.ACC, Word, Hue,
                       FixationDuration, Block.Correct, BlockCriteria.Reach,
                       Reversal, ReversalNumb, StroopDLScore,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       InstructionsTime, PracticeTime, TaskTime,
                       AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy, Response,
                       TrialCriteria.ACC, Word, Hue,
                       FixationDuration, Block.Correct, BlockCriteria.Reach,
                       Reversal, ReversalNumb, StroopDLScore,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       AdminTime, SessionDate, SessionTime)
  }

  return(x)
}


#' Calculate StroopDL threshold scores from a messy raw dataframe
#'
#' This function skips the 'raw_stroopDL()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_stroopDL()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_stroopDL <- function(x){
  x <- dplyr::select(x, Subject, StroopDLTime,
                     StroopMissedDeadlines, StroopDLTotalAccuracy,
                     StroopDLCorrectRT, StroopDLTotalRT,
                     AdminTime)
  x <- dplyr::distinct(x)
  x <- dplyr::mutate(x, AdminTime = AdminTime/1000/60)
  return(x)
}
