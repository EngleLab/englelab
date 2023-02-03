#' Raw Tidy Data for StroopDL
#'
#' Converts the messy e-prime data file into a tidy raw data file that is
#' easy to work with.
#'
#' @param x dataframe
#' @param include_col c(): list of additional columns to include
#' @export
#'

raw_stroopDL <- function(x, include_col = c()) {

  if (x$ExperimentName[1] == "StroopDL_v2") {
    x <- dplyr::rename(x, TrialProc = `Procedure[Trial]`)
    x <- dplyr::filter(x, TrialProc != "RestProc")
    x <- dplyr::mutate(x,
                       TrialProc =
                         dplyr::case_when(
                           TrialProc == "ResponseMapProc" ~
                             "practice - response mapping",
                           TrialProc == "NoDeadlinePracticeProc" ~
                             "practice - no response deadline",
                           TrialProc == "DeadlinePracticeProc" ~
                             "practice - response deadline",
                           TrialProc == "TrialProc" ~ "real"),
                       Accuracy =
                         dplyr::case_when(
                           TrialProc == "practice - response mapping" |
                             TrialProc == "practice - no response deadline" ~
                             NoDeadlineStimulus.ACC,
                           TrialProc == "practice - response deadline" |
                             TrialProc == "real" ~ Stimulus.ACC),
                       Response =
                         dplyr::case_when(
                           TrialProc == "practice - response mapping" |
                             TrialProc == "practice - no response deadline" ~
                             NoDeadlineStimulus.RESP,
                           TrialProc == "practice - response deadline" |
                             TrialProc == "real" ~ Stimulus.RESP),
                       Response =
                         dplyr::case_when(Response == 1 ~ "green",
                                          Response == 2 ~ "blue",
                                          Response == 3 ~ "red",
                                          TRUE ~ as.character(NA)),
                       CorrectResponse =
                         dplyr::case_when(CorrectResponse == 1 ~ "green",
                                          CorrectResponse == 2 ~ "blue",
                                          CorrectResponse == 3 ~ "red",
                                          TRUE ~ as.character(NA)),
                       RT =
                         dplyr::case_when(
                           TrialProc == "practice - response mapping" |
                             TrialProc == "practice - no response deadline" ~
                             NoDeadlineStimulus.RT,
                           TrialProc == "practice - response deadline" |
                             TrialProc == "real" ~ Stimulus.RT),
                       Trial =
                         dplyr::case_when(
                           TrialProc == "practice - no response deadline" ~
                             NoDeadlinePractice.Sample,
                           TrialProc == "practice - response deadline" ~
                             DeadlinePractice.Sample,
                           TRUE ~ Trial),
                       ResponseDeadline =
                         dplyr::case_when(
                           ResponseDeadline == "(infinite)" ~ as.integer(NA),
                           TRUE ~ as.integer(ResponseDeadline)),
                       AdminTime = AdminTime)
    x <- dplyr::select(x, Subject, TrialProc, Trial, StroopType, Color, Word,
                       ResponseDeadline, StepSize, Accuracy, RT, Response,
                       CorrectResponse, MadeDeadline,
                       PrevTrial_Accuracy = PrevTrialAcc, ReversalMade,
                       ReversalNumber = ReversalNum, FixationDuration,
                       dplyr::all_of(include_col),
                       AdminTime, SessionDate, SessionTime)
  } else {
    x <- dplyr::rename(x,
                       TrialProc = `Procedure[Trial]`,
                       ResponseDeadline = `StroopDuration[SubTrial]`,
                       FixationDuration = `DurationOfFixation[SubTrial]`)
    x <- dplyr::filter(x, TrialProc == "BlockProc" |
                         TrialProc == "stroopPRAC" |
                         TrialProc == "stroopPRAC2")
    x <- dplyr::group_by(x, Subject)
    x <- dplyr::mutate(x,
                       TrialProc = dplyr::case_when(TrialProc == "BlockProc" ~
                                                      "real",
                                                    TrialProc == "stroopPRAC" ~
                                                      "practice1",
                                                    TrialProc == "stroopPRAC2" ~
                                                      "practice2"),
                       Block = dplyr::case_when(TrialProc == "real" ~
                                                  TrialList.Cycle,
                                                TrialProc == "practice1" ~
                                                  as.numeric(NA),
                                                TrialProc == "practice2" ~
                                                  as.numeric(NA)),
                       Trial = dplyr::case_when(TrialProc == "real" ~ SubTrial,
                                                TrialProc == "practice1" ~
                                                  PractList1.Sample,
                                                TrialProc == "practice2" ~
                                                  PractList2.Sample),
                       Condition = dplyr::case_when(TrialProc == "real" ~
                                                      trialType,
                                                    TrialProc == "practice1" ~
                                                      pracTYPE,
                                                    TrialProc == "practice2" ~
                                                      pracTYPE),
                       Condition = dplyr::case_when(Condition == "Cong" ~
                                                      "congruent",
                                                    Condition == "Filler" ~
                                                      "congruent",
                                                    Condition == "Incong" ~
                                                      "incongruent",
                                                    TRUE ~ as.character(NA)),
                       RT =
                         dplyr::case_when(TrialProc == "real" &
                                            is.na(stim.RESP) ~
                                            MissedDL.RT + ResponseDeadline,
                                          TrialProc == "real" &
                                            !is.na(stim.RESP) ~
                                            stim.RT,
                                          TrialProc == "practice1" ~ pracSTIM.RT,
                                          TrialProc == "practice2" ~ PracStim2.RT),
                       Accuracy =
                         dplyr::case_when(TrialProc == "real" &
                                            is.na(stim.RESP) ~ MissedDL.ACC,
                                          TrialProc == "real" &
                                            !is.na(stim.RESP) ~ stim.ACC,
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
                       MissedDeadline = ifelse(TrialProc == "real" &
                                                 is.na(stim.RESP), 1, 0),
                       TrialCriteria.ACC =
                         dplyr::case_when(TrialProc == "real" ~ stim.ACC,
                                          TrialProc == "practice1" ~ 0,
                                          TrialProc == "practice2" ~ 0),
                       Word = dplyr::case_when(TrialProc == "real" ~ word,
                                               TrialProc == "practice1" ~ pracWORD,
                                               TrialProc == "practice2" ~ pracWORD),
                       Hue = dplyr::case_when(TrialProc == "real" ~ hue,
                                              TrialProc == "practice1" ~ pracHUE,
                                              TrialProc == "practice2" ~ pracHUE),
                       StartTime = min(pracSTIM.OnsetTime, na.rm = TRUE),
                       FinishTime = max(stim.RTTime, na.rm = TRUE),
                       AdminTime = (FinishTime - StartTime) / 60000)
    x <- dplyr::ungroup(x)

    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy,
                       Response, TrialCriteria.ACC, Word, Hue,
                       FixationDuration, StroopDLScore = StroopDLTime,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       AdminTime, SessionDate, SessionTime)

    x_block <- dplyr::filter(x, TrialProc == "real")
    x_block <- dplyr::group_by(x, Subject, Block)
    x_block <- dplyr::mutate(x_block,
                             Block.Correct = sum(TrialCriteria.ACC, na.rm = TRUE),
                             BlockCriteria.Reach = ifelse(Block.Correct > 14, 1, 0))
    x_block <- dplyr::filter(x_block, Trial == 1, !is.na(Block))
    x_block <- dplyr::group_by(x_block, Subject, TrialProc)
    x_block <- dplyr::mutate(x_block,
                             PrevBlockCriteria.Reach =
                               dplyr::lag(BlockCriteria.Reach, 1, 0),
                             Reversal =
                               ifelse(PrevBlockCriteria.Reach != BlockCriteria.Reach,
                                      1, 0))
    x_block <- dplyr::group_by(x_block, Subject, Reversal)
    x_block <- dplyr::mutate(x_block,
                             ReversalNumb = ifelse(Reversal == 1,
                                                   dplyr::row_number(), NA))
    x_block <- dplyr::ungroup(x_block)
    x_block <- dplyr::select(x_block, Subject, TrialProc, Block, Block.Correct,
                             BlockCriteria.Reach, Reversal,
                             ReversalNumb)

    x <- merge(x, x_block, by = c("Subject", "TrialProc", "Block"), all = TRUE)

    x <- dplyr::select(x, Subject, TrialProc, Block, Trial, ResponseDeadline,
                       Condition, RT, MissedDeadline, Accuracy, Response,
                       TrialCriteria.ACC, Word, Hue,
                       FixationDuration, Block.Correct, BlockCriteria.Reach,
                       Reversal, ReversalNumb, StroopDLScore,
                       StroopMissedDeadlines, StroopDLTotalAccuracy,
                       StroopDLCorrectRT, StroopDLTotalRT,
                       dplyr::all_of(include_col), AdminTime, SessionDate, SessionTime)
  }

  return(x)
}

#' Calculate StroopDL Scores
#'
#' Calculate StroopDL scores from the output of `raw_stroopDL()`
#' @param x dataframe
#' @param scoring_method List of scoring methods to use, e.g.
#'     c("Last 4 Reversals", "Last 8 Reversals").
#'     Passed onto score_adaptivethreshold().
#'     The scoring method to use. Default: Last 4 Reversals.
#'     Options are:
#'     Last n Reversals (substitute n for a digit, e.g., Last 4 Reversals. This
#'     represents the median of the last n reversals);
#'     Last Trial (the value of the adaptive variable on the last trial);
#'     All Reversals (median of all reversals);
#'     Overall Median (median across all trials)
#' @param rt_cutoff Optional. Criterion for cutoff of too short of reaction
#'     times. Used when calculating overall RT performance.
#' @export
#'

score_stroopDL <- function(x, scoring_method = "Last 4 Reversals",
                           rt_cutoff = 0) {

  x <- dplyr::filter(x, TrialProc == "real")

  x_threshold <- dplyr::filter(x, StroopType == "incongruent")

  x_adaptive <- list()
  for (i in seq_along(scoring_method)) {
    x_adaptive[[i]] <- dplyr::group_by(x_threshold, Subject)
    x_adaptive[[i]] <-
      englelab::score_adaptivethreshold(x_adaptive[[i]], taskname = "StroopDL",
                                        adaptive_variable = "ResponseDeadline",
                                        scoring_method = scoring_method[[i]])
    x_adaptive[[i]] <- dplyr::ungroup(x_adaptive[[i]])
  }
  x_adaptive <- plyr::join_all(x_adaptive, by = "Subject")

  x_condition_perf <- dplyr::group_by(x, Subject, StroopType)
  x_condition_perf <- dplyr::mutate(x,
                                    RT = ifelse(Accuracy == 0, NA, RT),
                                    RT = ifelse(RT < rt_cutoff, NA, RT),
                                    MissedDeadline =
                                      ifelse(MadeDeadline == 1, 0, 1))
  x_condition_perf <- dplyr::summarise(x,
                                       ACC = mean(Accuracy, na.rm = TRUE),
                                       RT = mean(RT, na.rm = TRUE),
                                       MissedDeadlines = mean(MissedDeadline))
  x_condition_perf <- dplyr::ungroup(x_condition_perf)
  x_condition_perf <- tidyr::pivot_wider(id_cols = "Subject",
                                         names_from = "StroopType",
                                         values_from =
                                           c("ACC", "RT", "MissedDeadlines"),
                                         names_glue = "{StroopType}.{.value}")
  x_condition_perf <- dplyr::rename_with(~ paste("StroopDL_", .x, sep = ""),
                                         dplyr::contains("congruent"))

  x_overall_perf <- dplyr::group_by(x, Subject)
  x_overall_perf <- dplyr::mutate(x,
                                  RT = ifelse(Accuracy == 0, NA, RT),
                                  RT = ifelse(RT < rt_cutoff, NA, RT),
                                  MissedDeadline =
                                    ifelse(MadeDeadline == 1, 0, 1))
  x_overall_perf <- dplyr::summarise(x,
                                     ACC = mean(Accuracy, na.rm = TRUE),
                                     RT = mean(RT, na.rm = TRUE),
                                     MissedDeadlines = mean(MissedDeadline),
                                     AdminTime = dplyr::first(AdminTime))
  x_overall_perf <- dplyr::ungroup(x_overall_perf)
  x_overall_perf <- dplyr::rename_with(~ paste("StroopDL_Overall.", .x, sep = ""),
                                       c(ACC, RT, MissedDeadlines, AdminTime))

  x_scores <- merge(x_adaptive, x_condition_perf, by = "Subject")
  x_scores <- merge(x_scores, x_overall_perf, by = "Subject")
}

