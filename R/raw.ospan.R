#' A Task Scoring Function
#'
#' Creates a raw data file of the OSPAN task from an E-Merged file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the Procedure[Block] variable has a different label. (Default = "new")
#' @keywords score
#' @export
#' @examples
#' raw.ospan(data, blocks = 2)

raw.ospan <- function(x, blocks = "", taskVersion = "new"){
  if (taskVersion=="new"){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
  } else if (taskVersion=="old"){
    x <- dplyr::filter(x, `Procedure[Block]`=="SessionProc")
    x <- dplyr::mutate(x, MathACC = NA)
  }

  x <- dplyr::group_by(x, Subject, Trial, setsz)

  if (blocks==1){
    x <- dplyr::mutate(x, SpanTotal = SpanTotalBlock1,
                       Recall.correct = stats::ave(SpanTotal, FUN = function(x) mean(x, na.rm = TRUE)),
                       Math.correct = stats::ave(OPERATION.ACC, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(SpanTotal))
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.AbsoluteUnit = OspanAbsoluteUnitScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.PartialUnit = OspanPartialUnitScore,
                         OSpan.PartialUnit_Block1 = OspanPartialUnitScoreBlock1,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    }
  } else if (blocks==2){
    x <- dplyr::mutate(x, SpanTotal = ifelse(is.na(SpanTotalBlock1), SpanTotalBlock2, SpanTotalBlock1),
                       Recall.correct = stats::ave(SpanTotal, FUN = function(x) mean(x, na.rm = TRUE)),
                       Math.correct_Block1 = stats::ave(OPERATION.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Math.correct_Block2 = stats::ave(OPERATION1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Math.correct = Math.correct_Block1 + Math.correct_Block2)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(SpanTotal))
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
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
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
                         OSpan.Absolute = OspanAbsoluteScore,
                         OSpan.Partial = OspanPartialScore,
                         OSpan.Partial_Block1 = OspanPartialScoreBlock1,
                         OSpan.Partial_Block2 = OspanPartialScoreBlock2,
                         OSpan.MathACC = MathACC,
                         OSpan.MathDuration = MathDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==3){
    x <- dplyr::mutate(x, SpanTotal = ifelse(is.na(SpanTotalBlock1), ifelse(is.na(SpanTotalBlock2), SpanTotalBlock3, SpanTotalBlock2), SpanTotalBlock1),
                       Recall.correct = stats::ave(SpanTotal, FUN = function(x) mean(x, na.rm = TRUE)),
                       Math.correct_Block1 = stats::ave(OPERATION.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Math.correct_Block2 = stats::ave(OPERATION1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Math.correct_Block3 = stats::ave(OPERATION2.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Math.correct = Math.correct_Block1 + Math.correct_Block2 + Math.correct_Block3)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(SpanTotal))
    if ("OspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
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
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Math.correct,
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
