#' A Task Scoring Function
#'
#' Creates a raw data file of the RotSpan task from an E-Merged file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the Procedure[Block] variable has a different label. (Default = "new")
#' @keywords raw
#' @export
#' @examples
#' raw_rotspan(x, blocks = 2)

raw_rotspan <- function(x, blocks = "", taskVersion = "new"){
  if(taskVersion=="old"){
    x <- dplyr::mutate(x, RotationACC = NA)
  }
  x <- dplyr::filter(x, `Procedure[Block]`=="realBoth")
  x <- dplyr::group_by(x, Subject, Trial, setsz)
  x <- dplyr::mutate(x, `SpanTotal[SubTrial]` = ifelse(!is.na(`SpanTotal[SubTrial]`)&`SpanTotal[SubTrial]`=="?", NA, `SpanTotal[SubTrial]`),
                     `SpanTotal[SubTrial]` = as.integer(`SpanTotal[SubTrial]`),
                     Recall.correct = stats::ave(`SpanTotal[SubTrial]`, FUN = function(x) mean(x, na.rm = TRUE)))
  if (blocks==1){
    x <- dplyr::mutate(x, Rotations.correct = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("RotspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.AbsoluteUnit = RotspanAbsoluteUnitScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.PartialUnit = RotspanPartialUnitScore,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==2){
    x <- dplyr::mutate(x,
                       Rotations.correct_Block1 = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Rotations.correct_Block2 = stats::ave(CheckResponse1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Rotations.correct = Rotations.correct_Block1 + Rotations.correct_Block2)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("RotspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.AbsoluteUnit = RotspanAbsoluteUnitScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                         RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                         RotSpan.PartialUnit = RotspanPartialUnitScore,
                         RotSpan.PartialUnit_Block1 = RotspanPartialUnitScoreBlock1,
                         RotSpan.PartialUnit_Block2 = RotspanPartialUnitScoreBlock2,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                         RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==3){
    x <- dplyr::mutate(x,
                       Rotations.correct_Block1 = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Rotations.correct_Block2 = stats::ave(CheckResponse1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Rotations.correct_Block3 = stats::ave(CheckResponse2.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Rotations.correct = Rotations.correct_Block1 + Rotations.correct_Block2 + Rotations.correct_Block3)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("RotspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.AbsoluteUnit = RotspanAbsoluteUnitScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                         RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                         RotSpan.Partial_Block3 = RotspanPartialScoreBlock3,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Rotations.correct,
                         RotSpan.Absolute = RotspanAbsoluteScore,
                         RotSpan.Partial = RotspanPartialScore,
                         RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                         RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                         RotSpan.Partial_Block3 = RotspanPartialScoreBlock3,
                         RotSpan.PartialUnit = RotspanPartialUnitScore,
                         RotSpan.PartialUnit_Block1 = RotspanPartialUnitScoreBlock1,
                         RotSpan.PartialUnit_Block2 = RotspanPartialUnitScoreBlock2,
                         RotSpan.PartialUnit_Block3 = RotspanPartialUnitScoreBlock3,
                         RotSpan.RotationACC = RotationACC,
                         RotSpan.RotationDuration = RotationDuration,
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
