#' A Task Scoring Function
#'
#' Creates a raw data file of the SymSpan task from an E-Merged file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @param taskVersion old or new version. Old version means the Procedure[Block] variable has a different label. (Default = "new")
#' @keywords raw
#' @export
#' @examples
#' raw.symspan(data, blocks = 2)

raw.symspan <- function(x, blocks = "", taskVersion = "new"){
  if (taskVersion=="new"){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
  } else if (taskVersion=="old"){
    x <- dplyr::filter(x, `Procedure[Block]`=="SessionProc")
    x <- dplyr::mutate(x, SymmetryACC = NA)
  }

  x <- dplyr::group_by(x, Subject, Trial, setsz)
  x <- dplyr::mutate(x, `SpanTotal[SubTrial]` = ifelse(!is.na(`SpanTotal[SubTrial]`)&`SpanTotal[SubTrial]`=="?", NA, `SpanTotal[SubTrial]`),
                     `SpanTotal[SubTrial]` = as.integer(`SpanTotal[SubTrial]`),
                     SymmetryDuration = ifelse(!is.na(SymmetryDuration)&SymmetryDuration=="?", NA, SymmetryDuration),
                     Recall.correct = stats::ave(`SpanTotal[SubTrial]`, FUN = function(x) mean(x, na.rm = TRUE)))
  if (blocks==1){
    x <- dplyr::mutate(x, Symm.correct = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)))
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.AbsoluteUnit = SspanAbsoluteUnitScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.PartialUnit = SspanPartialUnitScore,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.SymmetryDuration = SymmetryDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.SymmetryDuration = SymmetryDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==2){
    x <- dplyr::mutate(x,
                       Symm.correct_Block1 = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Symm.correct_Block2 = stats::ave(CheckResponse1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Symm.correct = Symm.correct_Block1 + Symm.correct_Block2)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.AbsoluteUnit = SspanAbsoluteUnitScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.PartialUnit = SspanPartialUnitScore,
                         SymSpan.PartialUnit_Block1 = SspanPartialUnitScoreBlock1,
                         SymSpan.PartialUnit_Block2 = SspanPartialUnitScoreBlock2,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.SymmetryDuration = SymmetryDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.SymmetryDuration = SymmetryDuration,
                         SessionDate, SessionTime)
    }

  } else if (blocks==3){
    x <- dplyr::mutate(x,
                       Symm.correct_Block1 = stats::ave(CheckResponse.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Symm.correct_Block2 = stats::ave(CheckResponse1.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Symm.correct_Block3 = stats::ave(CheckResponse2.ACC, FUN = function(x) sum(x, na.rm = TRUE)),
                       Symm.correct = Symm.correct_Block1 + Symm.correct_Block2 + Symm.correct_Block3)
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, !is.na(`SpanTotal[SubTrial]`))
    if ("SspanPartialUnitScore" %in% colnames(x)){
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
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
                         SymSpan.SymmetryDuration = SymmetryDuration,
                         SessionDate, SessionTime)
    } else {
      x <- dplyr::select(x, Subject, Trial, SetSize = setsz, Recall.correct, Symm.correct,
                         SymSpan.Absolute = SspanAbsoluteScore,
                         SymSpan.Partial = SspanPartialScore,
                         SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                         SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                         SymSpan.Partial_Block3 = SspanPartialScoreBlock3,
                         SymSpan.SymmetryACC = SymmetryACC,
                         SymSpan.SymmetryDuration = SymmetryDuration,
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

