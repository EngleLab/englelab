#' A Task Scoring Function
#'
#' Scores the Symmetry Span (SSPAN) task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @keywords score
#' @export
#' @examples
#' score.symspan(data, blocks = 2)

score.symspan <- function(x, blocks = ""){
  if (blocks==1){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.SymmetryDuration = SymmetryDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==2){
    x <- dplyr::filter(x, `Procedure[Block]`=="TaskProc")
    x <- dplyr::select(x, Subject,
                       SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.SymmetryDuration = SymmetryDuration)
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
                       SymSpan.SymmetryDuration = SymmetryDuration)
    x <- dplyr::distinct(x)
    x <- dplyr::rename(x, SymSpan.Absolute = SspanAbsoluteScore,
                       SymSpan.Partial = SspanPartialScore,
                       SymSpan.Partial_Block1 = SspanPartialScoreBlock1,
                       SymSpan.Partial_Block2 = SspanPartialScoreBlock2,
                       SymSpan.Partial_Block3 = SspanPartialScoreBlock3,
                       SymSpan.SymmetryACC = SymmetryACC,
                       SymSpan.SymmetryDuration = SymmetryDuration)
  } else if (blocks==""){
    warning('Need to specify the number of blocks')
  } else if (blocks>3|blocks<1){
    warning('Invalid number of blocks specified')
  }
  return(x)
}

