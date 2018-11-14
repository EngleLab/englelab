#' A Task Scoring Function
#'
#' Scores the Rotation Span (RotSPAN) task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @keywords score
#' @export
#' @examples
#' score_rotspan(x, blocks = 2)

score_rotspan <- function(x, blocks = ""){
  if (blocks==1){
    x <- dplyr::filter(x, `Procedure[Block]`=="realBoth")
    x <- dplyr::select(x, Subject,
                       RotSpan.Absolute = RotspanAbsoluteScore,
                       RotSpan.Partial = RotspanPartialScore,
                       RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                       RotSpan.RotationACC = RotationACC,
                       RotSpan.RotationDuration = RotationDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==2){
    x <- dplyr::filter(x, `Procedure[Block]`=="realBoth")
    x <- dplyr::select(x, Subject,
                       RotSpan.Absolute = RotspanAbsoluteScore,
                       RotSpan.Partial = RotspanPartialScore,
                       RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                       RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                       RotSpan.RotationACC = RotationACC,
                       RotSpan.RotationDuration = RotationDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==3){
    x <- dplyr::filter(x, `Procedure[Block]`=="realBoth")
    x <- dplyr::select(x, Subject,
                       RotSpan.Absolute = RotspanAbsoluteScore,
                       RotSpan.Partial = RotspanPartialScore,
                       RotSpan.Partial_Block1 = RotspanPartialScoreBlock1,
                       RotSpan.Partial_Block2 = RotspanPartialScoreBlock2,
                       RotSpan.Partial_Block3 = RotspanPartialScoreBlock3,
                       RotSpan.RotationACC = RotationACC,
                       RotSpan.RotationDuration = RotationDuration)
    x <- dplyr::distinct(x)
  } else if (blocks==""){
    warning('Need to specify the number of blocks')
  } else if (blocks>3|blocks<1){
    warning('Invalid number of blocks specified')
  }
  return(x)
}
