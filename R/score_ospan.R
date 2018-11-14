#' A Task Scoring Function
#'
#' Scores the Operation Span (OSPAN) task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @param blocks number of blocks administered. From 1-3
#' @keywords score
#' @export
#' @examples
#' score_ospan(data, blocks = 2)

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
