#' A Task Scoring Function
#'
#' Scores the Raven's Advanced Progressive Matrices (RAPM) task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords score
#' @export
#' @examples
#' score_rapm(data)

score_rapm <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, RAPM.Total = TotalScore, RAPM.Attempted = Attempted, RAPM.Time = TotalTime)
  return(x)
}
