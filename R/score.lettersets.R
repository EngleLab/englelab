#' A Task Scoring Function
#'
#' Scores the Letter Sets task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords score
#' @export
#' @examples
#' score.lettersets(data)

score.lettersets <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, LetterSets.Total = TotalScore, LetterSets.Attempted = Attempted, LetterSets.Time = TotalTime)
  return(x)
}
