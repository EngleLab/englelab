#' A Task Scoring Function
#'
#' Scores the Number Series task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords score
#' @export
#' @examples
#' score_numberseries(data)

score_numberseries <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, NumberSeries.Total = TotalScore, NumberSeries.Attempted = Attempted, NumberSeries.Time = TotalTime)
  return(x)
}
