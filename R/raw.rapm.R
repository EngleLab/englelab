#' A Task Scoring Function
#'
#' Creates a raw data file of the RAPM task from the E-Merge file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords raw
#' @export
#' @examples
#' raw.rapm(data)

raw.rapm <- function(x){
  x <- dplyr::filter(x, Blocks=="RealAll"|Blocks=="End", ShowStim.RT>0|!is.na(TotalScore))
  x <- dplyr::select(x, Subject, Trial, Answer = answer, Response = ItemResp,
                     Accuracy = ShowStim.ACC, RT = ShowStim.RT, TimeLeft = StopTime, TotalScore,
                     SessionDate, SessionTime)
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, TotalScore = mean(TotalScore, na.rm=TRUE))
  x <- dplyr::filter(x, !is.na(Trial))
  return(x)
}
