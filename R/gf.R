#' A Task Scoring Function
#'
#' Creates a raw data file of the RAPM task from the E-Merge file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords raw
#' @export
#' @examples
#' raw_rapm(data)

raw_rapm <- function(x){
  x <- dplyr::filter(x, Blocks=="RealAll"|Blocks=="End", ShowStim.RT>0|!is.na(TotalScore))
  x <- dplyr::select(x, Subject, Trial, Answer = answer, Response = ItemResp,
                     Accuracy = ShowStim.ACC, RT = ShowStim.RT, TimeLeft = StopTime, TotalScore,
                     SessionDate, SessionTime)
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, TotalScore = mean(TotalScore, na.rm=TRUE))
  x <- dplyr::filter(x, !is.na(Trial))
  return(x)
}



#' A Task Scoring Function
#'
#' Creates a raw data file of the NumberSeries task from the E-Merge file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords raw
#' @export
#' @examples
#' raw_numberseries(data)

raw_numberseries <- function(x){
  x <- dplyr::filter(x, Blocks=="Real"|Blocks=="End", ShowStim.RT>0|!is.na(TotalScore))
  x <- dplyr::select(x, Subject, Trial, Answer = answer, Response = ItemResp,
                     Accuracy = ShowStim.ACC, RT = ShowStim.RT, TimeLeft = StopTime, TotalScore,
                     SessionDate, SessionTime)
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, TotalScore = mean(TotalScore, na.rm=TRUE))
  x <- dplyr::filter(x, !is.na(Trial))
  return(x)
}



#' A Task Scoring Function
#'
#' Creates a raw data file of the LetterSets task from the E-Merge file
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords raw
#' @export
#' @examples
#' raw_lettersets(data)

raw_lettersets <- function(x){
  x <- dplyr::filter(x, Blocks=="Real"|Blocks=="End", ShowStim.RT>0|!is.na(TotalScore))
  x <- dplyr::select(x, Subject, Trial, Answer = answer, Response = ItemResp,
                     Accuracy = ShowStim.ACC, RT = ShowStim.RT, TimeLeft = StopTime, TotalScore,
                     SessionDate, SessionTime)
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, TotalScore = mean(TotalScore, na.rm=TRUE))
  x <- dplyr::filter(x, !is.na(Trial))
  return(x)
}


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



#' A Task Scoring Function
#'
#' Scores the Letter Sets task taking a .txt exported E-Merge (or single E-Data) file as input
#' @param x a .txt exported E-Merge (or single E-Data) file
#' @keywords score
#' @export
#' @examples
#' score_lettersets(data)

score_lettersets <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, LetterSets.Total = TotalScore, LetterSets.Attempted = Attempted, LetterSets.Time = TotalTime)
  return(x)
}

