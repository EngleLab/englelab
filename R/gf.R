#' Creates a "tidy" raw dataframe for the RAPM task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

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



#' Creates a "tidy" raw dataframe for the NumberSeries task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

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



#' Creates a "tidy" raw dataframe for the LetterSets task
#'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

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


#' Calculate RAPM scores from a messy raw dataframe
#'
#' This function skips the 'raw_rapm()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_rapm()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_rapm <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, RAPM.Total = TotalScore, RAPM.Attempted = Attempted, RAPM.Time = TotalTime)
  return(x)
}



#' Calculate NumberSeries scores from a messy raw dataframe
#'
#' This function skips the 'raw_numberseries()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_numberseries()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_numberseries <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, NumberSeries.Total = TotalScore, NumberSeries.Attempted = Attempted, NumberSeries.Time = TotalTime)
  return(x)
}



#' Calculate LetterSets scores from a messy raw dataframe
#'
#' This function skips the 'raw_lettersets()' step and therefore
#'     is not advised. However, some researchers may find
#'     it easier to just skip right to 'score_lettersets()'
#' @param x dataframe (an imported .emrge file)
#' @export
#'

score_lettersets <- function(x){
  x <- dplyr::filter(x,`Procedure[Block]`=="endproc")
  x <- dplyr::select(x, Subject, TotalScore, Attempted, TotalTime)
  x <- dplyr::rename(x, LetterSets.Total = TotalScore, LetterSets.Attempted = Attempted, LetterSets.Time = TotalTime)
  return(x)
}

