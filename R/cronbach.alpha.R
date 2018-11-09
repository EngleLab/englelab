#' Calculate Cronbach's Alpha
#'
#' Takes a trial-level data frame and calculates cronbach's alpha on the specified value
#' @param x a dataframe with trial level data
#' @param trial.col The column name that identifies trials
#' @param value The column name that identifies the values to be used
#' @param id The column name that identifies the Subject IDs.
#' @keywords cronbach
#' @export
#' @examples
#'

cronbach.alpha <- function(x, trial.col = "Trial", value = NULL, id = "Subject"){
  if (!is.null(value)){
    colnames(x)[which(colnames(x)==trial.col)] <- "Trial"
    trials <- max(x$Trial, na.rm = TRUE)
    if (trials < 100){
      x <- dplyr::mutate(x, Trial = ifelse(Trial<10, paste(0, Trial, sep = ""), Trial),
                         Trial = paste("Trial", Trial, sep = ""))
    } else if (trials >= 100){
      x <- dplyr::mutate(x, Trial = ifelse(Trial<10, paste(00, Trial, sep = ""), ifelse(Trial<100, paste(0, Trial, sep = ""), Trial)),
                         Trial = paste("Trial", Trial, sep = ""))
    }
    colnames(x)[which(colnames(x)=="Trial")] <- trial.col
    x <- dplyr::select(x, id, trial.col, value)
    x <- tidyr::spread(x, key = trial.col, value = value)
    x <- dplyr::select(x, dplyr::starts_with("Trial"))
  }
  a <- psych::alpha(x)$total$std.alpha
  return(a)
}
