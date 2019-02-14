#' Calculate Cronbach's Alpha
#'
#' Takes a trial-level data frame and calculates cronbach's alpha on the specified value
#' @param x a dataframe with trial level data
#' @param trial.col The column name that identifies trials
#' @param value The column name that identifies the values to be used
#' @param aggregate How should the value column be aggregated across even and odd trials? (ex. mean or sum)
#' @param id The column name that identifies the Subject IDs.
#' @keywords cronbach
#' @export
#' @examples
#'

splithalf <- function(x, trial.col = "Trial", value = "", aggregate = NULL, id = "Subject"){
  if (value != ""){
    colnames(x)[which(colnames(x)==trial.col)] <- "Trial"
    trials <- max(x$Trial, na.rm = TRUE)
    if (trials < 100){
      x <- dplyr::mutate(x, Trial = ifelse(Trial<10, paste(0, Trial, sep = ""), Trial),
                         Trial = paste("Trial", Trial, sep = ""))
    } else if (trials >= 100){
      x <- dplyr::mutate(x, Trial = ifelse(Trial<10, paste(00, Trial, sep = ""), ifelse(Trial<100, paste(0, Trial, sep = ""), Trial)),
                         Trial = paste("Trial", Trial, sep = ""))
    }
    x <- dplyr::select(x, id, Trial, value)
    x <- tidyr::spread(x, key = "Trial", value = value)
    x <- dplyr::select(x, dplyr::starts_with("Trial"))
  }
  even.col <- colnames(x)[c(FALSE,TRUE)]
  odd.col <- colnames(x)[c(TRUE,FALSE)]
  if (!is.null(aggregate)){
    if (aggregate=="mean"){
      x <- transform(x, even = rowMeans(x[even.col], na.rm = TRUE))
      x <- transform(x, odd = rowMeans(x[odd.col], na.rm = TRUE))
    }
    if (aggregate=="sum"){
      x <- transform(x, even = rowSums(x[even.col], na.rm = TRUE))
      x <- transform(x, odd = rowSums(x[odd.col], na.rm = TRUE))
    }
  } else {
    colnames(x) <- c("even", "odd")
  }
  r <- cor(x$even, x$odd, use = "pairwise.complete.obs")
  sh <- (2*r)/(1+r)
  return(sh)
}
