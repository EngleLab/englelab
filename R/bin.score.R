#' A Binning Function
#'
#' Calculates Bin scores using the method suggested in [insert citation here]
#' @param x a dataframe with trial level data. Needs to have RT and Accuracy DVs
#' @param RT.label Column name in dataframe that contains the reaction time data
#' @param Accuracy.label Column name in dataframe that contains the accuracy data
#' @param Subject.label Column name in dataframe that contains the subject identifiers
#' @param Condition.label Column name in dataframe that contains the trial condition type
#' @param baseline.condition The values that specify the baseline condition
#' @keywords bin
#' @export
#' @examples
#' bin.score(data, RT.label = "RT", Accuracy.label = "Accuracy", Condition.label = "TrialType", Subject.label = "Subject")

bin.score <- function(x,
                      RT.label = "RT", Accuracy.label = "Accuracy", Condition.label = "Condition", Subject.label = "Subject",
                      baseline.condition = "congruent"){
  labels <- c(Subject.label, RT.label, Accuracy.label, Condition.label)
  for (label in labels){
    if (label==Subject.label){
      change <- "Subject"
    } else if (label==RT.label){
      change <- "RT"
    } else if (label==Accuracy.label){
      change <- "Accuracy"
    } else if (label==Condition.label){
      change <- "TrialType"
    }
    colnames(x)[which(colnames(x)==label)] <- change
  }
  x <- dplyr::group_by(x, Subject, TrialType)
  x <- dplyr::mutate(x, MeanRT = stats::ave(RT),
              baselineMeanRT = ifelse(TrialType==baseline.condition, MeanRT, NA))
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, baselineMeanRT = stats::ave(baselineMeanRT, FUN = function(y) mean(y, na.rm = TRUE)))
  x <- dplyr::filter(x, TrialType!=baseline.condition)
  x <- dplyr::ungroup(x)
  x <- dplyr::group_by(x, Accuracy)
  x <- dplyr::mutate(x, RTDif = RT - baselineMeanRT,
              Bin = ifelse(Accuracy==1, dplyr::ntile(RTDif, 10), ifelse(Accuracy==0, 0, NA)),
              Bin = ifelse(Bin==0, 20, Bin))
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::summarise(x, BinScore = mean(Bin, na.rm = TRUE))
  colnames(x)[which(colnames(x)=="Subject")] <- Subject.label
  return(x)
}
