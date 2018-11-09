#' A Binning Function
#'
#' Calculates Bin scores using the method suggested in [insert citation here]
#' @param x a dataframe with trial level data. Needs to have RT and Accuracy DVs
#' @param rt.col Column name in dataframe that contains the reaction time data
#' @param accuracy.col Column name in dataframe that contains the accuracy data
#' @param condition.col Column name in dataframe that contains the trial condition type
#' @param type How should Bin trials be aggregated, "sum" or "mean" (Default: "mean")
#' @param baseline.condition The values that specify the baseline condition
#' @param id Column name in dataframe that contains the subject identifiers
#' @keywords bin
#' @export
#' @examples
#' bin.score(data, RT.label = "RT", Accuracy.label = "Accuracy", Condition.label = "TrialType", Subject.label = "Subject")

bin.score <- function(x, rt.col = "RT", accuracy.col = "Accuracy", condition.col = "Condition",
                      baseline.condition = "congruent", type = "mean", id = "Subject"){

  colnames(x)[which(colnames(x)==id)] <- "Subject"
  colnames(x)[which(colnames(x)==rt.col)] <- "RT"
  colnames(x)[which(colnames(x)==accuracy.col)] <- "Accuracy"
  colnames(x)[which(colnames(x)==condition.col)] <- "Condition"

  x <- dplyr::group_by(x, Subject, Condition)
  x <- dplyr::mutate(x, MeanRT = stats::ave(RT),
              baselineMeanRT = ifelse(Condition==baseline.condition, MeanRT, NA))
  x <- dplyr::group_by(x, Subject)
  x <- dplyr::mutate(x, baselineMeanRT = stats::ave(baselineMeanRT, FUN = function(y) mean(y, na.rm = TRUE)))
  x <- dplyr::filter(x, Condition!=baseline.condition)
  x <- dplyr::ungroup(x)
  x <- dplyr::group_by(x, Accuracy)
  x <- dplyr::mutate(x, RTDif = RT - baselineMeanRT,
              Bin = ifelse(Accuracy==1, dplyr::ntile(RTDif, 10), ifelse(Accuracy==0, 0, NA)),
              Bin = ifelse(Bin==0, 20, Bin))
  x <- dplyr::group_by(x, Subject)

  if (type == "mean"){
    x <- dplyr::summarise(x, BinScore = mean(Bin, na.rm = TRUE))
  }

  if (type = "sum"){
    x <- dplyr::summarise(x, BinScore = sum(Bin, na.rm = TRUE))
  }
  x <- dplyr::ungroup(x)

  colnames(x)[which(colnames(x)=="Subject")] <- id
  colnames(x)[which(colnames(x)=="RT")] <- rt.col
  colnames(x)[which(colnames(x)=="Accuracy")] <- accuracy.col
  colnames(x)[which(colnames(x)=="Condition")] <- condition.col
  return(x)
}
