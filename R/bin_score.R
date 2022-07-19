#' Calculate Bin Scores
#'
#' Calculates Bin scores using the method suggested in
#' Hughes et al. (2014) and Draheim et al. (2016)
#'
#' @section References:
#'
#' Draheim, C., Hicks, K. L., & Engle, R. W. (2016). Combining reaction time
#' and accuracy: The relationship between working memory capacity and task
#' switching as a case example. Perspectives on Psychological Science, 11(1),
#' 133–155. https://doi.org/10.1177/1745691615596990
#'
#' Hughes, M. M., Linck, J. A., Bowles, A. R., Koeth, J. T., & Bunting, M. F.
#' (2014). Alternatives to switch-cost scoring in the task-switching paradigm:
#' Their reliability and increased validity. Behavior Research Methods, 46(3),
#' 702–721. https://doi.org/10.3758/s13428-013-0411-5
#'
#' @param x a dataframe with trial level data. Needs to have RT and Accuracy DVs
#' @param rt Column name in dataframe that contains the reaction time data
#' @param accuracy Column name in dataframe that contains the accuracy data
#' @param condition Column name in dataframe that contains the trial condition type
#' @param baseline The values that specify the baseline condition
#' @param type How should Bin trials be aggregated, "sum" or "mean" (Default: "mean")
#' @param id Column name in dataframe that contains the subject identifiers
#' @keywords bin
#' @export
#'

bin_score <- function(x, rt = "RT", accuracy = "Accuracy",
                      condition = "Condition", baseline = "congruent",
                      type = "mean", id = "Subject"){

  colnames(x)[which(colnames(x) == id)] <- "Subject_colname"
  colnames(x)[which(colnames(x) == rt)] <- "RT_colname"
  colnames(x)[which(colnames(x) == accuracy)] <- "Accuracy_colname"
  colnames(x)[which(colnames(x) == condition)] <- "Condition_colname"

  x <- dplyr::group_by(x, Subject_colname, Condition_colname)
  x <- dplyr::mutate(x,
                     MeanRT = stats::ave(RT_colname),
                     baselineMeanRT =
                       ifelse(Condition_colname == baseline, MeanRT, NA))
  x <- dplyr::group_by(x, Subject_colname)
  x <- dplyr::mutate(x,
                     baselineMeanRT =
                       stats::ave(baselineMeanRT,
                                  FUN = function(y) mean(y, na.rm = TRUE)))
  x <- dplyr::filter(x, Condition_colname != baseline)
  x <- dplyr::ungroup(x)
  x <- dplyr::group_by(x, Accuracy_colname)
  x <- dplyr::mutate(x, RTDif = RT_colname - baselineMeanRT,
                     Bin =
                       ifelse(Accuracy_colname == 1,
                              dplyr::ntile(RTDif, 10),
                              ifelse(Accuracy_colname == 0, 0, NA)),
                     Bin = ifelse(Bin == 0, 20, Bin))
  x <- dplyr::group_by(x, Subject_colname)

  if (type == "mean") {
    x <- dplyr::summarise(x, BinScore = mean(Bin, na.rm = TRUE))
  }

  if (type == "sum") {
    x <- dplyr::summarise(x, BinScore = sum(Bin, na.rm = TRUE))
  }
  x <- dplyr::ungroup(x)

  colnames(x)[which(colnames(x) == "Subject_colname")] <- id
  colnames(x)[which(colnames(x) == "RT_colname")] <- rt
  colnames(x)[which(colnames(x) == "Accuracy_colname")] <- accuracy
  colnames(x)[which(colnames(x) == "Condition_colname")] <- condition

  return(x)
}
