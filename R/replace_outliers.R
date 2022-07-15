#' Replace Outlier Scores
#'
#' This function will replace outliers variable(s) based on a zscore cutoff
#' @param x dataframe
#' @param variables variables c() to be trimmed
#' @param cutoff zscore cutoff to use for trimming (default: 3.5)
#' @param with What value should the trimmed values be replaced with.
#'     (default: replace = "NA")
#' @param id If variables = "all", then need to supply the subject ID variable
#' @export
#'

replace_outliers <- function(x,
                             variables = c(),
                             cutoff = 3.5,
                             with = "NA",
                             id = "Subject") {
  col_order <- colnames(x)
  if ("all" %in% variables) {
    variables <- colnames(x)[which(colnames(x) != id)]
  }

  x_replace <- center(x, variables = variables, standardize = TRUE)

  if (replace == "NA") {
    for (var in variables) {
      zscored <- paste(var, "_z", sep = "")
      x_replace <-
        dplyr::mutate(x_replace,
                      placeholder =
                        ifelse(get(zscored) > cutoff, NA,
                               ifelse(get(zscored) < (cutoff * -1),
                                      NA, get(var))))
      x_replace <- dplyr::select(x_replace, -(var))
      colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
    }
  }

  if (replace == "cutoff") {
    for (var in variables) {
      zscored <- paste(var, "_z", sep = "")
      x_replace <-
        dplyr::mutate(x_replace,
                      placeholder = get(zscored),
                      placeholder.mean = mean(get(var), na.rm = TRUE),
                      placeholder.sd = sd(get(var), na.rm = TRUE),
                      placeholder =
                        ifelse(placeholder > cutoff,
                               placeholder.mean + (placeholder.sd * cutoff),
                               ifelse(placeholder < (cutoff * -1),
                                      placeholder.mean - (placeholder.sd*cutoff),
                                      get(var))))
      x_replace <- dplyr::select(x_replace, -(var))
      colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
    }
  }

  if (replace == "mean") {
    for (var in variables) {
      zscored <- paste(var, "_z", sep = "")
      x_replace <-
        dplyr::mutate(x_replace,
                      placeholder = get(zscored),
                      placeholder.mean = mean(get(var), na.rm = TRUE),
                      placeholder =
                        ifelse(placeholder > cutoff, placeholder.mean,
                               ifelse(placeholder < (cutoff * -1),
                                      placeholder.mean, get(var))))
      x_replace <- dplyr::select(x_replace, -(var))
      colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
    }
  }

  if (replace == "median") {
    for (var in variables) {
      zscored <- paste(var, "_z", sep = "")
      x_replace <-
        dplyr::mutate(x_replace,
                      placeholder = get(zscored),
                      placeholder.median = median(get(var), na.rm = TRUE),
                      placeholder =
                        ifelse(placeholder > cutoff, placeholder.median,
                               ifelse(placeholder < (cutoff * -1),
                                      placeholder.median, get(var))))
      x_replace <- dplyr::select(x_replace, -(var))
      colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
    }
  }

  if (!is.null(output_file)) {
    outliers <- dplyr::anti_join(x, x_replace)
    readr::write_csv(outliers, output_file)
  }

  x_replace <- x_replace[col_order]
  return(x_replace)
}
