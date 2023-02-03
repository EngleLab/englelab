#' Replace Outlier Scores
#'
#' This function will replace outliers based on a z-score cutoff
#'
#' @param x dataframe
#' @param variables variables c() to be trimmed
#' @param cutoff z-score cutoff to use for trimming (default: 3.5)
#' @param with What value should the trimmed values be replaced with.
#'     (default: with = "NA")
#' @param pass How many times to do outlier replacement?
#'     ex. pass = 2 will do a two-pass outlier replacement. (default: 1)
#' @param id If variables = "all", then need to supply the subject ID variable
#' @param log_file the file path and name for where to save a log file to
#' @export
#'

replace_outliers <- function(x,
                             variables = c(),
                             cutoff = 3.5,
                             with = "NA",
                             pass = 1,
                             id = "Subject",
                             log_file = NULL) {
  col_order <- colnames(x)
  if ("all" %in% variables) {
    variables <- colnames(x)[which(colnames(x) != id)]
  }

  outliers <- list()
  for (i in 1:pass) {
    if (i == 1) {
      x_replace <- x
      x_prev <- x
    }
    if (i > 1) {
      x_prev <- x_replace
    }

    x_replace <- center(x_replace, variables = variables, standardize = TRUE)

    if (with == "NA") {
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

    if (with == "cutoff") {
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

    if (with == "mean") {
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
        x_replace <- dplyr::select(x_replace, -(var),
                                   -placeholder.mean)
        colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
      }
    }

    if (with == "median") {
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
        x_replace <- dplyr::select(x_replace, -(var),
                                   -placeholder.median)
        colnames(x_replace)[which(colnames(x_replace) == "placeholder")] <- var
      }
    }

    x_replace <- x_replace[col_order]

    outliers[[i]] <- suppressMessages(dplyr::anti_join(x_prev, x_replace))
    outliers[[i]] <- dplyr::mutate(outliers[[i]], Pass = i)
    message("Outliers detected (pass = ", i, "): ", nrow(outliers[[i]]))
  }
  outliers <- dplyr::bind_rows(outliers)

  if (!is.null(log_file) & nrow(outliers) > 0) {
    readr::write_csv(outliers, log_file)
  }

  x_replace <- x_replace[col_order]
  return(x_replace)
}
