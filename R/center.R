#' Center Variables
#'
#' Center variables around the mean. To create z-scores,
#' specify standardize = TRUE
#' A new column will be created with the centered values.
#' @param x dataframe
#' @param variables c() of columns to standardize
#' @param standardize Logical. Do you want to calculate
#'     zscores? Default is FALSE
#' @param drop Drop original non-centered variables. Default is FALSE
#' @param suffix Suffix to add at the end of the column names.
#'     Default is NULL and will add a _c for non-standardized and
#'     _z for standardized z-scores.
#' @export
#'

center <- function(x,
                   variables = c(),
                   standardize = FALSE,
                   drop = FALSE, suffix = NULL) {

  # Perform this function for each variable specified
  for (variable in variables) {
    # Calculate centered scores using the scale() function
    x <- dplyr::mutate(x,
                       hold = as.vector(scale(get(variable), center = TRUE,
                                              scale = standardize)))

    if (drop == TRUE) {
      x <- x[, -which(colnames(x) == variable)]
    }

    if (standardize == FALSE) {
      if (is.null(suffix)) {
        names(x)[which(names(x) == "hold")] <- paste(variable, "_c", sep = "")
      } else {
        names(x)[which(names(x) == "hold")] <- paste(variable, suffix, sep = "")
      }
    }

    if (standardize == TRUE) {
      if (is.null(suffix)) {
        names(x)[which(names(x) == "hold")] <- paste(variable, "_z", sep = "")
      } else {
        names(x)[which(names(x) == "hold")] <- paste(variable, suffix, sep = "")
      }
    }
  }

  return(x)
}

