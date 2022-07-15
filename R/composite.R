#' Create a Composite Factor
#'
#' Create a composite factor from a list of variables. A composite factor can be
#' formed using a mean, sum, or factor analysis.
#' @param x data frame
#' @param name Name of the new composite variable
#' @param variables c() of columns to average together to create the composite
#' @param type What type of composite should be calculated?
#'     options are "mean", "sum", "fa" (factor analysis using psych::fa), or
#'     "pca" (principal component analysis using psych::pca). Default is "mean"
#' @param standardize Logical. Do you want to calculate the composite based
#'     on standardize (z-score) variables? (Default = FALSE)
#' @param missing_allowed Criteria for how many variables can having missing
#'     values and still calculate a composite
#' @export
#'

composite <- function(x,
                      name = NULL,
                      variables,
                      type = "mean",
                      standardize = TRUE,
                      missing_allowed = NULL) {

  # Compute z-scores if standardize == TRUE
  if (standardize == TRUE) {
    x <- center(x, variables = variables, standardize = TRUE)
    # Add _z to variable list so that the composite will use the z-scored values
    variables <- paste(variables, "_z", sep = "")
  }

  # Compute composite based on type
  if (type == "mean") {
    x <- transform(x, composite = rowMeans(x[variables], na.rm = TRUE))
  }
  if (type == "sum") {
    x <- transform(x, composite = rowSums(x[variables], na.rm = TRUE))
  }

  x <- dplyr::mutate(x, Missing = 0)
  # If missing criteria is specified, then index for each subject how many of
  # the variables they have missing values on. Then set composite to NA if
  # they exceed the missing criteria
  if (!is.null(missing_allowed)) {
    for (variable in colnames(x[variables])) {
      x <- dplyr::mutate(x,
                         Missing =
                           ifelse(is.na(get(variable)),
                                  (Missing + 1),
                                  (Missing + 0)))
    }
    x <- dplyr::mutate(x,
                       composite =
                         ifelse(Missing > missing_allowed, NA, composite))
  }

  # Don't need to do the missing criteria for fa and pca because scores
  # cannot be computed even with one missing value
  if (type == "fa") {
    x_fa <- dplyr::select(x, variables)
    x_fa <- psych::fa(x_fa)
    x_fa <- x_fa$scores[, 1]
    x <- dplyr::mutate(x, composite = x_fa)
  }
  if (type == "pca") {
    x_pca <- dplyr::select(x, variables)
    x_pca <- psych::pca(x_pca)
    x_pca <- x_pca$scores[, 1]
    x <- dplyr::mutate(x, composite = x_pca)
  }

  # Name composite variable and remove the Missing column
  colnames(x)[which(colnames(x) == "composite")] <- name
  remove <- "Missing"
  if (standardize == TRUE) {
    remove <- c(variables, remove)
  }
  x <- x[,-which(names(x) %in% remove)]

  return(x)
}
