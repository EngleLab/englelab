#' Remove Problematic Subjects
#'
#' This function will remove problematic subjects based on a filter criteria
#' and save them to a log data file.
#'
#' @param x dataframe
#' @param filter an expression passed on to dplyr:filter()
#' @param log_file the file path and name for where to save a log file to
#' @export
#'

remove_problematic <- function(x,
                             filter = "",
                             log_file = NULL) {

  filter_func <-
    rlang::parse_expr(
    paste("dplyr::filter(x, ", filter, ")", sep = "")
    )
  x_removed <- eval(filter_func)

  if (!is.null(log_file)) {
    removed <- dplyr::anti_join(x, x_removed)
    readr::write_csv(removed, log_file)
  }

  return(x_removed)
}
