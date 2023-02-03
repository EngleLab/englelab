#' Remove Problematic Subjects
#'
#' This function will remove problematic subjects based on a filter criterion
#' and save them to a log data file.
#'
#' @param x dataframe
#' @param remove an expression passed on to dplyr:filter()
#' @param log_file the file path and name for where to save a log file to
#' @export
#'

remove_problematic <- function(x,
                               remove = "",
                               log_file = NULL) {

  filter_func <-
    rlang::parse_expr(
    paste("dplyr::filter(x, ", remove, ")", sep = "")
    )
  x_removed <- eval(filter_func)
  x_keep <- suppressMessages(dplyr::anti_join(x, x_removed))

  if (!is.null(log_file) & nrow(x_removed) > 0) {
    readr::write_csv(x_removed, log_file)
  }

  message("Problematic subjects removed: ", nrow(x_removed))

  return(x_keep)
}
