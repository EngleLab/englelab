#' Check and Remove Duplicate IDs
#'
#' Checks and removes duplicate subject ids that can happen when misentered
#' during task administration
#' @param x dataframe
#' @param id Subject ID variable name.
#' @param unique Column names that are unique and should be used to
#'     check for duplicate id's (session date and/or time)
#' @param n Number of unique id's expected (default: 1). More than 1 if data
#'     file contains multiple sessions per subject id
#' @param remove logical. Remove duplicate ids from data? (default: TRUE)
#' @param keep_by If remove = TRUE, should one or more of the dupilcate id's be kept?
#'     options: "none", "first date"
#' @param save_as Folder path and file name to output the duplicate ID's
#' @export

duplicates_check <- function(x, id = "Subject",
                             unique = c("SessionDate", "SessionTime"),
                             n = 1, remove = TRUE, keep_by = "none",
                             save_as = NULL){
  # get duplicate ids
  duplicates <- dplyr::select(x, id, dplyr::all_of(unique))
  duplicates <- dplyr::distinct(duplicates)
  duplicates <- dplyr::group_by(duplicates, dplyr::across(id))
  duplicates <- dplyr::mutate(duplicates, count = n())
  duplicates <- dplyr::ungroup(duplicates)
  duplicates <- dplyr::filter(duplicates, count > n)
  duplicates <- dplyr::select(duplicates, id, dplyr::all_of(unique))

  # save duplicates to file
  if (!is.null(save_as)) {
    if (nrow(duplicates) > 0) {
      folder <- dirname(save_as)
      if (dir.exists(folder) == FALSE) {
        dir.create(folder, showWarnings = FALSE)
      }
      readr::write_csv(duplicates, save_as)
    }
  }

  # remove duplicates
  if (nrow(duplicates) > 0) {

    if (remove == TRUE) {
      if (keep_by == "none") {
        ids_remove <- duplicates[[id]]
        x <- dplyr::filter(x, !(get(id) %in% ids_remove))
        message("duplicates_check: Duplicate IDs found AND removed!")
      }
      if (keep_by == "first date") {
        remove_bydate <- dplyr::group_by(duplicates, dplyr::across(id))
        remove_bydate <- dplyr::arrange(remove_bydate, dplyr::across(unique))
        remove_bydate <- dplyr::slice(remove_bydate, -1)
        x <- dplyr::anti_join(x, remove_bydate,  by = c(id, unique))
        message("duplicates_check: Kept one duplicate that occured first by date.",
                " All others were removed.")
        ids_remove <- remove_bydate[[id]]
      }

      message(cat(ids_remove))
    }

    if (remove == FALSE) {
      message("duplicates_check: Duplicate IDs found but not removed!")
      message(cat(duplicates[[id]]))
    }

  } else {
    message("duplicates_check: No duplicate IDs found!")
  }
  return(x)
}
