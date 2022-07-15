#' Merge Multiple Files
#'
#' Merges multiple files located in the same directory into a single data frame
#' @param path Folder location of files to be merged
#' @param pattern Pattern string to identify files to be merged
#' @param delim Delimiter used in files. Passed onto `readr::read_delim()`
#' @param id Subject ID variable name. Passed onto `plyr::join_all(by = id)`
#' @param output_file File name and path to be saved to
#' @export
#'

files_join <- function(path = "", pattern = "", delim = ",",
                       id = "Subject", output_file = "") {

  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)) {
    if (delim == ",") {
      import[[i]] <- readr::read_csv(filelist[[i]])
    }

    if (delim == "\t") {
      import[[i]] <- readr::read_delim(filelist[[i]],
                                       delim,
                                       escape_double = FALSE,
                                       trim_ws = TRUE)
    }
  }

  merged <- plyr::join_all(import, by = id, type = "full")
  merged <- merged[, !duplicated(colnames(merged))]

  if (output_file != "") {
    readr::write_csv(bound, output_file, na = na)
  }

  return(merged)
}
