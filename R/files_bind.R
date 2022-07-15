#' Bind Multiple Files
#'
#' Binds multiple files in the same directory into a single data frame.
#' Bind can occur by "rows" or "columns".
#' @param path Folder location of files to be binded
#' @param pattern Pattern to identify files to be binded
#' @param delim Delimiter used in files. Passed onto `readr::read_delim()`
#' @param output_file File name and path to be saved to.
#' @export
#'

files_bind <- function(path = "", pattern = "", delim = ",",
                       output_file = "",
                       bind = "rows") {

  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)) {
    if (delim == ",") {
      import[[i]] <- readr::read_csv(filelist[[i]], guess_max = guess_max)
    }

    if (delim != ",") {
      import[[i]] <- readr::read_delim(filelist[[i]],
                                       delim,
                                       escape_double = FALSE,
                                       trim_ws = TRUE)
    }
  }

  if (bind == "rows") {
    bound <- dplyr::bind_rows(import)
  }
  if (bind == "columns" | bind == "cols") {
    bound <- dplyr::bind_cols(import)
  }

  if (output_file != "") {
    readr::write_csv(bound, output_file)
  }

  return(bound)
}
