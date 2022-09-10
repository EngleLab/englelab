#' Subject Missing Data Removal
#'
#' Remove subjects with too much missing data on a given latent construct
#' @param x dataframe
#' @param factor_list list of factors and tasks.
#'     e.g., list(WMC = c("OSpan", "RotSpan", SymSpan"))
#' @param missing_allowed Proportion of tasks allowed to be missing
#' @param id Subject ID variable
#' @export
#'

remove_missing <- function(x,
                          factor_list = list(),
                          missing_allowed = 1,
                          id = "Subject") {
  x.remove <- list()
  for (f in seq_along(factor_list)) {
    x.remove[[f]] <- dplyr::mutate(x, missing = 0)
    for (task in factor_list[[f]]) {
      x.remove[[f]] <-
        dplyr::mutate(x.remove[[f]],
                      missing = ifelse(is.na(get(task)), missing + 1, missing))
    }
    x.remove[[f]] <- dplyr::mutate(x.remove[[f]],
                                   missing = missing / length(factor_list[[f]]))
    x.remove[[f]] <- dplyr::filter(x.remove[[f]], missing > missing_allowed)
    x.remove[[f]] <- dplyr::select(x.remove[[f]], dplyr::contains(id), missing)
    colnames(x.remove[[f]])[which(colnames(x.remove[[f]]) == "missing")] <-
      paste(names(factor_list[f]), "missing", sep = ".")
  }
  x.remove <- plyr::join_all(x.remove, by = id, type = "full")
  subj.remove <- unique(x.remove$Subject)
  x <- dplyr::filter(x, !(Subject %in% subj.remove))

  return(x)
}
