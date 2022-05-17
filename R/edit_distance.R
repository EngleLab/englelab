#' Calculates EditDistance.unit and EditDistance.load scores
#'
#' The Damerau-Levenshtein distance is implemented in the stringdist package:
#' van der Loo, M. P. J. (2014). The stringdist package for approximate string
#' matching. R Journal, 6(1), 111-122. doi:10.32614/RJ-2014-011
#'
#' @param x dataframe
#' @param target the column name containing the sequence of target memory items
#' @param recall the column name containing the sequence of recalled items
#' @export
#'

edit_distance <- function(x, target = "MemoryTargets", recall = "Recalled"){

  x <- dplyr::mutate(x,
                     distance = stringdist::stringdist(get(target), get(recall),
                                                       method = "dl"),
                     EditDistance.unit =
                       (nchar(get(target)) - distance) / nchar(get(target)),
                     EditDistance.load =
                       nchar(get(target)) - distance)
  x <- dplyr::select(x, -distance)

  return(x)
}


