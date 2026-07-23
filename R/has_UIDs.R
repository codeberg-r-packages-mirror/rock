#' Check whether a source has UIDs
#'
#' A convenience function to quickly check whether a source has UIDs - or at
#' least more than one.
#'
#' @param x The source as a character vector.
#'
#' @returns A single logical value.
#' @export
#'
#' @examples exampleText <- c(
#'   "Lorem ipsum dolor sit amet, consectetur",
#'   "adipiscing elit. Nunc non commodo ex,",
#'   "ac varius mi. Praesent feugiat nunc",
#'   "eget urna euismod lobortis. Sed",
#'   "hendrerit suscipit nisl, ac tempus",
#'   "magna porta et. Quisque libero massa,",
#'   "tempus vel tristique lacinia, tristique",
#'   "in nulla. Nam cursus enim dui, non",
#'   "ornare est tempor eu. Vivamus et massa",
#'   "consectetur, tristique magna eget,",
#'   "viverra elit."
#' );
#'
#' withUIDs <-
#'   rock::prepend_ids_to_source(
#'     exampleText
#'   );
#'
#' rock::has_UIDs(
#'   exampleText
#' );
#'
#' rock::has_UIDs(
#'   withUIDs
#' );
has_UIDs <- function(x) {

  return(
    length(rock::extract_uids(x)) > 0
  );

}
