#' Return which regular expression of several matches a character value
#'
#' @param pattern
#' @param x
#' @param ignore.case
#' @param perl
#' @param fixed
#' @param useBytes
#'
#' @returns
#' @export
#'
#' @examples
which_regex_matches <- function(pattern,
                                x,
                                ignore.case = FALSE,
                                perl = TRUE,
                                fixed = FALSE,
                                useBytes = FALSE) {

  if (length(x) > 1) {

    res <-
      lapply(
        x,
        which_regex_matches,
        pattern = pattern,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        useBytes = useBytes
      );

    res <-
      do.call(
        cbind,
        res
      );

    colnames(res) <- x;

  } else if (length(x) == 1) {

    res <-
      unlist(
        lapply(
          pattern,
          grepl,
          x = x,
          ignore.case = ignore.case,
          perl = perl,
          fixed = fixed,
          useBytes = useBytes
        )
      );

    names(res) <- pattern;

  } else {

    stop("As `x`, pass either a single character value, or multiple; ",
         "but you now passed something with length 0.");

  }

  return(res);

}
