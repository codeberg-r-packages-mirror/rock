#' Return which regular expression of several matches a character value
#'
#' @param pattern The character vector with regular expressions to match against
#' @param x The text to match against the regular expressions
#' @param ignore.case Whether to be case sensitive (see [base::grepl()]).
#' @param perl Whether `pattern` specifies Perl regexes (see [base::grepl()]).
#' @param fixed Whether `pattern` is a regex or fixed string (see [base::grepl()]).
#' @param useBytes See [base::grepl()].
#'
#' @returns A logical vector or array (of `x` has more than one element)
#' @export
#'
#' @examples pattern <-
#'   c("[0-9]",
#'     "[A-Z]",
#'     "[a-z]");
#'
#' rock::which_regex_matches(
#'   pattern,
#'   "42"
#' );
#'
#' rock::which_regex_matches(
#'   pattern,
#'   "forty-two"
#' );
#'
#' rock::which_regex_matches(
#'   pattern,
#'   c(42, "forty-two")
#' );
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
