#' Mix and match lists
#'
#' Add elements from `y` to `x` only if they do not yet
#' exist in `x`. Without names, just `x` is returned.
#'
#' @param x,y Named lists.
#'
#' @returns The merged list.
#' @export
#'
#' @examples rock::mix_and_match_lists(
#'   list(a=1, b=4, c=7),
#'   list(b=9, d=9)
#' );
mix_and_match_lists <- function(x, y) {

  if (is.null(x)) {
    return(y);
  } else if (is.null(y)) {
    return(x);
  }

  if (!is.list(x)) {
    x <- as.list(x);
  }

  if (!is.list(y)) {
    y <- as.list(y);
  }

  x <-
    c(
      x,
      y[setdiff(names(y), names(x))]
    );

  return(x);

}
