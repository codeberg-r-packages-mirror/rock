#' Helper functions for note extraction
#'
#' These are normally not used directly by end users; they're used
#' internally to extract notes.
#'
#' @param x A numeric vector of value 1 (for [nr_in_interval()] and more for
#' the other functions.
#' @param interval,intervals One or more intervals: sets of two or more line
#' numbers, with the minimum and maximum line numbers forming an internal
#'
#' @returns A logical value or vector for [nr_in_interval()],
#' [nr_in_any_interval()], and [nrs_in_any_interval()]; a list for
#' [find_blocks_of_lines()]
#' @export
#'
#' @rdname note_extraction_helper_functions
#' @examples rock::nr_in_interval(5, 7:10);
#' rock::nr_in_interval(8, 7:10);
#'
#' rock::nr_in_intervals(
#'   5,
#'   c(c(1,4), 7:10)
#' );
#'
#' rock::find_blocks_of_lines(
#'   c(
#'     1, 2, 5, 7, 8, 9, 10,
#'     18, 20, 23, 24, 25, 30
#'   )
#' );
nr_in_interval <- function(x, interval) {

  return(!(x < min(interval) | (x > max(interval))));

}

#' @export
#' @rdname note_extraction_helper_functions
nr_in_any_interval <- function(x, intervals) {

  if (length(x) > 1) {
    return(
      unlist(
        lapply(
          x,
          nr_in_any_interval,
          intervals = intervals
        )
      )
    );
  } else {
    return(
      any(
        unlist(
          lapply(
            intervals,
            nr_in_interval,
            x = x
          )
        )
      )
    );
  }

}

nrs_in_any_interval <- function(x, intervals) {

  return(
    unlist(
      lapply(
        x,
        nr_in_any_interval,
        intervals = intervals
      )
    )
  );

}

#' @export
#' @rdname note_extraction_helper_functions
find_blocks_of_lines <- function(x) {

  res <- list(x[1]);

  if (length(x) == 1) {
    return(res);
  }

  indices <- seq_along(x);
  resultIndex <- 1;
  currentIndex <- 2;

  while (currentIndex <= max(indices)) {

    if ((x[currentIndex - 1]) == (x[currentIndex] - 1)) {

      ### The line we're looking at now immediately follows the last line
      ### we looked at, so we're still in the same block.

      res[[resultIndex]] <- c(res[[resultIndex]], x[currentIndex]);

    } else {

      ### There's some distance between the line we're looking at now and
      ### the last line we looked at, so start a new block.
      resultIndex <- resultIndex + 1;
      res[[resultIndex]] <- x[currentIndex];

    }

    currentIndex <- currentIndex + 1;

  }

  return(res);

}


