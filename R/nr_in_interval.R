nr_in_interval <- function(x, interval) {

  return(!(x < min(interval) | (x > max(interval))));

}

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

find_blocks <- function(x) {

  indices <- seq_along(x);

  res <- list(character());

  resultIndex <- 1;
  currentIndex <- 1;

  while (currentIndex <= max(indices)) {

    ### We can't look for the next value if we're at max.
    if (currentIndex < max(indices)) {

      if ((x[currentIndex] + 1) == x[currentIndex + 1]) {

        res[[resultIndex]] <-
          c(res[[resultIndex]],
            x[currentIndex]);

      } else if ((x[currentIndex - 1]) == (x[currentIndex] - 1)) {

        res[[resultIndex]] <- c(res[[resultIndex]], x[currentIndex]);
        resultIndex <- resultIndex + 1;
        res[[resultIndex]] <- character();

      } else {

        resultIndex <- resultIndex + 1;
        res[[resultIndex]] <- character();

      }

    } else {

      ### There might only be one value
      if (currentIndex == 1) {

        res <- x[currentIndex];

      } else {

        if ((x[currentIndex - 1]) == (x[currentIndex] - 1)) {

          res[[resultIndex]] <- c(res[[resultIndex]], x[currentIndex]);

        } else {

          res[[resultIndex]] <- x[currentIndex];

        }

      }

    }

    currentIndex <- currentIndex + 1;

  }

  return(res);

}


