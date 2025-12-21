splitString <- function(x,
                        splittingValuesRegex = rock::opts$get("splittingValuesRegex")) {

  res <-
    strsplit(
      x,
      splittingValuesRegex
    );

  ### Replace ending newlines with empty character strings
  res <-
    lapply(
      seq_along(x),
      function(i) {
        if (grepl("\\n\\s*$", x[i])) {
          return(
            c(
              res[[i]],
              ""
            )
          );
        } else {
          return(
            res[[i]]
          );
        }
      }
    );

  ### Retain empty elements (empty lines)
  res <- lapply(res, function(x) {
    if (length(x) == 0) {
      return("");
    } else {
      return(x);
    }
  });

  return(
    unlist(
      res
    )
  );

}
