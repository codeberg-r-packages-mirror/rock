#' Get a named list with vectors with all class instance identifiers
#'
#' @param x An object with one or more parsed ROCK sources
#'
#' @returns A named list of character vectors
#' @export
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Get all class instance identifiers
#' rock::get_all_classInstanceIds(
#'   parsedExample
#' );
get_all_classInstanceIds <- function(x) {

  if ((!inherits(x, "rock_parsedSources")) && (!inherits(x, "rock_parsedSource"))) {

    stop("As `x`, you have to pass an object with one or more parsed sources, ",
         "as produced by a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. This object should have class ",
         "`rock_parsedSource` or `rock_parsedSources`, but the object ",
         "you passed has class(es) ", vecTxtQ(class(x)), ".");

  }

  if (inherits(x, "rock_parsedSources")) {
    allClassIds <- x$convenience$allClassIds;
  } else {
    allClassIds <- x$convenience$allClasses;
  }

  res <- lapply(
    allClassIds,
    function(classId) {
      res <- unique(x$qdt[[classId]]);
      res <- res[!is.na(res)];
      res <- setdiff(res, "no_id");
      return(res);
    }
  );
  names(res) <- allClassIds;

  return(res);

}
