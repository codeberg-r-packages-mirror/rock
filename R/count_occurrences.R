#' Count code occurrences
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include, or,
#' alternatively, a character vector with literal code idenfitiers.
#' @param matchRegexAgainstPaths Whether to match the `codes` regular expression
#' against the full code paths or only against the code identifier.
#'
#' @return A [data.frame()].
#' @export
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-3.rock");
#'
#' ### Load example source
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show code occurrences
#' rock::count_occurrences(
#'   loadedExample
#' );
count_occurrences <- function(x,
                              codes = ".*",
                              matchRegexAgainstPaths = TRUE,
                              includeDescendents = FALSE,
                              includeAncestors = FALSE,
                              accumulateCounts = FALSE) {

  if ((!inherits(x, "rock_parsedSources")) && (!inherits(x, "rock_parsedSource"))) {

    stop("As `x`, you have to pass an object with one or more parsed sources, ",
         "as produced by a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. This object should have class ",
         "`rock_parsedSource` or `rock_parsedSources`, but the object ",
         "you passed has class(es) ", vecTxtQ(class(x)), ".");

  }

  if (length(codes) > 1) {
    codes <- paste(codes, collapse="|");
  }

  if (matchRegexAgainstPaths) {
    codesToInclude <-
      names(x$convenience$codingPaths)[
        grepl(
          codes,
          x$convenience$codingPaths,
          perl = TRUE
        )
      ];
  } else {
    codesToInclude <-
      x$convenience$codingLeaves[
        grepl(
          codes,
          x$convenience$codingLeaves,
          perl = TRUE
        )
      ];
  }

  if (includeDescendents) {
    codesToInclude <-
      c(
        codesToInclude,
        rock::get_childCodeIds(
          x = x,
          codesToInclude,
          childrenOnly = FALSE,
        )
      );
  }

  if (includeAncestors) {
    codesToInclude <-
      c(
        codesToInclude,
        rock::get_parentCodeIds(
          x = x,
          codesToInclude,
          parentOnly = FALSE,
        )
      );
  }

  codesToInclude <- unique(codesToInclude);

  codeNodesToInclude <-
    lapply(
      codesToInclude,
      rock::get_codeNode,
      x = x
    );

  browser();

  codingLeavesToInclude <-
    intersect(
      codesToInclude,
      x$convenience$codingLeaves
    );

  codingParentsToInclude <-
    setdiff(
      codesToInclude,
      c(x$convenience$codingLeaves,
        names(rock::opts$get("codeRegexes")))
    );

  leafCodesInQDT <-
    codingLeavesToInclude[(codingLeavesToInclude %in% names(x$qdt))];

  parentCodesInQDT <-
    codingParentsToInclude[(codingParentsToInclude %in% names(x$qdt))];

  leafCodesNotInQDT <-
    codingLeavesToInclude[!(codingLeavesToInclude %in% names(x$qdt))];

  parentCodesNotInQDT <-
    codingParentsToInclude[!(codingParentsToInclude %in% names(x$qdt))];

  codesToIncludeInQDT <-
    c(leafCodesInQDT, parentCodesInQDT);

  counts_total <-
    apply(
      x$qdt[, codesToIncludeInQDT],
      2,
      sum
    );

  totalUtterances <- nrow(x$qdt);

  totalCodings <- sum(x$qdt[, codesToIncludeInQDT]);

  totalCodedUtterances <-
    sum(
      as.numeric(
        apply(
          x$qdt[, codesToIncludeInQDT],
          1,
          function(row) {
            return(any(as.logical(row)));
          }
        )
      )
    );

  proportions_totalCodedUtterances <-
    counts_total / totalCodedUtterances;

  res <- data.frame(
    codeId = codesToIncludeInQDT,
    count = counts_total,
    totalCodedUtterances = totalCodedUtterances,
    totalUtterances = totalUtterances
  );

  return(res);

}

