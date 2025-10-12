#' Lines with specific contents
#'
#' These functions return a logical vector indicating on which lines the
#' specific content occurs.
#'
#' @param x The charactor vector to search.
#'
#' @returns A logical vector.
#' @export
#' @rdname lines_with
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-5.rock");
#'
#' ### Parse single example source
#' loadedExample <-
#'   rock::load_source(exampleFile);
#'
#' linesWithCodes <-
#'   lines_with_codes(loadedExample);
#' loadedExample[linesWithCodes];
#'
#' linesWithBreaks <-
#'   lines_with_breaks(loadedExample);
#' loadedExample[linesWithBreaks];
lines_with_codes <- function(x) {

  codeRegexes <- rock::opts$get('codeRegexes');
  codeValueRegexes <- rock::opts$get('codeValueRegexes');
  classInstanceRegex <- rock::opts$get('classInstanceRegex');
  networkCodeRegexes <- rock::opts$get('networkCodeRegexes');

  allRegexes <-
    c(codeRegexes,
      codeValueRegexes,
      classInstanceRegex,
      networkCodeRegexes);

  res <- rep(FALSE, length(x));

  for (currentRegex in allRegexes) {

    res <-
      res |
      grepl(
        currentRegex,
        x,
        perl = TRUE
      );

  }

  return(res);

}

#' @export
#' @rdname lines_with
lines_with_breaks <- function(x) {

  sectionRegexes <- rock::opts$get('sectionRegexes');

  res <- rep(FALSE, length(x));

  for (currentRegex in sectionRegexes) {

    res <-
      res |
      grepl(
        currentRegex,
        x,
        perl = TRUE
      );

  }

  return(res);

}

#' @export
#' @rdname lines_with
lines_with_anchors <- function(x) {

  anchorRegex <- rock::opts$get('anchorRegex');

  return(
    grepl(
      anchorRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_uids <- function(x) {

  uidRegex <- rock::opts$get('uidRegex');

  return(
    grepl(
      uidRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_comments <- function(x) {

  ignoreRegex <- rock::opts$get('ignoreRegex');

  return(
    grepl(
      ignoreRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_nesting <- function(x) {

  nestingMarker <- rock::opts$get('nestingMarker');

  return(
    grepl(
      paste0("^\\s*", nestingMarker, "+"),
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_whitespaceOnly <- function(x) {

  return(
    grepl(
      paste0("^\\s*$"),
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname remove_from_text
remove_codes_from_text <- function(x) {

  codeRegexes <- rock::opts$get('codeRegexes');
  codeValueRegexes <- rock::opts$get('codeValueRegexes');
  classInstanceRegex <- rock::opts$get('classInstanceRegex');
  networkCodeRegexes <- rock::opts$get('networkCodeRegexes');

  allRegexes <-
    c(codeRegexes,
      codeValueRegexes,
      classInstanceRegex,
      networkCodeRegexes);

  for (currentRegex in allRegexes) {

    x <-
      gsub(
        currentRegex,
        "",
        x,
        perl = TRUE
      );

  }

  return(trimws(x));

}


#' @export
#' @rdname remove_from_text
remove_breaks_from_text <- function(x) {

  sectionRegexes <- rock::opts$get('sectionRegexes');

  for (currentRegex in sectionRegexes) {

    x <-
      gsub(
        currentRegex,
        "",
        x,
        perl = TRUE
      );

  }

  return(trimws(x));

}

#' @export
#' @rdname remove_from_text
remove_uids_from_text <- function(x) {

  uidRegex <- rock::opts$get('uidRegex');

  x <-
    gsub(
      uidRegex,
      "",
      x,
      perl = TRUE
    );

  return(trimws(x));

}

#' @export
#' @rdname remove_from_text
remove_anchors_from_text <- function(x) {

  anchorRegex <- rock::opts$get('anchorRegex');

  x <-
    gsub(
      anchorRegex,
      "",
      x,
      perl = TRUE
    );

  return(trimws(x));

}

#' @export
#' @rdname remove_from_text
remove_comments_from_text <- function(x) {

  ignoreRegex <- rock::opts$get('ignoreRegex');

  x <-
    gsub(
      ignoreRegex,
      "",
      x,
      perl = TRUE
    );

  return(trimws(x));

}

#' @export
#' @rdname remove_from_text
remove_nesting_from_text <- function(x) {

  nestingMarker <- rock::opts$get('nestingMarker');

  x <-
    gsub(
      paste0("^\\s*", nestingMarker, "+"),
      "",
      x,
      perl = TRUE
    );

  return(trimws(x));

}

#' @export
#' @rdname lines_with
remove_rock_from_text <- function(x) {

  x <- remove_codes_from_text(x);
  x <- remove_uids_from_text(x);
  x <- remove_breaks_from_text(x);
  x <- remove_anchors_from_text(x);
  x <- remove_comments_from_text(x);
  x <- remove_nesting_from_text(x);

  return(x);

}


#' @export
#' @rdname lines_with
lines_with_data <- function(x) {



  res <- rep(FALSE, length(x));

  return(res);

}

