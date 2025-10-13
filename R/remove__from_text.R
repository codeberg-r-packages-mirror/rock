#' Removes ROCK elements from text
#'
#' @param x The text as a character vector.
#'
#' @returns The character vector.
#'
#' @export
#' @rdname remove_rock_from_text
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
#' ### Extract notes
#' rock::remove_rock_codes_from_text(loadedExample);
#'
remove_rock_codes_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_breaks_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_uids_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_anchors_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_comments_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_nesting_from_text <- function(x) {

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
#' @rdname remove_rock_from_text
remove_rock_from_text <- function(x) {

  x <- remove_rock_codes_from_text(x);
  x <- remove_rock_uids_from_text(x);
  x <- remove_rock_breaks_from_text(x);
  x <- remove_rock_anchors_from_text(x);
  x <- remove_rock_comments_from_text(x);
  x <- remove_rock_nesting_from_text(x);

  return(x);

}
