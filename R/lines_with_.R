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
lines_with_rock_codes <- function(x) {

  codeRegexes <- rock::opts$get('codeRegexes');
  codeValueRegexes <- rock::opts$get('codeValueRegexes');
  classInstanceRegex <- rock::opts$get('classInstanceRegex');
  networkCodeRegexes <- rock::opts$get('networkCodeRegexes');

  allRegexes <-
    c(codeRegexes,
      codeValueRegexes,
      classInstanceRegex,
      networkCodeRegexes);

  nonYamlLines <- !lines_with_yaml(x);

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

  return(nonYamlLines & res);

}

#' @export
#' @rdname lines_with
lines_with_rock_breaks <- function(x) {

  sectionRegexes <- rock::opts$get('sectionRegexes');

  nonYamlLines <- !lines_with_yaml(x);

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

  return(nonYamlLines & res);

}

#' @export
#' @rdname lines_with
lines_with_rock_anchors <- function(x) {

  nonYamlLines <- !lines_with_yaml(x);

  anchorRegex <- rock::opts$get('anchorRegex');

  return(
    nonYamlLines &
    grepl(
      anchorRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_rock_uids <- function(x) {

  nonYamlLines <- !lines_with_yaml(x);

  uidRegex <- rock::opts$get('uidRegex');

  return(
    nonYamlLines &
    grepl(
      uidRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_rock_comments <- function(x) {

  nonYamlLines <- !lines_with_yaml(x);

  ignoreRegex <- rock::opts$get('ignoreRegex');

  return(
    nonYamlLines &
    grepl(
      ignoreRegex,
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_rock_nesting <- function(x) {

  nonYamlLines <- !lines_with_yaml(x);

  nestingMarker <- rock::opts$get('nestingMarker');

  return(
    nonYamlLines &
    grepl(
      paste0("^\\s*", nestingMarker, "+"),
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_yaml <- function(x) {

  return(
    seq_along(x) %in%
      unlist(yum::find_yaml_fragment_indices(text=x))
  );

}


#' @export
#' @rdname lines_with
lines_with_rock_notes <- function(x) {

  noteRegex_complete <- rock::opts$get('noteRegex_complete');
  noteRegex_openingOnly <- rock::opts$get('noteRegex_openingOnly');
  noteRegex_closingOnly <- rock::opts$get('noteRegex_closingOnly');
  noteRegex_extractionRegex <- rock::opts$get('noteRegex_extractionRegex');
  noteRegex_keyvalue <- rock::opts$get('noteRegex_keyvalue');

  noteLines_complete <- grep(noteRegex_complete, x, perl = TRUE);
  noteLines_openingOnly <- grep(noteRegex_openingOnly, x, perl = TRUE);
  noteLines_closingOnly <- grep(noteRegex_closingOnly, x, perl = TRUE);
  noteLines_keyvalue <- grep(noteRegex_keyvalue, x, perl = TRUE);

  noteLines_hasOpening <- union(noteLines_complete, noteLines_openingOnly);
  noteLines_hasClosing <- union(noteLines_complete, noteLines_closingOnly);

  noteLines_any <-
    sort(
      unique(
        c(
          noteLines_complete,
          noteLines_openingOnly,
          noteLines_closingOnly,
          noteLines_keyvalue
        )
      )
    );

  if (length(noteLines_openingOnly) > 0) {

    noteLines_spanning <-
      lapply(
        noteLines_openingOnly,
        function(lineIndex) {
          return(
            lineIndex:min(noteLines_closingOnly[noteLines_closingOnly > lineIndex])
          )
        }
      );

    noteLines_all <-
      sort(
        unique(
          c(
            noteLines_any,
            unlist(noteLines_spanning)
          )
        )
      );

  } else {

    noteLines_all <- noteLines_any;

  }

  res <-
    ifelse(
      seq_along(x) %in% noteLines_all,
      TRUE,
      FALSE
    );

  nonYamlLines <- !lines_with_yaml(x);

  return(nonYamlLines & res);

}

#' @export
#' @rdname lines_with
lines_with_whitespaceOnly <- function(x) {

  nonYamlLines <- !lines_with_yaml(x);

  return(
    nonYamlLines &
    grepl(
      paste0("^\\s*$"),
      x,
      perl = TRUE
    )
  );

}

#' @export
#' @rdname lines_with
lines_with_data <- function(x) {

  res <-
    trimws(remove_rock_from_text(x));

  res <-
    nchar(res) > 0;

  return(res);

}

