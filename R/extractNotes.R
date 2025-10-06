#' Extract the notes from a text
#'
#' @param text The text as a character vector
#'
#' @returns An object containing the text, both in its original version
#' and in a cleaned version, as well as the notes.
#' @export
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
#' notes <-
#'   rock::extractNotes(loadedExample);
#'
extractNotes <- function(text) {

  noteRegex_complete <- rock::opts$get('noteRegex_complete');
  noteRegex_openingOnly <- rock::opts$get('noteRegex_openingOnly');
  noteRegex_closingOnly <- rock::opts$get('noteRegex_closingOnly');
  noteRegex_keyvalue <- rock::opts$get('noteRegex_keyvalue');

  noteLines_complete <- grep(noteRegex_complete, text, perl = TRUE);
  noteLines_openingOnly <- grep(noteRegex_openingOnly, text, perl = TRUE);
  noteLines_closingOnly <- grep(noteRegex_closingOnly, text, perl = TRUE);
  noteLines_keyvalue <- grep(noteRegex_keyvalue, text, perl = TRUE);

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

  res <-
    list(
      text = text,
      regexes =
        list(
          noteRegex_complete = noteRegex_complete,
          noteRegex_openingOnly = noteRegex_openingOnly,
          noteRegex_closingOnly = noteRegex_closingOnly,
          noteRegex_keyvalue = noteRegex_keyvalue
        ),
      lines =
        list(
          noteLines_complete = noteLines_complete,
          noteLines_openingOnly = noteLines_openingOnly,
          noteLines_closingOnly = noteLines_closingOnly,
          noteLines_keyvalue = noteLines_keyvalue,
          noteLines_any = noteLines_any
        )
    );

  if ((length(noteLines_any) == 0)) {

    return(res);

  }

  if (length(noteLines_keyvalue) > 0) {

    browser();

  }

  return(res);

}
