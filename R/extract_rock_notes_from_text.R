#' Extract ROCK elements a text
#'
#' @param text The text as a character vector.
#'
#' @returns An object containing the text, both in its original version
#' and in a version omitting the extracted elements, as well as the
#' extracted elements.
#' @rdname extract_rock_elements
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
#'   rock::extract_rock_notes_from_text(loadedExample);
#'
extract_rock_notes_from_text <- function(x) {

  noteRegex_extractionRegex <- rock::opts$get('noteRegex_extractionRegex');
  noteRegex_keyvalue <- rock::opts$get('noteRegex_keyvalue');

  noteLines_all <-
    which(
      lines_with_rock_notes(
        x
      )
    );

  noteLines_perNote <-
    find_blocks_of_lines(
      noteLines_all
    );

  notes <-
    lapply(
      noteLines_perNote,
      function(lineNrs) {

        lines_in_note <- lineNrs;

        keyValueLines_in_note <-
          lines_in_note[
            grep(
              noteRegex_keyvalue,
              x[lines_in_note],
              perl = TRUE
            )
          ];

        if (length(keyValueLines_in_note) > 0) {

          keys <- trimws(sub(noteRegex_keyvalue, "\\1", x[keyValueLines_in_note], perl=TRUE));
          values <- trimws(sub(noteRegex_keyvalue, "\\2", x[keyValueLines_in_note], perl=TRUE));

          keyValuePairs_in_note <- values;
          names(keyValuePairs_in_note) <- keys;

          lines_in_note <-
            setdiff(
              lines_in_note,
              keyValueLines_in_note
            );

        }

        currentNoteText <- x[lines_in_note];

        currentNoteText <-
          trimws(sub(noteRegex_extractionRegex, "\\1", currentNoteText));

        currentNoteText <-
          currentNoteText[
            nchar(currentNoteText) > 0
          ];

        currentNote <-
          list(text = currentNoteText,
               lines = lines_in_note);

        if (length(keyValueLines_in_note) > 0) {
          currentNote$keyedValues <-
            keyValuePairs_in_note
        } else {
          currentNote$keyedValues <- NULL;
        }

        return(currentNote);

      }
    );

  ### Find original sequence of last line that contains
  ### data; if it contains a UID, also get that

  linesWithData <- rock::lines_with_data(x);
  linesWithUIDs <- rock::lines_with_rock_uids(x);

  browser();

  lines_with_yaml

  if (any(linesWithUIDs)) {
    if (all(linesWithData == linesWithUIDs)) {

    }
  }

  browser();


  return(res);

}
