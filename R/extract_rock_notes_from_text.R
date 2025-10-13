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

  noteLines_all <-
    which(
      lines_with_rock_notes(
        x
      )
    );

  res$lines$noteLines_perNote <-
    find_blocks_of_lines(
      res$lines$noteLines_all
    );

  res$notes <-
    lapply(
      res$lines$noteLines_perNote,
      function(lineNrs) {

        lines_in_note <- lineNrs;

        keyValueLines_in_note <-
          which(
            res$lines$noteLines_keyvalue %in% lines_in_note
          );

        if (length(keyValueLines_in_note) > 0) {

          keyValueLines_in_note <-
            res$lines$noteLines_keyvalue[
              keyValueLines_in_note
            ];

          keys <- trimws(sub(noteRegex_keyvalue, "\\1", text[keyValueLines_in_note], perl=TRUE));
          values <- trimws(sub(noteRegex_keyvalue, "\\2", text[keyValueLines_in_note], perl=TRUE));

          keyValuePairs_in_note <- values;
          names(keyValuePairs_in_note) <- keys;

          lines_in_note <-
            setdiff(
              lines_in_note,
              keyValueLines_in_note
            );

        }

        currentNoteText <- text[lines_in_note];

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

  browser();


  return(res);

}
