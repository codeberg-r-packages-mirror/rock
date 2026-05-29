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
#' @examples ### Extract notes
#' notes <-
#'   rock::extract_rock_notes_from_text(loadedExample);
#'
#' notes$notes[[3]];
#'
extract_rock_notes_from_text <- function(x) {

  noteRegex_extractionRegex <- rock::opts$get('noteRegex_extractionRegex');
  noteRegex_keyvalue <- rock::opts$get('noteRegex_keyvalue');

  linesWithData <- rock::lines_with_data(x);

  noteLines_all <-
    which(
      lines_with_rock_notes(
        x
      )
    );

  if (length(noteLines_all) == 0) {
    return(list(text = x,
                text_clean = x,
                notes = NA));
  }

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

        ### Find last preceding line with data
        currentNote$attached_to_line <-
          max(which((seq_along(x) <= min(lines_in_note)) & (linesWithData)));
        currentNote$attached_to_data <-
          remove_rock_from_text(x)[currentNote$attached_to_line];
        currentNote$attached_to_uid <-
          extract_rock_uids_from_text(x)$UIDs[currentNote$attached_to_line];

        return(currentNote);

      }
    );

  res <-
    list(text = x,
         text_clean = remove_rock_notes_from_text(x),
         notes = notes);

  return(res);

}
