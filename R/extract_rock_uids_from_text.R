#' @rdname extract_rock_elements
#' @export
#' @examples ### Extract UIDs
#' UIDs <-
#'   rock::extract_rock_uids_from_text(loadedExample);
extract_rock_uids_from_text <- function(x) {

  uidRegex <- rock::opts$get('uidRegex');

  linesWithUIDs <-
    grepl(
      uidRegex,
      x,
      perl = TRUE
    );

  UIDs <-
    sub(
      paste0(".*", uidRegex, ".*"),
      "\\1",
      x,
      perl = TRUE
    );

  UID_vector <-
    ifelse(
      linesWithUIDs,
      UIDs,
      ""
    );

  cleanText <-
    remove_rock_uids_from_text(x);

  res <-
    list(
      text = x,
      text_clean = cleanText,
      UIDs = UID_vector
    );

  return(res);

}
