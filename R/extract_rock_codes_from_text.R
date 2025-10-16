#' @rdname extract_rock_elements
#' @export
#' @examples ### Extract all codes
#' allCodes <-
#'   rock::extract_rock_codes_from_text(
#'     loadedExample
#'   );
#'
extract_rock_codes_from_text <- function(x) {

  res <- list();

  res$valueCodes <-
    rock::extract_rock_value_codes_from_text(x);
  res$classCodes <-
    rock::extract_rock_class_codes_from_text(x);
  res$networkCodes <-
    rock::extract_rock_network_codes_from_text(x);
  res$treeCodes <-
    rock::extract_rock_tree_codes_from_text(x);

  res$allCodedLines <-
    sort(
      unique(
        c(res$valueCodes$allCodings_lines,
          res$classCodes$allCodings_lines,
          res$networkCodes$allCodings_lines,
          res$treeCodes$allCodings_lines)
      )
    );

  res$text <- x;
  res$text_clean <-
    remove_rock_codes_from_text(x);

  return(res);

}
