#' @rdname extract_rock_elements
#' @export
#' @examples ### Extract network codes
#' treeCodes <-
#'   rock::extract_rock_network_codes_from_text(
#'     loadedExample
#'   );
#'
extract_rock_network_codes_from_text <- function(x) {

  codeRegexes <- rock::opts$get('networkCodeRegexes');

  codeRegexTypes <- names(codeRegexes);

  res <- list(codes = list());

  for (codeRegexType in codeRegexTypes) {

    res$codes[[codeRegexType]]$lines <-
      grepl(
        codeRegexes[codeRegexType],
        x,
        perl = TRUE
      );

    res$codes[[codeRegexType]]$codings <-
      regmatches(x,
                 gregexpr(codeRegexes[codeRegexType], x, perl=TRUE));

  }

  if (length(codeRegexTypes) == 1) {

    res$allCodings_lines <-
      which(res$codes[[codeRegexTypes]]$lines);

  } else {

    res$allCodings_lines <-
      sort(
        unique(
          unlist(
            lapply(
              res$codes,
              function(currentCodeObject) {
                return(which(currentCodeObject$lines));
              }
            )
          )
        )
      );

  }

  if (length(codeRegexTypes) == 1) {

    res$allCodings <-
      res$codes[[codeRegexTypes]]$codings;

  } else {

    res$allCodings <-
      lapply(
        seq_along(x),
        function(i) {
          return(
            unname(
              unlist(
                lapply(
                  res$codes,
                  function(currentCodeObject) {
                    return(currentCodeObject$codings[[i]]);
                  }
                )
              )
            )
          );
        }
      );

  }

  res$text <- x;
  res$text_clean <-
    remove_rock_codes_from_text(x);

  return(res);

}
