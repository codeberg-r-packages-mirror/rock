#' Produce the YAML for a ROCK codebook
#'
#' @param x The ROCK codebook
#'
#' @returns The YAML as a character vector
#' @export
#'
#' @examples
codebook_to_yaml <- function(x) {

  if (!inherits(x, "rock_codebook_spreadsheet")) {
    stop("As `x`, you must pass a ROCK codebook in spreadsheet format, as ",
         "imported with `rock::codebook_fromSpreadsheet()`.");
  }

  res <-
    list(
      metadata = x$metadata,
      codes = apply(x$codes, 1, as.list),
      aesthetics = x$aesthetics
    );

  names(res$codes) <-
    unlist(
      lapply(
        res$codes,
        function(currentCode) {
          return(currentCode$code_id);
        }
      )
    );

  for (i in names(res$codes)) {
    res$codes[[i]]$examples <-
      apply(x$examples[x$examples$code_id == i, ], 1, list);
    res$codes[[i]]$relationships <-
      as.list(x$relationships[x$examples$from_code_id == i, ]);
  }


  browser();

}
