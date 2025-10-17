#' Produce the YAML for a ROCK codebook
#'
#' @param x The ROCK codebook
#'
#' @returns The YAML as a character vector
#' @export
#'
#' @examples data(exampleCodebook_1, package="rock");
#' cat(
#'   codebook_to_yaml(
#'     exampleCodebook_1
#'   )
#' );
codebook_to_yaml <- function(x) {

  if (!inherits(x, "rock_codebook_spreadsheet")) {
    stop("As `x`, you must pass a ROCK codebook in spreadsheet format, as ",
         "imported with `rock::codebook_fromSpreadsheet()`.");
  }

  res <-
    list(
      codebook = list(
        metadata = rock::yamlify_cols_to_keyedvalues(x$metadata,
                                                     keyCol = "field",
                                                     valueCol = "content"),
        codes = apply(x$codes, 1, as.list),
        aesthetics = rock::yamlify_rows_to_nodes(x$aesthetics)
      )
    );

  for (i in names(res$codes)) {
    res$codebook$codes[[i]]$examples <-
      rock::yamlify_rows_to_nodes(
        x$examples[x$examples$code_id == i, ],
        returnYAML = FALSE,
        colsToOmit = "code_id"
      );
    res$codebook$codes[[i]]$relationships <-
      rock::yamlify_rows_to_nodes(
        x$relationships[x$relationships$from_code_id == i, ],
        returnYAML = FALSE,
        colsToOmit = "from_code_id"
      );
  }

  yaml <-
    yaml::as.yaml(res);

  return(yaml);

}
