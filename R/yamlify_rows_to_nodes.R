#' Convert rows in a data frame to 'YAML' nodes
#'
#' @param x The data frame
#' @param keyCol Optionally, a column used to name each YAML node. If set to
#' `NA`, the row names will be used as keys.
#' @param returnYAML Whether to return 'YAML' (`TRUE`) or a list (`FALSE`)
#'
#' @returns 'YAML' as character string or a list of lists
#' @export
#'
#' @examples cat(
#'   rock::yamlify_rows_to_nodes(
#'     mtcars[1:3, ]
#'   )
#' );
yamlify_rows_to_nodes <- function(x,
                                  keyCol = NULL,
                                  returnYAML = TRUE,
                                  colsToInclude = NULL,
                                  colsToOmit = NULL) {

  if (is.null(colsToInclude)) {
    colsToInclude <- names(x);
  }

  if (!is.null(colsToOmit)) {
    colsToInclude <- setdiff(colsToInclude, colsToOmit);
  }

  res <-
    apply(
      x[, colsToInclude],
      1,
      as.list,
      simplify = FALSE
    );

  if (is.null(keyCol)) {
    res <- unname(res);
  } else if (!is.na(keyCol)) {
    names(res) <-
      x[, keyCol];
  }

  if (returnYAML) {
    res <-
      yaml::as.yaml(res);
  }

  return(res);

}
