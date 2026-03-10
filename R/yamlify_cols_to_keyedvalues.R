#' Convert two columns in a data frame to a 'YAML' sequence of keyed values
#'
#' @param x The data frame
#' @param keyCol The column holding the keys (names)
#' @param valueCol The column holding the values
#' @param returnYAML Whether to return 'YAML' (`TRUE`) or a list (`FALSE`)
#'
#' @returns 'YAML' as character string or a list of lists
#' @export
#'
#' @examples cat(
#'   rock::yamlify_cols_to_keyedvalues(
#'     mtcars[1:2, ]
#'   )
#' );
yamlify_cols_to_keyedvalues <- function(x,
                                        keyCol = 1,
                                        valueCol = 2,
                                        returnYAML = TRUE) {

  res <- as.list(x[, valueCol]);
  names(res) <- x[, keyCol];

  if (returnYAML) {
    res <-
      yaml::as.yaml(res);
  }

  return(res);

}
