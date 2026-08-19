#' Produce ROCK attributes in YAML format from a data frame or spreadsheet
#'
#' This function takes a data frame and then writes them to a file in the
#' correct format of ROCK attributes.
#'
#' Note that if the `output` file has the extension `.yml` or `.yaml`, the
#' delimiters passed in `delimiterString` will be ignored.
#'
#' @param x For `attributes_from_df`, the data frame; and
#' for `attributes_from_spreadsheet`, the path to the spreadsheet
#' (an argument to [rock::read_spreadsheet()]).
#' @param classInstanceId_col The column name of the column with the class
#' instance identifiers.
#' @param colsToInclude The column names of the columns with the attributes.
#' @param output The filename to write the attributes to.
#' @param delimiterString When exporting to `.rock` format (as opposed to
#' `.yml` or `.yaml`), the delimiter to use to delimit the YAML.
#' @param attributeContainer The name of the root-level YAML node to store the
#' attributes in (the ROCK standard specifies this should
#' be `ROCK_attributes`).
#'
#' @returns The produced YAML as a character vector (invisibly if `output`
#' is `NULL`).
#'
#' @export
#' @rdname attributes_from_rectangular_data
#'
#' @examples
attributes_from_df <- function(x,
                               classInstanceId_col,
                               colsToInclude = names(x),
                               output = NULL,
                               delimiterString = rock::opts$get('delimiterString'),
                               attributeContainer = rock::opts$get('attributeContainers')[1],
                               silent = rock::opts$get('silent'),
                               encoding = rock::opts$get('encoding')) {

  if (!inherits(x, "data.frame")) {
    stop(
      "As `x`, pass a data frame! You passed on object with class(es) ",
      vecTxtQ(x), "."
    );
  }

  if (!(classInstanceId_col %in% names(x))) {
    stop(
      "As `classInstanceId_col`, you did not pass a column name that exists in ",
      "the data frame you passed! The column name you passed in '",
      classInstanceId_col, "', and the column names in the data frame are ",
      vecTxtQ(names(x)), "."
    );
  }

  if (!(classInstanceId_col %in% colsToInclude)) {

    colsToInclude <- c(classInstanceId_col, colsToInclude);

  }

  if (!is.null(output)) {
    if (tolower(tools::file_ext(output)) %in% c("yml", "yaml")) {
      delimiterString <- "";
    }
  }

  x <- x[, colsToInclude];

  cols_to_ciids <- classInstanceId_col;
  cols_to_attributes <- setdiff(colsToInclude, classInstanceId_col);

  colsToInclude <- c(cols_to_ciids, cols_to_attributes);

  attributeList <-
    apply(x, 1, as.list, simplify = FALSE);

  res <-
    attributeList_to_yaml(
      attributeList,
      delimiterString = delimiterString,
      attributeContainer = attributeContainer
    );

  if (is.null(output)) {

    return(res);

  } else {

    writeTxtFile(
      x = res,
      output = output,
      encoding = encoding,
      preventOverwriting = preventOverwriting,
      silent = silent
    );

    return(invisible(res));

  }

}
