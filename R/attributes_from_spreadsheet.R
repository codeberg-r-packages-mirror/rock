#' @export
#'
#' @rdname attributes_from_rectangular_data
#'
#' @param col_renaming A named character vector where the names are the
#' current column names and the values are the new names to be used when
#' writing the corresponding attributes.
#' @param sheet The index (or name) of the worksheet containing the attributes.
#'
#' @examples
attributes_from_spreadsheet <- function(x,
                                        classInstanceId_col,
                                        colsToInclude = names(x),
                                        col_renaming = NULL,
                                        sheet = 1,
                                        output = NULL,
                                        delimiterString = rock::opts$get('delimiterString'),
                                        attributeContainer = rock::opts$get('attributeContainers')[1],
                                        silent = rock::opts$get('silent'),
                                        encoding = rock::opts$get('encoding')) {

  if (!file.exists(x)) {
    stop(
      "As `x`, pass a path to an existing file! You passed '",
      x, "'."
    );
  }

  x <- rock::read_spreadsheet(
    x,
    sheet = sheet,
    flattenSingleDf = TRUE
  );

  if (!(classInstanceId_col %in% names(x))) {
    stop(
      "As `classInstanceId_col`, you did not pass a column name that exists in ",
      "the data frame you passed! The column name you passed in '",
      classInstanceId_col, "', and the column names in the data frame are ",
      vecTxtQ(names(x)), "."
    );
  }

  if (!is.null(output)) {
    if (tolower(tools::file_ext(output)) %in% c("yml", "yaml")) {
      delimiterString <- "";
    }
  }

  ### Make sure the ciid is the first element
  colsToInclude <- c(classInstanceId_col, setdiff(colsToInclude, classInstanceId_col));

  x <- x[, colsToInclude];

  ### Rename columns

  if (!is.null(col_renaming)) {

    for (i in seq_along(col_renaming)) {

      colIndex <-
        which(names(x) == names(col_renaming)[i]);

      names(x)[colIndex] <- col_renaming[i];

    }

  }

  ### Convert dataframe rows to YAML

  attributeList <-
    apply(x, 1, as.list, simplify = FALSE);

  ### Convert to YAML

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
