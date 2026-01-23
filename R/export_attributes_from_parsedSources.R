#' Export the attributes in a parsed source(s) object to YAML
#'
#' @param x A `rock_parsedSource` or `rock_parsedSources` object.
#' @param file The filename to write to; pass `NULL` to only return the YAML as a character vector
#' @param path If not `NULL`, this will be combined with the `file` argument
#'
#' @returns The YAML as a character vector
#' @export
#'
#' @examples
#'
export_attributes_from_parsedSources <- function(x,
                                                 file = "ROCK_attributes.rock",
                                                 path = NULL,
                                                 preventOverwriting = rock::opts$get("preventOverwriting")) {

  if (inherits(x, "rock_parsedSource")) {

    if (is.null(x$attributes) || all(is.na(x$attributes)) || (length(x$attributes) == 0)) {

      res <- yaml::as.yaml(list(ROCK_attributes = NULL));

    } else {

      res <- yaml::as.yaml(
        list(
          ROCK_attributes = unname(x$attributes)
        )
      );

    }

  } else if (inherits(x, "rock_parsedSources")) {

    ### Keep separate for potentially implementing differential behavior later

    if (is.null(x$attributesDf) || (nrow(x$attributesDf) == 0)) {

      res <- yaml::as.yaml(list(ROCK_attributes = NULL));

    } else {

      res <-
        list(ROCK_attributes =
               apply(
                 parsedExamples$attributesDf,
                 1,
                 as.list,
                 simplify = FALSE
               )
        );

      ### Note: replace ".na.character" with NULL ('~' in YAML) or and empty string ("")

      res <- yaml::as.yaml(res);

    }

  } else {

    stop("As `x`, pass an object as produced by rock::parse_source() or rock::parse_sources() ",
         "- you passed an object with class(es) ", vecTxtQ(class(x)), ".");

  }

  if (!is.null(path)) {
    if (!dir.exists(path)) {
      stop("The path you passed as `path` ('", path, "') does not exist!");
    }
    file <- file.path(path, file);
  }

  if (is.null(file)) {
    return(res);
  } else {
    if (file.exists(file) && preventOverwriting) {
      warning("The file you specified to write to, '", file, "', exists, ",
              "and `preventOverwriting` is set to TRUE, to not writing to disk!");
    } else {
      writeLines(
        res,
        file
      );
    }
    return(invisible(res));
  }

}
