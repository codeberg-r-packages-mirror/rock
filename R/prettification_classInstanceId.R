prettification_classInstanceId <- function(classId,
                                           classInstanceId,
                                           attributes = NULL,
                                           format = NULL) {

  instanceFormatting <- rock::opts$get("instanceFormatting");
  instanceFormatting_html <- rock::opts$get("instanceFormatting_html");

  if (is.null(attributes)) {

    if (is.null(format)) {

      return(
        sprintf(
          instanceFormatting,
          classInstanceId
        )
      );

    } else if (format == "html") {

      return(
        sprintf(
          instanceFormatting_html,
          classInstanceId
        )
      );

    }

  } else {

    if (is.null(format)) {

      res <-
        paste0(
          "### Class instance identifier: `",
          classInstanceId,
          "`\n"
        );

      res <-
        paste0(
          res,
          paste0(
            "  - `",
            names(attributes),
            "` = ",
            attributes,
            "\n"
          )
        );

      res <- paste0(res, "\n\n");

    } else if (format == "html") {

      res <-
        paste0(
          "\n\n<div class='rock-class-instance'><strong>Class instance identifier: <pre>",
          classInstanceId,
          "</pre></strong>\n",
          "<div class='rock-class-instance-attributes'>"
        );

      res <-
        paste0(
          res,
          paste0(
            "<div class='rock-class-instance-single-attribute'>",
            names(attributes),
            " = ",
            attributes,
            "</div>"
          )
        );

      res <- paste0(res, "</div></div>\n\n");

    }

  }

  return(res);

}

