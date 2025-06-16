#' @rdname prettifying_sources
#' @export
knit_prettified_source <- function(x,
                                   heading = NULL,
                                   headingLevel = 2,
                                   add_html_tags = TRUE,
                                   output = NULL,
                                   template = "default",
                                   includeCSS = TRUE,
                                   preserveSpaces = TRUE,
                                   includeBootstrap = rock::opts$get("includeBootstrap"),
                                   preventOverwriting = rock::opts$get(preventOverwriting),
                                   silent=rock::opts$get(silent)) {

  res <-
    prettify_source(
      x = x,
      heading = heading,
      headingLevel = headingLevel,
      add_html_tags = add_html_tags,
      output = output,
      outputViewer = FALSE,
      template = template,
      includeCSS = includeCSS,
      preserveSpaces = preserveSpaces,
      includeBootstrap = includeBootstrap,
      preventOverwriting = preventOverwriting,
      silent = silent
    );

  return(
    knitr::asis_output(
      res
    )
  );

}

