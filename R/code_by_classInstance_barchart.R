#' Create a bar chart showing code occurrences for each class instance
#'
#' This function creates a bar chart with class instances (or sources) in rows,
#' plotting the code occurrences for each code in bars.
#'
#' @param x The object with the parsed coded source(s) as resulting from a
#' call to [rock::parse_source()] or [rock::parse_sources()].
#' @param wrapLabels Whether to wrap the labels; if not `NULL`, the
#' number of character to wrap at.
#' @param classId The identifier of the class for which the class instances should
#' form the rows (the class identifier is the bit to the left of the `=` or `:` in
#' class instance codings).
#' @param codesRegex A regular expression specifying which codes identifiers
#' to select for the columns.
#' @param classInstanceRegex A regular expression specifying which class instance
#' identifiers to select for the rows.
#' @param classInstanceLab Labels to use for the rows (`NULL` to omit the label).
#' @param codeLab Labels to use for the codes (`NULL` to omit the label).
#' @param freqLab Labels to use for the cell colors (`NULL` to omit the label).
#' @param plotTitle The title to use for the plot
#' @param fillScale Convenient way to specify the fill scale (the colours)
#' @param theme Convenient way to specify the [ggplot2::ggplot()] theme.
#'
#' @return The heatmap as a ggplot2 plot.
#' @export
#'
#' @examples examplePath <-
#'   file.path(
#'     system.file(package="rock"), 'extdata'
#'   );
#'
#' parsedSources <- rock::parse_sources(
#'   examplePath,
#'   regex = "example-[123].rock"
#' );
#'
#' ### If no TSSIDs were configures, you can use
#' ### source filenames instead like this:
#' rock::code_by_classInstance_heatmap(
#'   parsedSources,
#'   classId = "originalSource"
#' );
code_by_classInstance_barchart <- function(x,
                                           wrapLabels = 80,
                                           classId = "tssid",
                                           codesRegex = ".*",
                                           classInstanceRegex = ".*",
                                           classInstanceLab = NULL,
                                           codeLab = NULL,
                                           freqLab = "Count",
                                           plotTitle = "Heatmap",
                                           fillScale = ggplot2::scale_fill_viridis_c(),
                                           theme = ggplot2::theme_minimal()) {

  if (!inherits(x, c("rock_parsedSource", "rock_parsedSources"))) {
    stop("As `x`, pass one or more parsed sources (as resulting from ",
         "a call to `rock::parse_source()` or `rock::parse_sources()`.");
  }

  allCodesToInclude <-
    grep(
      codesRegex,
      x$convenience$codings,
      value = TRUE
    );

  allClassInstancesToInclude <-
    grep(
      classInstanceRegex,
      stats::na.omit(unique(x$mergedSourceDf[, classId])),
      value = TRUE
    );

  nrOfCodes <- length(allCodesToInclude);

  nrOfInstances <- length(allClassInstancesToInclude);

  if (nrOfInstances == 0) {
    stop("No instance identifiers found!");
  }

  if (nrOfCodes == 0) {
    stop("No codes were found!");
  }

  if ((nrOfCodes*nrOfInstances) < 2) {
    stop("Only one class instance ('",
         vecTxtQ(allClassInstancesToInclude),
         "') was coded with one code ('",
         allCodesToInclude,
         "')!");
  }

  codingPaths_inverted <-
    stats::setNames(
      names(x$convenience$codingPaths),
      nm = x$convenience$codingPaths
    );

  names(codingPaths_inverted) <-
    gsub(
      "^codes>",
      "",
      names(codingPaths_inverted)
    );

  leaves_for_codes_to_include <-
    codingPaths_inverted[
      allCodesToInclude
    ];

  usedCodes <- intersect(
    leaves_for_codes_to_include,
    names(x$mergedSourceDf)
  );

  if (length(usedCodes) == 0) {
    stop("No codes were found!");
    return(invisible(NULL));
  }

  mergedSourceDf <-
    x$mergedSourceDf[
      x$mergedSourceDf[[classId]] %in% allClassInstancesToInclude,
      c(classId, usedCodes)
    ];

  codeFrequencyTable <-
    do.call(
      rbind,
      by(
        data = mergedSourceDf[, usedCodes],
        INDICES = mergedSourceDf[, classId],
        FUN = colSums
      )
    );

  tidyCodeFrequencies <-
    data.frame(
      rep(rownames(codeFrequencyTable), ncol(codeFrequencyTable)),
      rep(colnames(codeFrequencyTable), each=nrow(codeFrequencyTable)),
      as.vector(codeFrequencyTable)
    );
  names(tidyCodeFrequencies) <- c(classId, "code", "frequency");

  heatMap <-
    rock::barchart_basic(
      data = tidyCodeFrequencies,
      x = classId,
      y = "frequency",
      fill = "code",
      xLab = classInstanceLab,
      yLab = freqLab,
      fillLab = codeLab,
      plotTitle = plotTitle,
      fillScale = fillScale,
      theme = theme
    );

  return(heatMap);

}
