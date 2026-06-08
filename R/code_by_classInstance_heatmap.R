#' Create a heatmap showing issues with items
#'
#' When conducting cognitive interviews, it can be useful to quickly inspect
#' the code distributions for each item. These heatmaps facilitate that
#' process.
#'
#' @param x The object with the parsed coded source(s) as resulting from a
#' call to [rock::parse_source()] or [rock::parse_sources()].
#' @param nrmSpec Optionally, an imported Narrative Response Model
#' specification, as imported with [rock::ci_import_nrm_spec()], which will
#' then be used to obtain the item labels.
#' @param language If `nrmSpec` is specified, the language to use.
#' @param itemOrder,itemLabels Instead of specifying an NRM specification,
#' you can also directly specify the item order and item labels. `itemOrder`
#' is a character vector of item identifiers, and `itemLabels` is a named
#' character vector of item labels, where each value's name is the
#' corresponding item identifier. If `itemLabels` is provided but `itemOrder`
#' is not, the order of the `itemLabel` is used.
#' @param wrapLabels Whether to wrap the labels; if not `NULL`, the
#' number of character to wrap at.
#' @param itemIdentifier The column identifying the items; the class instance
#' identifier prefix, e.g. if item identifiers are specified as
#' `[[uiid:familySize_7djdy62d]]`, the `itemIdentifier` to pass here
#' is `"uiid"`.
#' @param codingScheme The coding scheme, either as a string if it represents
#' one of the cognitive interviewig coding schemes provided with the `rock`
#' package, or as a coding scheme resulting from a call
#' to [rock::create_codingScheme()].
#' @param itemlab,codelab,freqlab Labels to use for the item and code axes
#' and for the frequency color legend (`NULL` to omit the label).
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
code_by_classInstance_heatmap <- function(x,
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
    rock::heatmap_basic(
      data = tidyCodeFrequencies,
      x = "code",
      y = classId,
      fill = "frequency",
      xLab = codeLab,
      yLab = classInstanceLab,
      fillLab = freqLab,
      plotTitle = plotTitle,
      fillScale = fillScale,
      theme = theme
    );

  return(heatMap);

}
