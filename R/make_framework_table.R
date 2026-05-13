#' Make a framework table
#'
#' @param parsedSources The object with parsed ROCK sources
#' @param classId The identifier of the class the instances of which should
#' form the framework table rows
#' @param codeAggregation How to aggregate codes (to prevent too many columns);
#' either `none` (do not aggregate codes), `maxLeafs` (the maximum number of
#' leaves (codes without children or 'sub-codes') to allow; with a higher
#' number, all codes are collapsed into their parent code) or `maxDepth`
#' (the maximum depth of codes to include; all codes at a higher 'depth'
#' (farther away from the root; the highest-level codes are level 1, their
#' children or 'sub-codes' are level 2, *their* children or 'sub-codes' are
#' level 3, and so on) are collapsed into their parent code).
#' @param maxLeafs When aggregating codes, the maximum number of leaves (codes
#' without children or 'sub-codes') to allow; with a higher number, all codes
#' are collapsed into their parent code
#' @param maxDepth When aggregating codes, (the maximum depth of codes to
#' include; all codes at a higher 'depth' (farther away from the root; the
#' highest-level codes are level 1, their children or 'sub-codes' are level 2,
#' *their* children or 'sub-codes' are level 3, and so on) are collapsed into
#' their parent code.
#' @param codeSelectionRegex A regular expression against which to match the codes to form
#' the framework table columns - this is matched against the original applied
#' codes, and so in combination with the code aggregation
#' @param classInstanceSelectionRegex A regular expression against which to match the
#' class instance identifiers to be included in the rows
#'
#' @returns A framework table object; a list containing the framework
#' table as a data frame in its $framework_table slot
#' @export
#'
#' @examples
make_framework_table <- function(parsedSources,
                                 classId,
                                 codeAggregation = "maxLeafs",
                                 codeAggregation_maxLeafs = 5,
                                 codeAggregation_maxDepth = 2,
                                 codeSelectionRegex = ".*",
                                 classInstanceSelectionRegex = ".*") {

  initialCodes <-


  if (!is.null(codeAggregation) && (length(codeAggregation) == 1) && (codeAggregation == "maxLeafs")) {

  } else if (!is.null(codeAggregation) && (length(codeAggregation) == 1) && (codeAggregation == "maxDepth")) {

  } else {

    stop("As `codeAggregation`, you can only pass 'maxLeafs' or 'maxDepth'. ",
         "However, you passed '", codeAggregation, "'.")

  }

  browser();


}
