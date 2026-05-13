#' Get the node from a code tree for a code identifier
#'
#' @param x The parsed sources object
#' @param codeId The code identifier of the code
#'
#' @return A [data.tree::Node()] object
#' @rdname get_codeNode
#' @export
get_codeNode <- function(x,
                         codeId) {

  if ((!inherits(x, "rock_parsedSources")) &&
      (!inherits(x, "rock_parsedSource"))) {
    stop("As `x`, you have to pass a parsed sources object. You passed ",
            deparse(substitute(x)), "', which instead has class(es) ",
            vecTxtQ(class(x)), ".");
  }

  if ((!is.character(codeId)) | (length(codeId) != 1)) {
    stop("As `codeId`, you have to pass a single character value. ",
         "However, what you passed is either not a character value, or ",
         "has a length other than 1 (i.e. 0, 2, or larger).");
  }

  if (inherits(x, "rock_parsedSource")) {
    node <- NULL;
    for (i in seq_along(x$inductiveCodeTrees)) {
      node <- NULL;
      if ((!is.null(x$inductiveCodeTrees[[i]])) && is.null(node)) {
        node <-
          data.tree::FindNode(x$inductiveCodeTrees[[i]], codeId);
      }
    }
  } else if (inherits(x, "rock_parsedSources")) {
    node <-
      data.tree::FindNode(x$fullyMergedCodeTrees, codeId);
  } else {
    stop("As `x`, you passed '",
         substitute(deparse(x)), "', but this does not have class ",
         "`rock_parsedSource` or `rock_parsedSources`.");
  }

  if (is.null(node)) {
    stop("In the parsed sources object that you passed (",
         substitute(deparse(x)), "), no code identifier '",
         codeId, "' was found.");
  }

  return(node);

}
