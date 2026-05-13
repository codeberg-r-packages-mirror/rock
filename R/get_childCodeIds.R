#' Get the code identifiers a code's descendents
#'
#' Get the code identifiers of all children, or all descendents (i.e. including
#' grand-children, grand-grand-children, etc) of a code with a given identifier.
#'
#' @param x The parsed sources object
#' @param childrenOnly Whether to only return 'immediate' / 'direct' children,
#' or the full descendancy (including grand-children, grand-grand children,
#' etc etc).
#' @param parentCodeId The code identifier of the parent code
#' @param returnNodes For `get_childCodeIds()`, set this to `TRUE` to return
#' a list of nodes, not just the code identifiers.
#' @param returnPaths Whether to return the paths (from the root) or the names.
#' @param includeParentCode Whether to include the parent code
#' identifier in the result
#'
#' @return A character vector with code identifiers (or a list of nodes)
#' @rdname get_childCodeIds
#' @export
get_childCodeIds <- function(x,
                             parentCodeId,
                             childrenOnly = TRUE,
                             returnNodes = FALSE,
                             returnPaths = FALSE,
                             includeParentCode = FALSE) {

  if (length(parentCodeId) > 1) {
    res <-
      lapply(
        parentCodeId,
        rock::get_childCodeIds,
        x = x,
        childrenOnly = childrenOnly,
        returnNodes = returnNodes,
        returnPaths = returnPaths,
        includeParentCode = includeParentCode
      );
    if (returnNodes || returnPaths) {
      return(res);
    } else {
      return(unlist(res));
    }
  }

  node <- rock::get_codeNode(x, parentCodeId);

  if (childrenOnly) {
    res <- node$children;
  } else {
    res <- data.tree::Traverse(
      node,
      traversal = "level",
      filterFun = function(x) {
        return(!(x$name == node$name));
      });
  }

  if (includeParentCode) {
    res <- c(node, res);
  }

  if (returnNodes) {
    return(res);
  } else {
    if (length(res) == 0) {
      return(NA);
    } else {
      if (returnPaths) {
        return(data.tree::Get(res, "path"));
      } else {
        return(unname(data.tree::Get(res, "name")));
      }
    }
  }

}
