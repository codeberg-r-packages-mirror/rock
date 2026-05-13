#' Get the code identifiers a code's ascendents
#'
#' Get the code identifiers of all parents, or all ascendents (i.e. including
#' grand-parents, grand-grand-parents, etc) of a code with a given identifier.
#'
#' @param x The parsed sources object
#' @param parentOnly Whether to only return the 'immediate' / 'direct' parent,
#' or the full ascendancy (including grand-parents, grand-grand parents,
#' etc etc).
#' @param childCodeId The code identifier of the parent code
#' @param returnNodes For `get_childCodeIds()`, set this to `TRUE` to return
#' a list of nodes, not just the code identifiers.
#' @param returnPaths Whether to return the paths (from the root) or the names.
#' @param includeChildCode Whether to include the child code
#' identifier in the result
#'
#' @return A character vector with code identifiers (or a list of nodes)
#' @rdname get_parentCodeIds
#' @export
get_parentCodeIds <- function(x,
                              childCodeId,
                              parentOnly = TRUE,
                              returnNodes = FALSE,
                              returnPaths = FALSE,
                              includeChildCode = FALSE) {

  if (length(parentCodeId) > 1) {
    res <-
      lapply(
        childCodeId,
        rock::get_parentCodeIds,
        x = x,
        parentOnly = parentOnly,
        returnNodes = returnNodes,
        returnPaths = returnPaths,
        includeChildCode = includeChildCode
      );
    if (returnNodes || returnPaths) {
      return(res);
    } else {
      return(unlist(res));
    }
  }

  node <- rock::get_codeNode(x, childCodeId);

  if (parentOnly) {
    res <- node$parent;
  } else {
    res <- node$path;
  }

  if (!includeChildCode) {
    res <- res[1:(length(res)-1)];
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
