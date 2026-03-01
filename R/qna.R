#' Parse a source and show its QNA network
#'
#' Applies [rock::parse_source()] to a source, extract the produced network,
#' and shows it.
#'
#' @param x A loaded source (with [rock::load_source()]), the path to a
#' loaded source, or an already parsed source (with [rock::parse_source()]).
#'
#' @return A list with all produced graph(s) DiagrammeR objects and as Dot code.
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Read a souce coded with the Qualitative Network Approach
#' qnaExample <-
#'   file.path(
#'     examplePath,
#'     "network-example-1.rock"
#'   );
#'
#' ### Show the network
#' rock::qna(qnaExample);
#'
#' @export
qna <- function(x) {

  if (inherits(x, "rock_source")) {

    x <- rock::parse_source(x);

  } else if (is.character(x) && (length(x) == 1) &&
             file.exists(x)) {

    x <- rock::parse_source(x);

  } else if (!inherits(x, "rock_parsedSource")) {

    stop("As `x`, provide a loaded source (with `rock::load_source()`, the ",
         "path to a loaded source, or an already parsed source ",
         " (with `rock::parse_source()`). You provided an object of class(es) ",
         vecTxtQ(class(x)), ".");

  }

  if (!('network' %in% names(x$networkCodes))) {

    stop("No network codes found in the provided source!");

  }

  res <- lapply(
    x$networkCodes,
    function(currentNetworkCode) {

      if (is.null(currentNetworkCode)) {
        return(NULL);
      } else if (is.null(currentNetworkCode$dot) ||
                 (is.null(currentNetworkCode$graph))) {
        return(NULL);
      } else {

        res <-
          list(graph = currentNetworkCode$graph,
               dot = currentNetworkCode$dot);

        return(res);

      }

    }
  );

  if (length(res) == 1) {
    res <- res[[1]];
    print(DiagrammeR::render_graph(
      res$graph
    ));
  } else if (length(res) == 0) {
    res <- NULL;
  } else {
    names(res) <- names(x$networkCodes);
  }

  return(invisible(res));

}
