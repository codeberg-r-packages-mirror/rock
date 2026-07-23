#' Make a ROCK codebook
#'
#' With this function, you can create an empty ROCK codebook, populated
#' with the code identifiers you pass as `codes`. You can also pass data frames,
#' in the format of the spreadsheet ROCK codebook standard, to produce a ROCK
#' codebook object.
#'
#' @param metadata The metadata, such as title, authors, and positionaly
#' statements, as a named list, where the names have to be the names as
#' defined in the ROCK standard.
#' @param codes The codes; either a character vector with code identifiers or a
#' data frame
#' @param examples,aesthetics,relationships Data frames with examples,
#' aesthetics, and relationships, as defined in the ROCK standard.
#'
#' @returns
#' @export
#'
#' @examples
make_ROCK_codebook <- function(metadata = NULL,
                               codes = NULL,
                               examples = NULL,
                               aesthetics = NULL,
                               relationships = NULL) {

  codebookDefaultMetadata <- rock::opts$get("codebookDefaultMetadata");

  metadataVector = unlist(rock::mix_and_match_lists(metadata, codebookDefaultMetadata));

  metadata <- data.frame(field = names(metadataVector),
                         content = unname(metadataVector));

  ### Check codes
  if (is.data.frame(codes)) {
    for (i in codes) {
      if (!("code_id" %in% codes[[i]])) {
        stop("Code number ", i, " does not have an identifier specified!");
      }
    }
  }

  if (is.null(codes)) {
    codes <- "exampleCodeId"
  }

  if (is.character(codes)) {
    codes <-
      data.frame(
        code_label = rep("", length(codes)),
        code_instruction = rep("", length(codes)),
        code_id = codes,
        ucr = rep("", length(codes)),
        ucr_prefix = rep("", length(codes)),
        ucr_url = rep("", length(codes)),
        ucid_url = rep("", length(codes)),
        fillcolor = rep("", length(codes)),
        color = rep("", length(codes)),
        shape = rep("", length(codes))
      );
  }

  if (is.null(examples)) {
    examples <-
      data.frame(
        code_id = codes$code_id,
        example_fragment = rep("An example of a data fragment", length(codes$code_id)),
        obviousness = rep("'core' or 'edge'", length(codes$code_id)),
        match = rep("'match' or 'mismatch'", length(codes$code_id)),
        explanation = rep("An explanation of why this is a core/edge (mis)match", length(codes$code_id))
      );
  }

  if (is.null(aesthetics)) {
    aesthetics <-
      data.frame(
        target = c("graph", "node", "edge"),
        type = c("default", "default", "default"),
        attribute = c("rankdir", "shape", "color"),
        value = c("LR", "oval", "black")
      );
  }

  if (is.null(relationships)) {
    relationships <-
      data.frame(
        from_code_id = "parent_code_id",
        to_code_id = "child_code_id",
        relationship_type = "parent_to_child"
      );
  }

  res <- list(
    metadata = metadata,
    codes = codes,
    examples = examples,
    aesthetics = aesthetics,
    relationships = relationships
  );

  return(res);

}
