make_ROCK_codebook <- function(metadata = NULL,
                               codes = NULL,
                               examples = NULL,
                               aesthetics = NULL,
                               relationships = NULL,
                               output = NULL) {

  codebookDefaultMetadata <- rock::opts$get("codebookDefaultMetadata");

  metadata = rock::mix_and_match_lists(metadata, codebookDefaultMetadata);

  ### Check codes
  for (i in codes) {
    if (!("code_id" %in% codes[[i]])) {
      stop("Code number ", i, " does not have an identifier specified!");
    }
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
