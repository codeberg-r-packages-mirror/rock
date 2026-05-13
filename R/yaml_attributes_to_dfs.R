#' Convert attributes from one or more YAML files to a list of data frames
#'
#' @param x A character vector with one or more file paths
#'
#' @returns A list of data frames
#' @export
#'
#' @examples
yaml_attributes_to_dfs <- function(x) {

  ### To do: allow import of .rock files, too

  ### For now, only accept YAML files

  x <- grep("\\.ya?ml$", x, value=TRUE, ignore.case = TRUE);

  yml <-
    lapply(
      x,
      function(i) {
        if (!file.exists(i)) {
          warning("File '", i, "' does not exist! Skipping it.");
          return(NA);
        } else {
          res <- yaml::read_yaml(i);
          if (!("ROCK_attributes" %in% names(res))) {
            warning("File '", i, "' does not contain a YAML node named 'ROCK_attributes'! Skipping it.");
            return(NA);
          } else {
            return(res$ROCK_attributes);
          }
        }
      }
    );
  yml <- yml[!is.na(yml)];

  dfs <-
    lapply(
      yml,
      function(currentYamlObject) {
        return(
          rbind_df_list(
            lapply(
              currentYamlObject,
              as.data.frame
            )
          )
        );
      }
    );
  names(dfs) <-
    unlist(lapply(dfs, \(x) names(x)[1]));

  return(dfs);

}
