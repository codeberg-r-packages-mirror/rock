### Easily update everything
updateEverything <- FALSE;

#' An very rudimentary example codebook specification
#'
#' This is a simple and relatively short codebook
#' specification, both in spreadsheet form (`exampleCodebook_1`) and
#' in YAML form (`yamlCodebook_1`).
#'
#' @format An example of a codebook specification
#'
"exampleCodebook_1"
"yamlCodebook_1"

### Inclusive General-Purpose Registration Form
if (exists("updateEverything") && updateEverything) {

  gSheet_url <-
    "https://docs.google.com/spreadsheets/d/1gVx5uhYzqcTH6Jq7AYmsLvHSBaYaT-23c7ZhZF4jmps";

  localBackupFile <-
    here::here(
      "inst", "extdata", "exampleCodebook_1.xlsx"
    );

  exampleCodebook_1 <-
    rock::codebook_fromSpreadsheet(
      gSheet_url,
      localBackup = localBackupFile,
      silent=FALSE
    );

  usethis::use_data(exampleCodebook_1, overwrite=TRUE);

  ### Convert to YAML

  yamlFile <-
    here::here(
      "inst", "extdata", "exampleCodebook_1.yml"
    );

  yamlCodebook_1 <-
    rock::codebook_to_yaml(
      exampleCodebook_1
    );

  usethis::use_data(yamlCodebook_1, overwrite=TRUE);

}
