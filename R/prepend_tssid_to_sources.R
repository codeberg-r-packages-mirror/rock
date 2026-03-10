#' Prepend a line with a TSSID to a source
#'
#' This function adds a line with a TSSID (a time-stamped source identifier)
#' to the beginning of a source that was read with one of
#' the `loading_sources` functions. When combined with UIDs, TSSIDs are
#' virtually unique references to a specific data fragment.
#'
#' TSSIDs are a date and time in the UTC timezone, consisting of eight digits
#' (four for the year, two for the month, and two for the day), a `T`, four
#' digits (two for the hour and two for the minute), and a `Z` (to designate
#' that the time is specified in the UTC timezone). TSSIDs are valid ISO8601
#' standard date/times. TSSIDs match the regular expression `[0-9]{8}T[0-9]{4}Z`.
#'
#' @param input The source, as produced by one of the `loading_sources`
#' functions, or a path to an existing file that is then imported.
#' @param tssids A data frame or path to a CSV file with
#' columns `filename_regex` and `tssid` or `moment`. The `tssid` column
#' contains TSSIDs, and the `moment` column contains timestamps in the form
#' `2025-05-28 11:30 CEST` (so, `YYYY-MM-DD HH-MM TZ`).
#' @param output If specified, the coded source will be written here.
#' @param designationSymbol The symbol to use to designate an instance
#' identifier for a class (can be "`=`" or "`:`" as per the ROCK standard).
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param encoding The encoding to use.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the coded source object.
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' loadedExample <-
#'   rock::load_source(exampleFile);
#'
#' ### Add a coder identifier
#' loadedExample <-
#'   rock::prepend_tssid_to_source(
#'     loadedExample,
#'     moment = "2025-05-28 11:30 CEST"
#'   );
#'
#' ### Show the first line
#' cat(loadedExample[1]);
#'
#' @export
prepend_tssid_to_sources <- function(input,
                                     tssids,
                                     output = NULL,
                                     outputPrefix = "",
                                     outputSuffix = "_withTSSIDs",
                                     designationSymbol = "=",
                                     filenameRegex = "\\.rock",
                                     recursive = TRUE,
                                     preventOverwriting = rock::opts$get('preventOverwriting'),
                                     rlWarn = rock::opts$get(rlWarn),
                                     encoding = rock::opts$get('encoding'),
                                     silent = rock::opts$get('silent')) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!is.character(output) || !length(output)==1) {
    stop("Only specify a single string as 'output'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (tolower(output) == "same") {
    if ((is.null(outputPrefix) || (nchar(outputPrefix) == 0)) &&
        (is.null(outputSuffix) || (nchar(outputSuffix) == 0))) {
      stop("If writing the output to the same directory, you must specify ",
           "an outputPrefix and/or an outputSuffix!");
    }
  } else {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  if (input == output) {
    if (any(grepl("\\.rock$",
                  rawSourceFiles))) {
      if (isTRUE(nchar(outputPrefix) == 0) && isTRUE(nchar(outputSuffix) == 0)) {
        stop("At least one of the input files already has the .rock extension! ",
             "Therefore, you have to provide at least one of `outputPrefix` and `outputSuffix` ",
             "to allow saving the files to new names!");
      }
    }
  }

  if (is.data.frame(tssids)) {
    tssidVector <-
      stats::setNames(
        tssids$tssid,
        nm = tssids$filename_regex
      );
  } else if ((is.list(tssids) && (length(tssids) == length(rawSourceFiles))) ||
             (is.character(tssids) && (length(tssids) == length(rawSourceFiles)))) {
    tssidVector <-
      stats::setNames(
        unlist(tssids),
        nm = names(tssids)
      );
  } else if (file.exists(tssids)) {
    tssidDf <- read.csv(tssids);

    if ("tssid" %in% names(tssidDf)) {
      tssidVector <-
        stats::setNames(
          tssidDf$tssid,
          nm = tssidDf$filename_regex
        );
    } else if ("moment" %in% names(tssidDf)) {
      tssidVector <-
        stats::setNames(
          rock::generate_tssid(tssidDf$moment),
          nm = tssidDf$filename_regex
        );
    } else {
      stop("The file you provide as `tssids` must either have a column ",
           "named `tssid` or a column named `moment`.");
    }

  } else {
    stop(
      "As `tssids`, pass either a dataframe with columns `filename_regex` and `tssid`, ",
      "the path to a .csv file with that same structure, or a named character vector or ",
      "list, where the values are the TSSIDs and the names are regular expressions to match ",
      "against the filenames."
    );
  }

  res <- character();
  for (filename in rawSourceFiles) {

    newFilename <-
      paste0(outputPrefix,
             sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(filename)),
             outputSuffix,
             ".rock");
    if (tolower(output) == "same") {
      newFileDir <-
        dirname(filename);
    } else {
      newFileDir <-
        output;
    }

    current_tssid <-
      tssidVector[
        which_regex_matches(
          pattern = names(tssidVector),
          filename
        )
      ][1];

    prepend_tssid_to_source(
      input = filename,
      moment = current_tssid,
      output = file.path(newFileDir,
                         newFilename),
      designationSymbol = designationSymbol,
      preventOverwriting = preventOverwriting,
      rlWarn = rlWarn,
      encoding = encoding,
      silent = silent
    );

    res <-
      c(res,
        newFilename);
  }
  if (!silent) {
    message("I just wrote ", length(rawSourceFiles), " cleaned sources to path '",
            output,
            "' ",
            ifelse(preventOverwriting,
                   "(unless the files already existed)",
                   "(overwriting any files that may already have existed)"),
            ". Note that these files may all be overwritten if this ",
            "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
            "Therefore, make sure to copy them to ",
            "another directory before starting to code those sources!\n\n",
            "A recommended convention is to place all data in a directory ",
            "called 'data', and use three subdirectories: 'raw-sources' for ",
            "the raw sources; 'clean-sources' for the cleaned sources (which ",
            "should then be the `output` specified to this `clean_sources` ",
            "function), and 'coded-sources' for the coded sources. If you have ",
            "multiple coders, use e.g. 'coded-sources-coder-A' and ",
            "'coded-sources-coder-B' to organise these versions.");
  }
  invisible(res);
}
