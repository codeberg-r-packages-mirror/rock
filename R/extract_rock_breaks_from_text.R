#' @rdname extract_rock_elements
#' @export
#' @examples ### Extract section breaks
#' sectionBreaks <-
#'   rock::extract_rock_breaks_from_text(
#'     loadedExample
#'   );
#'
extract_rock_breaks_from_text <- function(x) {

  sectionRegexes <- rock::opts$get('sectionRegexes');

  sectionBreakTypes <- names(sectionRegexes);

  res <- list(sectionBreaks = list());

  for (currentSectionBreakType in sectionBreakTypes) {

    res$sectionBreaks[[currentSectionBreakType]]$lines <-
      grepl(
        sectionRegexes[currentSectionBreakType],
        x,
        perl = TRUE
      );

    res$sectionBreaks[[currentSectionBreakType]]$ids <-
      ifelse(
        res$sectionBreaks[[currentSectionBreakType]]$lines,
        sub(
          paste0(".*", sectionRegexes[currentSectionBreakType], ".*"),
          "\\1",
          x[res$sectionBreaks[[currentSectionBreakType]]$lines],
          perl = TRUE
        ),
        ""
      );

  }

  if (length(sectionBreakTypes) == 1) {

    res$allSectionBreak_lines <-
      which(res$sectionBreaks[[sectionBreakTypes]]$lines);

  } else {

    res$allSectionBreak_lines <-
      sort(
        unique(
          unlist(
            lapply(
              res$sectionBreaks,
              function(currentSectionBreakObject) {
                return(which(currentSectionBreakObject$lines));
              }
            )
          )
        )
      );

  }

  if (length(sectionBreakTypes) == 1) {

    res$allSectionBreak_ids <-
      res$sectionBreaks[[sectionBreakTypes]]$ids;

  } else {

    res$allSectionBreak_ids <-
      do.call(
        paste,
        c(
          lapply(
            res$sectionBreaks,
            function(currentSectionBreakObject) {
              return(currentSectionBreakObject$ids);
            }
          ),
          list(sep = "---",
               collapse = " ")
        )
      );

  }

  res$text <- x;
  res$text_clean <-
    remove_rock_breaks_from_text(x);

  return(res);

}
