#' Soft Non-numeric Occurrence Estimation (SNOE) plot
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include, or,
#' alternatively, a character vector with literal code identifiers.
#' @param estimateWithin The column specifying within what to count.
#' @param matchRegexAgainstPaths Whether to match the `codes` regular expression
#' against the full code paths or only against the code identifier.
#' @param ggplot2Theme Can be used to specify theme elements for the plot.
#' @param title Title of the plot
#' @param vertical Whether to plot horizontally (`FALSE`, best for reading code
#  identifiers) or vertically (`TRUE`, best for looking like snow).
#' @param barWidth The width of the bars; can be decreased or increased, for
#' example in combination with vertical plotting to make the plot look more like
#' a snowy landscape.
#' @param greyScale Whether to produce the plot in color (`FALSE`) or greyscale
#' (`TRUE`).
#' @param colors,greyScaleColors The (two) colors to use for the color and
#' greyscale versions of the SNOE plot.
#' @param silent Whether to be chatty or silent
#'
#' @return A list containing a [ggplot2::ggplot()] in the `$plot` slot.
#' @export
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-3.rock");
#'
#' ### Load example source
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show code occurrence estimates
#' rock::snoe_plot(
#'   loadedExample
#' );
#'
#' ### Load two example sources
#' loadedExamples <- rock::parse_sources(
#'   examplePath,
#'   regex = "example-[34].rock"
#' );
#'
#' rock::snoe_plot(
#'   loadedExamples
#' );
#'
#' ### And plotted vertically
#' rock::snoe_plot(
#'   loadedExamples,
#'   vertical = TRUE,
#'   barWidth = 1
#' );
snoe_plot <- function(x,
                      codes = ".*",
                      matchRegexAgainstPaths = FALSE,
                      accumulateChildren = TRUE,
                      estimateWithin = NULL,
                      title = "SNOE plot",
                      vertical = FALSE,
                      barWidth = .9,
                      ggplot2Theme = ggplot2::theme_minimal(),
                      greyScale = FALSE,
                      colors = c("#C0C0C0", "#0072B2"),
                      greyScaleColors = c("#C0C0C0", "#808080"),
                      silent = rock::opts$get("silent")) {

  if ((!inherits(x, "rock_parsedSources")) && (!inherits(x, "rock_parsedSource"))) {

    stop("As `x`, you have to pass an object with one or more parsed sources, ",
         "as produced by a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. This object should have class ",
         "`rock_parsedSource` or `rock_parsedSources`, but the object ",
         "you passed has class(es) ", vecTxtQ(class(x)), ".");

  }

  ### Edit 2025-09-18 --- only considering 'codes' regex is length==1
  # if (length(codes) > 1) {
  #   codes <- paste(codes, collapse="|");
  # }

  if (matchRegexAgainstPaths && (length(codes) == 1)) {

    msg(
      "`matchRegexAgainstPaths` is set to TRUE, and a single value has been ",
      "passed as `codes`, so I'm considering it a regular expression to ",
      "match against the coding paths.",
      silent = silent
    );

    codesToInclude <-
      names(x$convenience$codingPaths)[
        grepl(
          codes,
          x$convenience$codingPaths,
          perl = TRUE
        )
      ];

  } else if (length(codes) == 1) {

    msg(
      "`matchRegexAgainstPaths` is set to FALSE, and a single value has been ",
      "passed as `codes`, so I'm considering it a regular expression to ",
      "match against the code identifiers.",
      silent = silent
    );

    codesToInclude <-
      x$convenience$codingLeaves[
        grepl(
          codes,
          names(x$convenience$codingPaths),
          perl = TRUE
        )
      ];

  } else {

    codesToInclude <- codes;

  }

  msg(
    "Identified ", length(codesToInclude), " codes, specifically: ",
    vecTxtQ(sort(codesToInclude)),
    silent = silent
  );

  ### Get coding scheme names
  codingSchemeNames <-
    unique(
      unname(
        unlist(
          lapply(
            x$convenience$rawCodings,
            names
          )
        )
      )
    );

  ### Remove coding tree roots (the coding scheme names)
  codesToInclude <-
    setdiff(
      codesToInclude,
      c(codingSchemeNames, NA)
    );

  ### Potentially accumulate over descendents

  if (accumulateChildren) {

    msg(
      "`accumulateChildren` is set to TRUE, so including child codes  ",
      "occurrences.",
      silent = silent
    );

    descendents <-
      lapply(
        codesToInclude,
        rock::get_descendentCodeIds,
        x = x
      );
    names(descendents) <- codesToInclude;

    msg(
      "Identified the following descendents for each code (in alphabetic order): ",
      silent = silent
    );

    for (codeId in codesToInclude) {

      msg(
        "\n  - For code '", codeId, "', I identified descendents ",
        vecTxtQ(descendents[[codeId]]),
        silent = silent
      );

    }

  }

  allCodes <- names(x$convenience$codingPaths);
  codedCodes <- intersect(names(x$qdt), allCodes);

  existentCodes <-
    codesToInclude[which(codesToInclude %in% codedCodes)];
  nonexistentCodes <-
    codesToInclude[which(!(codesToInclude %in% codedCodes))];

  if ((!accumulateChildren) && (length(nonexistentCodes) > 0)) {

    msg("\nNot all codes you specified exist in the qualitative data table!\n",
        "Specifically, I could not find the following codes:\n",
        vecTxtQ(nonexistentCodes), "\n\nRemoving them from the list for the SNOE plot.\n",
        silent = silent);

    warning("Not all codes you specified exist in the qualitative data table!\n",
            "Specifically, I could not find the following codes:\n",
            vecTxtQ(nonexistentCodes), "\n\nRemoving them from the list for the SNOE plot.\n");

    codesToInclude <- existentCodes;

  }

  if (inherits(x, "rock_parsedSource") || inherits(x, "rock_parsedSources")) {

    if (accumulateChildren) {

      ### Processing code xxx and it's descendents. Counts:

      counts_total <-
        lapply(
          names(descendents),
          function(currentCode) {

            actuallyAppliedCodes <-
              sort(
                intersect(
                  c(currentCode, descendents[[currentCode]]),
                  codedCodes
                )
              );

            if ("uids" %in% names(x$qdt)) {

              UIDs <-
                lapply(
                  actuallyAppliedCodes,
                  function(currentCodeId) {
                    return(
                      x$qdt$uids[as.logical(x$qdt[[currentCodeId]])]
                    );
                  }
                );

            } else {

              UIDs <- NULL;

            }

            res <- colSums(
              x$qdt[, actuallyAppliedCodes, drop = FALSE]
            );
            res <-  res[order(names(res))];

            if (is.null(UIDs)) {
              UIDs_bit <- "";
            } else {
              UIDs <- UIDs[order(names(res))];
              UIDs_bit <-
                unlist(
                  lapply(
                    seq_along(UIDs),
                    function(i) {
                      paste0(
                        " [",
                        vecTxt(UIDs[[i]]),
                        "]"
                      );
                    }
                  )
                );
            }

            msg("\n\nFor code identifier '", currentCode,
                "' and its descendents (total occurrences: ", sum(res),
                "), the following codes were applied, ",
                "with the following occurrences counted for each:",
                vecTxt(paste0("\n  - '", names(res), "' (", res, ")", UIDs_bit)),
                silent = silent);

            res <- sum(res);

            return(res);

          }
        );
     names(counts_total) <- names(descendents);

    } else {

      counts_total <-
        apply(
          x$qdt[, codesToInclude],
          2,
          sum,
          simplify = FALSE
        );

    }

    counts_total <- unlist(counts_total);

    totalUtterances <- nrow(x$qdt);

    totalCodings <- sum(x$qdt[, x$convenience$codingLeaves]);

    totalCodedUtterances <-
      sum(
        as.numeric(
          apply(
            x$qdt[, x$convenience$codingLeaves],
            1,
            function(row) {
              return(any(as.logical(row)));
            }
          )
        )
      );

    proportions_totalCodedUtterances <-
      counts_total / totalCodedUtterances;

    CIs_totalCodedUtterances_objects <-
      lapply(
        counts_total,
        rock::confIntProp,
        n = totalCodedUtterances
      );

    CIs_totalCodedUtterances_df <-
      rock::rbind_df_list(
        lapply(
          CIs_totalCodedUtterances_objects,
          as.data.frame
        )
      );

    CIs_totalCodedUtterances_df$codeId <- codesToInclude;
    CIs_totalCodedUtterances_df$prop <- proportions_totalCodedUtterances;
    CIs_totalCodedUtterances_df$count <- counts_total;
    CIs_totalCodedUtterances_df$totalCodedUtterances <- totalCodedUtterances;

    row.names(CIs_totalCodedUtterances_df) <- codesToInclude;

    minProp <- min(c(CIs_totalCodedUtterances_df$ci.lo, CIs_totalCodedUtterances_df$ci.hi));
    maxProp <- max(c(CIs_totalCodedUtterances_df$ci.lo, CIs_totalCodedUtterances_df$ci.hi));

    gradients <-
      lapply(
        codesToInclude,
        function(codeId) {
          return(
            grid::linearGradient(
              colours = colors,
              x1 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.lo"], "npc"),
              y1 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.lo"], "npc"),
              x2 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.hi"], "npc"),
              y2 = grid::unit(CIs_totalCodedUtterances_df[codeId, "ci.hi"], "npc"),
              extend = "pad"
            )
          )
        }
      );
    names(gradients) <- codesToInclude;


  } else {
    stop("As `x`, you have to pass one or more parsed sources, as ",
         "produced by a call to rock::parse_source() or rock::parse_sources(). ",
         "However, the object you passed has class ", rock::vecTxtQ(class(x)), ".");
  }

  palette_functionFactory <-
    function(lb, ub) {
      return(
        function(x) {
          return(
            ifelse(
              x < lb,
              0,
              ifelse (
                x > ub,
                1,
                (x - lb) / (ub - lb)
              )
            )
          )
        }
      )
    }

  ### It does not seem possible to use this as only one fill scale can exist
  ### for a plot; butkeeping it here anyway in case I ever need it again.
  ###
  # palette_transformerFactory <-
  #   function(lb, ub) {
  #     return(
  #       scales::new_transform(
  #         name = "snoe",
  #         transform = palette_functionFactory(lb, ub),
  #         inverse = function(x) {return(x)},
  #         domain = c(0, 1)
  #       )
  #     )
  #   }

  nCodesToInclude <- length(codesToInclude);

  resolution <- 10000;

  estimationScale <-
    seq(
      0,
      maxProp,
      length = resolution
    );

  combinedDf <-
    rock::rbind_df_list(
      lapply(
        codesToInclude,
        function(codeId) {
          res <-
            data.frame(
              codeId = rep(codeId, each = resolution),
              occurrence = estimationScale
            );
          res$estimation <-
            palette_functionFactory(
              lb = CIs_totalCodedUtterances_df[codeId, 'ci.lo', drop=TRUE],
              ub = CIs_totalCodedUtterances_df[codeId, 'ci.hi', drop=TRUE]
            )(res$occurrence);
          return(res);
        }
      )
    );

  combinedDf$codeId <-
    factor(
      combinedDf$codeId,
      levels = sort(unique(codesToInclude), decreasing = TRUE),
      labels = sort(unique(codesToInclude), decreasing = TRUE),
      ordered = TRUE
    );

  res <- list(
    occurrenceEstimates = CIs_totalCodedUtterances_df
  );

  res$plot <-
    ggplot2::ggplot(
      data = combinedDf,
      mapping = ggplot2::aes_string(
        x = 'occurrence',
        y = 'codeId',
        fill = 'estimation'
      )
    ) +
    ggplot2::geom_col(width=barWidth);

  if (greyScale) {
    res$plot <-
      res$plot +
        ggplot2::scale_fill_gradient(
          low = greyScaleColors[1],
          high = greyScaleColors[2],
          guide = NULL
        ) +
      ggplot2::scale_color_gradient(
        low = greyScaleColors[1],
        high = greyScaleColors[2],
        guide = NULL
      );
  } else {
    res$plot <-
      res$plot +
      ggplot2::scale_fill_gradient(
        low = colors[1],
        high = colors[2],
        guide = NULL
      ) +
      ggplot2::scale_color_gradient(
        low = colors[1],
        high = colors[2],
        guide = NULL
      );

  }

  res$plot <-
    res$plot +
    ggplot2::labs(
      x = "Occurrence estimation",
      y = NULL,
      title = title
    ) +
    ggplot2Theme;

  if (vertical) {
    res$plot <-
      res$plot +
      ggplot2::coord_flip() +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      );
  } else {
    res$plot <-
      res$plot +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      );
  }

  class(res) <- c("rock_snoe_plot", "rock");

  return(res);

    # lapply(
    #   codesToInclude,
    #   function(codeId) {
    #     return(
    #       ggplot2::geom_col(
    #         data = CIs_totalCodedUtterances_df[codeId, , drop=FALSE],
    #         mapping = ggplot2::aes(
    #           x = codeId,
    #           y = prop
    #         )
    #       ) +
    #         ggplot2::scale_fill_gradient(
    #           name = "occurrence estimation",
    #           low = colors[1],
    #           high = colors[2],
    #           transform =
    #             palette_transformerFactory(
    #               lb = CIs_totalCodedUtterances_df[codeId, 'ci.lo', drop=TRUE],
    #               ub = CIs_totalCodedUtterances_df[codeId, 'ci.hi', drop=TRUE]
    #             )
    #         )
    #     )
    #   }
    # ) +
    # ggplot2::theme_minimal();

}

#' @export
print.rock_snoe_plot <- function(x, ...) {
  print(x$plot);
  return(invisible(x));
}
