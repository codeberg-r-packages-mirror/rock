#' Title
#'
#' @param x
#' @param classId_segmentation
#' @param topics
#' @param ciid_selection
#'
#' @returns
#' @export
#'
#' @examples
mata_determineTopics <- function(x,
                                 classId_segmentation,
                                 topics = 5:10,
                                 parallel = FALSE,
                                 ciid_selection = NULL,
                                 max.em.its = 1000,
                                 heldout.seed = 101010,
                                 words_to_remove = NULL,
                                 min_word_nchar = 2,
                                 silent = rock::opts$get('silent')) {

  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop(
      "To use this function, you need to have the {quanteda} package installed.",
      "to install it, you can use:\n\n",
      "  install.packages('quanteda');\n\n"
    );
  }

  if (!requireNamespace("stm", quietly = TRUE)) {
    stop(
      "To use this function, you need to have the {stm} package installed.",
      "to install it, you can use:\n\n",
      "  install.packages('stm');\n\n"
    );
  }

  xName <- substitute(deparse(x));

  if (!all(classId_segmentation %in% names(x$qdt))) {
    stop("Of the class identifiers to be used for segmentation into what in ",
         "MATA terms is know as documents, which should be specified in ",
         "argument `classId_segmentation`, one or more did not occur ",
         "in the qualitative data table present in the input object (you ",
         "passed object '", xName, "'). As class identifiers, you passed ",
         vecTxtQ(classId_segmentation), ".");
  }

  if (!is.null(ciid_selection)) {

    if (!all(names(ciid_selection) %in% names(x$qdt))) {
      stop(
        "Not all class identifiers you want to select on exist in the ",
        "Qualitative Data Table! You specified the following class ",
        "identifiers: ", vecTxtQ(names(ciid_selection)), "."
      );
    }

    for (i in seq_along(ciid_selection)) {

      currentCIID <- ciid_selection[i];
      currentCID <- names(ciid_selection)[i];

      msgTxt <-
        paste0(
          "\nSelecting data based on class identifier ", currentCID,
          " (has to match regex '", currentCIID,
          "'); QDT has ", nrow(x$qdt), " rows before selection, and "
        );

      x$qdt <-
        x$qdt[
          grepl(currentCIID, x$qdt[[currentCID]]),
        ];

      msg(
        msgTxt,
        nrow(x$qdt),
        " rows after selection.",
        silent = silent
      );

    }

  }

  ### Use split which handles the interactions
  splitIndices_full <-
    split(
      1:nrow(x$qdt),
      lapply(classId_segmentation, \(cid) return(x$qdt[[cid]]))
    );

  ### Remove combinations without data

  splitIndices_full_hasData <-
    unlist(lapply(splitIndices_full, length)) > 0;

  splitIndices <-
    splitIndices_full[splitIndices_full_hasData];

  ### Get list of QDT segments
  segmentedQDT_list <-
    lapply(
      seq_along(splitIndices),
      function(i) {
        res <-
          x$qdt[splitIndices[[i]], ];
        res$doc_id <- names(splitIndices)[i];
        return(res);
      }
    );

  segmentedQDT <-
    do.call(
      rbind,
      segmentedQDT_list
    );

  # ### Get a vector of each segment's data only
  # dataOnly <-
  #   unlist(
  #     lapply(
  #       segmentedQDT,
  #       function(x) {
  #         return(
  #           paste(
  #             x$utterances_clean,
  #             collapse="\n"
  #           )
  #         );
  #       }
  #     )
  #   );

  ### Import text as a corpus for further processing
  quantedaCorpus <-
    quanteda::corpus(
      x = segmentedQDT,
      docid_field = "doc_id",
      text_field = "utterances_clean",
      unique_docnames = FALSE
    );

  ### Produce tokens object
  quantedaTokens_raw <-
    quanteda::tokens(
      quantedaCorpus,
      what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE
    );

  ### Clean up tokens

  quantedaTokens <-
    quanteda::tokens_tolower(quantedaTokens_raw);

  if (is.null(words_to_remove)) {
    words_to_remove <- quanteda::stopwords("en");
  }

  quantedaTokens <-
    quanteda::tokens_remove(
      quantedaTokens,
      words_to_remove
    );

  quantedaTokens <-
    quanteda::tokens_keep(
      quantedaTokens,
      min_nchar = min_word_nchar
    );

  ### Construct a document-feature matrix
  quantedaDFM <-
    quanteda::dfm(
      quantedaTokens
    );

  ### Convert to a format that can be used by the {stm} package
  dfm_for_stm <-
    quanteda::convert(
      quantedaDFM,
      "stm"
    );

  if (parallel) {

    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("If you want to use parallel processing, ",
           "you need to have the parallel package, which ",
           "*should* normally be part of base R.");
    }

    ### Detect number of cores and create a cluster
    nCores <- parallel::detectCores();

    ### Because the trick below doesn't seem to work
    maxCores <- metabefor::opts$get("maxCores");
    if (!is.null(maxCores) && is.numeric(maxCores)) {
      nCores <- min(maxCores, nCores);
    }

    ### From https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      nCores <- min(2L, nCores);
    }

    msg("\nI will use ", nCores,
        " processor cores.", silent = silent);

  } else {

    nCores <- 1;

    msg("\nI will not use multiple processor cores.",
        silent = silent);

  }

  if (nCores == 1) {

    kResults <-
      lapply(
        topics,
        function(K) {
          return(
            stm::searchK(
              documents = dfm_for_stm$documents,
              vocab = dfm_for_stm$vocab,
              K = K,
              #    data = dfm_for_stm$meta,
              max.em.its = max.em.its,
              heldout.seed = heldout.seed
            )
          );
        }
      );

  } else {

    ### Multi core approach

    cl <- parallel::makeCluster(nCores);

    ### Load the metabefor package in each cluster
    parallel::clusterEvalQ(
      cl,
      library(stm)
    );

    ### Perform the parallel computations
    kResults <-
      parallel::parLapplyLB(
        cl,
        topics,
        stm::searchK,
        documents = dfm_for_stm$documents,
        vocab = dfm_for_stm$vocab,
        max.em.its = max.em.its,
        heldout.seed = heldout.seed
      );

    ### Stop the cluster
    parallel::stopCluster(cl);

  }


  res <- list(segmentedQDT = segmentedQDT,
              documents = dfm_for_stm$documents,
              vocab = dfm_for_stm$vocab,
              kResults = kResults);

  datOfLists <-
    do.call(
      rbind,
      lapply(
        kResults,
        `[[`,
        "results"
      )
    );

  res$dat <-
    data.frame(
      lapply(
        datOfLists,
        unlist
      )
    );

  res$plots <- list();

  res$plots$heldout <-
    ggplot2::ggplot(
      data = res$dat,
      mapping = ggplot2::aes(x = K,
                             y = heldout)
    ) + ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Number of topics",
                  y = "Held-Out Likelihood",
                  title = "Held-Out Likelihood");

  res$plots$residual <-
    ggplot2::ggplot(
      data = res$dat,
      mapping = ggplot2::aes(x = K,
                             y = residual)
    ) + ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Number of topics",
                  y = "Residuals",
                  title = "Residuals");

  res$plots$semcoh <-
    ggplot2::ggplot(
      data = res$dat,
      mapping = ggplot2::aes(x = K,
                             y = semcoh)
    ) + ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Number of topics",
                  y = "Semantic Coherence",
                  title = "Semantic Coherence");

  res$plots$lbound <-
    ggplot2::ggplot(
      data = res$dat,
      mapping = ggplot2::aes(x = K,
                             y = lbound)
    ) + ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Number of topics",
                  y = "Lower Bound",
                  title = "Lower Bound");

  return(invisible(res));

}
