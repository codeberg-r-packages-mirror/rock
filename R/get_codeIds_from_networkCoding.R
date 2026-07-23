get_codeIds_from_networkCoding <- function(x) {

  codeDelimiters <- rock::opts$get('codeDelimiters');

  codeRegexes <- rock::opts$get('codeRegexes');
  idRegexes <- rock::opts$get('idRegexes');
  classInstanceRegex <- rock::opts$get('classInstanceRegex');
  codeValueRegexes <- rock::opts$get('codeValueRegexes');
  sectionRegexes <- rock::opts$get('sectionRegexes');
  uidRegex <- rock::opts$get('uidRegex');

  inductiveCodingHierarchyMarker <- rock::opts$get('inductiveCodingHierarchyMarker');
  codeTreeMarker <- rock::opts$get('codeTreeMarker');

  networkCodeRegexes <- rock::opts$get('networkCodeRegexes');
  networkCodeRegexOrder <- rock::opts$get('networkCodeRegexOrder');
  networkEdgeWeights <- rock::opts$get('networkEdgeWeights');
  networkDefaultEdgeWeight <- rock::opts$get('networkDefaultEdgeWeight');
  networkCodeCleaningRegexes <- rock::opts$get('networkCodeCleaningRegexes');
  networkCollapseEdges <- rock::opts$get('networkCollapseEdges');

  codeIds_by_networkCodeName <-
    lapply(
      networkCodeRegexes,
      function(networkCodeRegex) {
        return(
          lapply(
            x,
            function(coding) {
              elements <-
                c(gsub(networkCodeRegex, "\\1", coding, perl=TRUE),
                  gsub(networkCodeRegex, "\\2", coding, perl=TRUE),
                  gsub(networkCodeRegex, "\\3", coding, perl=TRUE),
                  gsub(networkCodeRegex, "\\4", coding, perl=TRUE)
                );
              element_from <- elements[which(networkCodeRegexOrder == "from")];
              element_to <- elements[which(networkCodeRegexOrder == "to")];
              element_type <- elements[which(networkCodeRegexOrder == "type")];
              element_weight <- elements[which(networkCodeRegexOrder == "weight")];
              return(
                list(
                  from = element_from,
                  to = element_to,
                  type = element_type,
                  weight = element_weight
                )
              );
            }
          )
        );
      }
    );
  names(codeIds_by_networkCodeName) <- names(networkCodeRegexes);

  codeIds_by_networkCodeName_sorted <-
    lapply(
      codeIds_by_networkCodeName,
      function(codeIds) {
        return(
          list(
            from = unlist(lapply(codeIds, \(x) return(x$from))),
            to = unlist(lapply(codeIds, \(x) return(x$to))),
            type = unlist(lapply(codeIds, \(x) return(x$type))),
            weight = unlist(lapply(codeIds, \(x) return(x$weight)))
          )
        );
      }
    );

  codeIds_by_networkCodeName_sorted_unique <-
    lapply(
      codeIds_by_networkCodeName_sorted,
      function(codeIds) {
        return(
          list(
            from = unique(codeIds$from),
            to = unique(codeIds$to),
            type = unique(codeIds$type),
            weight = unique(codeIds$weight)
          )
        );
      }
    );

  codeIds_all_sorted_unique <-
    lapply(
      c("from", "to", "type", "weight"),
      function(type) {
        return(
          unlist(
            lapply(
              codeIds_by_networkCodeName_sorted_unique,
              function(x) {
                return(x[[type]]);
              }
            ),
            use.names = FALSE
          )
        );
      }
    );
  names(codeIds_all_sorted_unique) <- c("from", "to", "type", "weight");

  res <-
    list(
      codeIds_by_networkCodeName = codeIds_by_networkCodeName,
      codeIds_by_networkCodeName_sorted = codeIds_by_networkCodeName_sorted,
      codeIds_by_networkCodeName_sorted_unique = codeIds_by_networkCodeName_sorted_unique,
      codeIds_all_sorted_unique = codeIds_all_sorted_unique
    );

  return(res);

}
