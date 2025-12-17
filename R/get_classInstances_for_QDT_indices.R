get_classInstances_for_QDT_indices <- function(x, indices, classIds) {

  existingClassIds <-
    classIds[classIds %in% names(x$qdt)];

  if (length(existingClasses) < classIds) {
    warning("Not all classes exist in the Qualitative Data Table in `x`! The ",
            "following classes do not exist (maybe you misspelled them?): ",
            vecTxtQ(classIds[!(classIds %in% names(x$qdt))]), ".");
  }

  instanceIds <-
    lapply(
      existingClassIds,
      function(currentClassId) {
        res <-
          unique(
            x$qdt[, currentClassId]
          );
        res <- res[!is.na(res)];
        res <- res[!(res == "no_id")];
        return(res);
      }
    );
  names(instanceIds) <- existingClassIds;

  return(instanceIds);

}
