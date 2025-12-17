get_attributes_for_classInstance <- function(x, classId, instanceId) {

  res <-
    x$convenience$attributesPerClass[[classId]][
      x$convenience$attributesPerClass[[classId]][, classId] == instanceId,
    ];

  res <- as.list(res);

  return(res);

}
