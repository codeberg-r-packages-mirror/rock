re_encode_textfile <- function(x,
                               from = "ISO-8859-1",
                               to = "UTF-8",
                               outputSuffix) {

  if (length(x) > 1) {
    stop(
      "For multiple textfiles, use `rock::re_encode_textfiles()`"
    );
  }

  res <-
    readLines(

    );


  res <-
    iconv(
      dataVector,
      from = from,
      to = to
    );


}
