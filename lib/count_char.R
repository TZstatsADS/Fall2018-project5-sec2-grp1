count_char <- function(vec){
  string <- paste(vec, collapse = " ")
  chars <- strsplit(string, "")[[1]]
  chars_useful <- chars[grepl("[a-zA-Z]", chars)]
  chars_useful <- chars_useful[chars_useful != "\f"]
  chars_useful <- factor(chars_useful, levels = c(letters, LETTERS))
  count_table <- table(chars_useful)
  cnts <- as.vector(count_table)
  names(cnts) <- c(letters, LETTERS)
  return(cnts)
}