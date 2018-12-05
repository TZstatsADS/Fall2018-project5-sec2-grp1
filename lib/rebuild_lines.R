rebuild_lines <- function(vec, line_lens){
  lst <- list()
  v <- vec
  nlines <- length(line_lens)
  for(j in 1:nlines){
    current_line_vec <- v[1:line_lens[j]] # get the content for the current line
    v <- v[(line_lens[j]+1):length(v)] # delete the content for the current line
    lst <- list.append(lst, current_line_vec)
  }
  return(lst)
}