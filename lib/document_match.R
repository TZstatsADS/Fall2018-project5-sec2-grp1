doc_match <- function(tesseract_doc, truth_doc){
  
  l_tess <- length(tesseract_doc)
  l_truth <- length(truth_doc)
  
  doc_clean_ind_list <- list()
  errors_list <- list()
  line_diff <- 0
  if(l_tess == l_truth){
    for (i in 1:l_tess){
      doc_clean_ind_list[[i]] <- tesseract_doc[[i]] %in% truth_doc[[i]]
      errors_list[[i]] <- tesseract_doc[[i]][!doc_clean_ind_list[[i]]]
    }
  }
  else{
    line_diff <- l_tess - l_truth
    for (i in 1:l_tess){
      doc_clean_ind_list[[i]] <- tesseract_doc[[i]] %in% unlist(truth_doc[i:(i+abs(line_diff))])
      errors_list[[i]] <- tesseract_doc[[i]][!doc_clean_ind_list[[i]]]
    }
  }
  return(list("doc_clean_ind_list" = doc_clean_ind_list, "line_diff" = line_diff, "errors_list" = errors_list))
}

char_match <- function(tesseract_doc, truth_doc){
  l_tess <- length(tesseract_doc)
  l_truth <- length(truth_doc)
  
  tesseract_one_vec <- paste(unlist(tesseract_doc), collapse = " ")
  truth_one_vec <- paste(unlist(truth_doc), collapse = " ")
  tesseract_nchar <- nchar(tesseract_one_vec) - (l_tess - 1)
  truth_nchar <- nchar(truth_one_vec) - (l_truth - 1)
  
  crct_nchar <- 0
  if(l_tess == l_truth){
    for (i in 1:l_tess){
      current_tesseract_line <- paste(tesseract_doc[[i]], collapse = " ")
      current_tesseract_line_nchar <- nchar(current_tesseract_line)
      current_truth_line <- paste(truth_doc[[i]], collapse = " ")
      current_truth_line_nchar <- nchar(current_truth_line)
      current_line_nchar_error <- levenshtein.damerau.distance(current_tesseract_line, current_truth_line)
      crct_nchar <- crct_nchar + (current_tesseract_line_nchar - current_line_nchar_error)
    }
  }
  else{
    doc_nchar_error <- levenshtein.damerau.distance(tesseract_one_vec, truth_one_vec)
    crct_nchar <- tesseract_nchar - doc_nchar_error
  }
  names(crct_nchar) <- NULL
  return(list("nchar_correct_OCR" = crct_nchar, "nchar_tesseract" = tesseract_nchar, "nchar_truth" = truth_nchar, "nline_tess" = l_tess))
}
