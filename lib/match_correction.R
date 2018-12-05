match_correct <- function(tesseract_doc, truth_doc){
  
  l_tess <- length(tesseract_doc)
  l_truth <- length(truth_doc)
  
  correction_list <- list()
  line_diff <- 0
  if(l_tess == l_truth){
    for (i in 1:l_truth){
      correction_list[[i]] <- truth_doc[[i]][!truth_doc[[i]] %in% tesseract_doc[[i]]]
    }
  }
  else{
    line_diff <- l_tess - l_truth
    for (i in 1:l_truth){
      correction_list[[i]]<- truth_doc[[i]][!truth_doc[[i]] %in% unlist(tesseract_doc[max(1,i-abs(line_diff)):min(i,l_tess)])]
    }
  }
  return(correction_list)
}


