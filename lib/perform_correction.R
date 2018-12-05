do_correction_all <- function(cur_token, left, right, pred_err, 
                              all_candidates=dict_candidates){
  # check if cur_token is an error
  if(pred_err) {
    return(list(c=cur_token, legal=TRUE, num=NA, score=NA))
  }
  else{
    # check if cur_token has any plausible correction
    candidates <- propose_candidate(cur_token)
    if(length(candidates)==0) {
      return(list(c=cur_token, legal=FALSE, num=0, score=NA))
    }
    else {
      scores <- sapply(candidates, calculate_score, cur_token, left, right)
      max_ind <- which.max(scores)
      best_candid <- candidates[max_ind]
      corrected <- 
        ifelse(cur_token==toupper(cur_token), toupper(best_candid), 
               ifelse(substr(cur_token,1,1) %in% LETTERS, 
                      paste(toupper(substr(best_candid,1,1)), substr(best_candid,2,nchar(best_candid)), sep=""),
                      best_candid))
      return(list(c=corrected, legal=FALSE, num=length(candidates), score=as.numeric(scores[max_ind])))
    }
  }
}

