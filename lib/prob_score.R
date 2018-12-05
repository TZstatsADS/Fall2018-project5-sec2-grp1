# Step 1. find candidates differ from the error token by one edit distance
propose_candidate <- function(err_token, all_candidates=dict_candidates){
  nv <- levenshtein.damerau.distance(tolower(err_token), all_candidates)
  return(all_candidates[nv == 1])
}

# Step 2.1 calculate prior probabilities
calculate_prior <- function(train_corpus, all_candidates){
  train_corpus <- tolower(train_corpus)
  N <- length(train_corpus)
  V <- length(all_candidates)
  freqs <- rep(list(0), length(all_candidates))
  names(freqs) <- all_candidates
  nonzero_freqs <- table(train_corpus)
  freqs[names(nonzero_freqs)] <- nonzero_freqs
  priors <- lapply(freqs, function(x) { (x + 0.5)/(N + V/2) })
  return(priors)
}

# Step 2.2 calculate channel probabilities

# apply Good-Turing estimation method
gt_estimation <- function(cur_freq, freq_table){
  all_freq <- as.integer(names(freq_table))
  if(cur_freq == max(all_freq)) {
    adj_freq <- 0.00001
  }
  else {
    pos <- which(all_freq == cur_freq)
    Nr <- as.integer(freq_table[pos])
    Nr1 <- as.integer(freq_table[(pos+1)])
    adj_freq <- (cur_freq + 1) * Nr1 / Nr
  }
  return(as.numeric(adj_freq))
}

# calculate chars[x, y] and chars[x]
calculate_chars <- function(train_freq_bigrams, train_truth){
  train_truth <- tolower(train_truth)
  # chars[x, y]
  # create an empty list for 27*27-1 letter bigrams
  chars_bigram <- rep(list(0), (27*27-1))
  names(chars_bigram) <- unlist(
    lapply(c(letters,"#"), function(x){ paste(x, c(letters,"#"), sep="") }))[1:(27*27-1)]
  # actual frequencies of 26*26 alphabetic bigrams
  nonzero_chars_bigram <- train_freq_bigrams[grepl('^[A-Za-z]{2}$', names(train_freq_bigrams))]
  chars_bigram[names(nonzero_chars_bigram)] <- nonzero_chars_bigram
  # frequencies of 2*26 bigrams with # (NULL)
  chars_bigram[paste("#", letters, sep="")] <- sapply(letters, function(x) { sum(startsWith(train_truth, x)) })
  chars_bigram[paste(letters, "#", sep="")] <- sapply(letters, function(x) { sum(endsWith(train_truth, x)) })
  # apply GT estimation to adjust frequecies
  chars_bigram_adj <- lapply(chars_bigram, gt_estimation, table(unlist(chars_bigram)))
  
  # chars[x]
  # create an empty list for 27 letters
  chars_unigram <- rep(list(0), 27)
  names(chars_unigram) <- c(letters,"#")
  # actual frequencies of 26 alphabetic letters
  train_corpus_l <- paste(train_truth, collapse = " ")
  chars_unigram[letters] <- sapply(letters, function(x) { str_count(train_corpus_l, pattern = x) })
  # frequencies of # (NULL), needed for insertion
  # remove the first character if alphabetic and check if the remaining token is still legal
  train_truth_remain <- sapply(train_truth,
                               function(x) { ifelse(substr(x, 1, 1) %in% letters, substring(x, 2), x)})
  chars_unigram["#"] <- sum(train_truth_remain %in% dict_candidates)
  # apply GT estimation to adjust frequecies
  chars_unigram_adj <- lapply(chars_unigram, gt_estimation, table(unlist(chars_unigram)))
  return(list(chars_bi=chars_bigram_adj, chars_uni=chars_unigram_adj))
}

# apply Good-Turing estimation on the confusion matrix
gt_confusion <- function(confusion){
  confusion_temp <- confusion[, 2:27]
  rownames(confusion_temp) <- confusion[, 1]
  confusion_freq <- table(unlist(confusion_temp))
  confusion_adj <- t(apply(confusion_temp, 1, function(x) {sapply(x, gt_estimation, confusion_freq)}))
  return(data.frame(confusion_adj))
}

calculate_channel <- function(cur_candidate, cur_error, 
                              del_adj=adj_del, add_adj=adj_add, sub_adj=adj_sub, rev_adj=adj_rev,
                              chars_bigram_adj=chars_bi, chars_unigram_adj=chars_uni){
  cur_error <- tolower(cur_error)
  cur_dist <- adist(cur_error, cur_candidate, counts = TRUE)
  # reversal if adist == 2
  if(cur_dist == 2) {
    p <- str_locate(attr(cur_dist,"trafos")[1,1], "D")[1]
    Cp <- substr(cur_candidate, p, p)
    Cp1 <- substr(cur_candidate, (p+1), (p+1))
    numerator <- rev_adj[Cp, Cp1]
    denom <- chars_bigram_adj[paste(Cp, Cp1, sep="")][[1]]
  }
  
  # deletion if ins == 1
  pattern_log <- as.logical(attr(cur_dist, "counts"))
  if(pattern_log[1]) {
    p <- str_locate(attr(cur_dist,"trafos")[1,1], "I")[1]
    Cp_1 <- ifelse(p == 1, "#", substr(cur_candidate, (p-1), (p-1)))
    Cp <- substr(cur_candidate, p, p)
    numerator <- del_adj[Cp_1, Cp]
    denom <- chars_bigram_adj[paste(Cp_1, Cp, sep="")][[1]]
  } 
  
  # insertion if del == 1
  if(pattern_log[2]) {
    p <- str_locate(attr(cur_dist,"trafos")[1,1], "D")[1]
    Cp_1 <- ifelse(p == 1, "#", substr(cur_candidate, (p-1), (p-1)))
    Tp <- substr(cur_error, p, p)
    numerator <- add_adj[Cp_1, Tp]
    denom <- chars_unigram_adj[Cp_1][[1]]
  }
  
  # substitution if sub == 1
  if(pattern_log[3]) {
    p <- str_locate(attr(cur_dist,"trafos")[1,1], "S")[1]
    Tp <- substr(cur_error, p, p)
    Cp <- substr(cur_candidate, p, p)
    numerator <- sub_adj[Tp, Cp]
    denom <- chars_unigram_adj[Cp][[1]]
  }
  
  # treat non-alphabetic characters as one class and assume their frequency would be larger than the others 
  numerator <- ifelse(is.numeric(numerator), numerator, 0.00001)
  denom <- ifelse(is.numeric(denom), denom, 0.00001)
  return(numerator/denom)
}

# Step 2.3 calculate context probabilities
calculate_context <- function(left, right, context_unique=context_names, context_gt=adj_context){
  if((left %in% names(context_unique)) & (right %in% context_unique[[left]])) {
    adj_freq <- context_gt[[left]][right]
  }
  else {
    ## for r = 0: Nr+1 / Nr 
    adj_freq <- 99161/614194897
  }
  return(adj_freq)
}

# Step 3 calculate probability score
calculate_score <- function(cur_candidate, cur_error, left, right, prior=priors){
  contexts <- calculate_context(cur_candidate, right) * calculate_context(left, cur_candidate)
  score <- priors[[cur_candidate]] * calculate_channel(cur_candidate, cur_error) * contexts
  return(score)
}













