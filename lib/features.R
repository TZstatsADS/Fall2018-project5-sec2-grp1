stringsplit_1st <- function(line){
  return(strsplit(line, " ")[[1]])
}

cut_bigram <- function(start_pos, token){
  return(substr(token, start = start_pos, stop = start_pos+1))
}

bigram_from_token <- function(input_string){
  nb <- nchar(input_string)-1
  if(nb >= 2) { bigram <- sapply(1:nb, cut_bigram, input_string) }
  else { bigram <- input_string }
  return(bigram)
}

num_pattern <- function(candidate, cur_token){
  ldist <- adist(cur_token, candidate, counts = TRUE)
  return(sum(as.vector(attr(ldist, "counts")) != 0))
}

count_slide <- function(ngram, ngram_table){
  if(is.null(ngram)){
    return(0)
  } else {
    return(ngram_table[ngram])
  }
  
}

cur_token_features <- function(cur_token, tvec, truth_set, freq_bigrams){
    
    feature_list <- list()
  
    #feature 1 the length l of input string
    l <- nchar(cur_token)
    feature_list[[1]] <- l

    #feature 2 number of vowels v, number of cosonants c, v/l, c/l, v/c
    vowels <- '[aeiouAEIOU]'
    consonants <- '[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]'
    v <- str_count(cur_token, pattern = vowels)
    c <- str_count(cur_token, pattern = consonants)
    feature_list[[2]] <- c(v,c,v/l,c/l,ifelse(c>0,v/c,100))

    #feature 3 number of special symbols s and s/l
    sb <- 0
    if ( l >= 2){
      bulk <- substr(cur_token, start = 1, stop = l-1)
      sb <- str_count(bulk, pattern = "[^[:alnum:]\\\"\\']")
    }
    feature_list[[3]] <- c(sb, sb/l)

    #feature 4 number of digits d and d/l
    d <- str_count(cur_token, pattern = '[0-9]')
    feature_list[[4]] <- c(d, d/l)

    #feature 5 number of lower case letters(low), number of uppercase letters(upp), low/l, upp/l
    low <- str_count(cur_token, pattern = '[:lower:]')
    upp <- str_count(cur_token, pattern = '[:upper:]')
    feature_list[[5]] <- c(low, upp, low/l, upp/l)

    #feature 6 for strings containing a sequence of at least three consecutive occurences of the same symbol
    #we use the quotient of the length of the maximal sequnce of identical letters divived by l
    #for other strings the feature receives value 0
    string_split <- strsplit(cur_token, "")[[1]]
    consecutive_cnt <- rle(string_split)
    max_seq <- max(consecutive_cnt[[1]])
    feature_list[[6]] <- ifelse(max_seq>=3, max_seq/l, 0)

    #feature 7 calculate the number of all alpha-numerical symbols la occuring in the string
    #number k of other symbols
    #for k>la value is 1. otherwise, value is 0.
    s <- str_count(cur_token, pattern = '[^[:alnum:]]')
    la <- str_count(cur_token, pattern = '[:alnum:]')
    feature_list[[7]] <- ifelse(s>la, 1, 0)

    #feature 8 if the input string contains a subsequence of >=6 directly consecutive consonants
    #Receive value 1, otherwise value 0.
    conso_token <- gsub(consonants, "b", cur_token)
    consecutive_conso <- rle(strsplit(conso_token, "")[[1]])
    max_conso_seq <- max(c(0, consecutive_conso[[1]][consecutive_conso[[2]] == "b"]))
    feature_list[[8]] <- ifelse(max_conso_seq>=6, 1, 0)

    #feature 9 delete the first and last symbol of the input string
    #if the remaining infix contained two or more non-alpha numerical symbols, receives value 1
    #otherwise value 0
    token_infix <- substr(cur_token, start = 2, stop = l-1)
    n <- str_count(token_infix, pattern = '[^[:alnum:]]')
    feature_list[[9]] <- ifelse(n>=2, 1, 0)

    #feature 10 bigram sum(frequency of the ith bigram in the list Lb/10000)/number of bigrams in input string
    cur_bigrams <- bigram_from_token(tolower(cur_token))
    n <- length(cur_bigrams)
    if(n == 1) { if(nchar(cur_bigrams) == 1) n <- 0.5 }
    feature_list[[10]] <- (sum(freq_bigrams[cur_bigrams],na.rm = TRUE)/10000)/n

    #feature 11 compute the number of occurences i of the most frequent symbol of the input string of length l
    #if i>=3, use i/l as feature value
    #if i <=2, use feature value=0
    max_freq <- max(table(string_split))
    feature_list[[11]] <- ifelse(max_freq>=3, max_freq/l, 0)

    #feature 12 l1:number of occurences of alphabetical symbols in the input string
    #l2=l-l1
    #l2/l1 as feature
    l1 <- str_count(cur_token, pattern = '[:alpha:]')
    l2 <- l - l1
    feature_list[[12]] <- ifelse(l1>0, l2/l1, 100)

    #feature 13 leveshtein distance
    all.words <- unique(c(english.words, truth_set))
    nv <- levenshtein.distance(tolower(cur_token), all.words)
    min_dist <- min(nv)
    if(min_dist > 2) { f13 <- l }
    else {
      candidate.words <- all.words[which.min(nv)]
      p <- min(sapply(candidate.words, num_pattern, cur_token))
      f13 <- (min_dist+(p/2)+1)/l
    }
    feature_list[[13]] <- f13
      
    #feature 14 1-gram frequency of a word
    f14 <- sum(tvec == cur_token)/l
    feature_list[[14]] <- ifelse(f14>=2,0,1)
    # n1 <- ngram(cur_token,n=1)
    # ifelse(get.phrasetable(n1)$freq>=2,feature_list[[14]] <- 0, feature_list[[14]] <- 1)
        
    ft <- unlist(feature_list)

  return(ft)
}

prepare_ngram <- function(vec_list){
  nlist <- length(vec_list)
  low_train_list <- tolower(vec_list)
  gram_list <- rep(list(NULL), nlist)
  for (i in 1:nlist){
    ni <- length(vec_list[[i]])
    low_train <- tolower(vec_list[[i]])
    gram_list[[i]]<- rep(NA, ni-4)
    for (j in 1:(ni-2)){
      gram_list[[i]][j] <- paste(low_train[j:(j+2)], collapse = " ")
    }
  }
  gram_vec <- unlist(gram_list)
  return(gram_vec)
}

extract_feature <- function(tesseract_list, truth_set, grams, freq_bigrams){
  #data preparation for 5-gram sliding window
  # truth_set <- unique(ground_truth)
  
  features <- NULL
  
  for (i in 1:length(tesseract_list)){
    ni <- length(tesseract_list[[i]])
    low_tesseract_vec <- tolower(tesseract_list[[i]])
    feature_mat <- matrix(NA, ncol = 24, nrow = ni)
    
    feature_mat[, -24] <- matrix(unlist(lapply(tesseract_list[[i]], cur_token_features, unlist(tesseract_list), truth_set, freq_bigrams)), byrow = TRUE, ncol = 23)
    for(j in 1:ni){
        #f24 sliding window, 3-gram freq
        f24 <- 0
        slide_window <- rep(list(NULL), 3)
        if(j >= 3){slide_window[[1]] <- paste(low_tesseract_vec[(j-2):j], collapse = " ")}
        if(j >= 2 & j <= ni - 1){slide_window[[3]] <- paste(low_tesseract_vec[(j-1):(j+1)], collapse = " ")}
        if(j <= ni - 2){slide_window[[5]] <- paste(low_tesseract_vec[j:(j+2)], collapse = " ")}

        slide_freq <- unlist(lapply(slide_window, count_slide, grams))
        f24 <- max(slide_freq, na.rm = TRUE)
        feature_mat[j,24] <- f24
    }
    features <- rbind(features, feature_mat)  
  }
  return(features)
}
  
token_to_input_format <- function(vec, freq_bigrams, truth_set){
  input_format <- matrix(unlist(lapply(vec,extract_feature,freq_bigrams,truth_set)), byrow = TRUE, nrow = length(vec))
  return(input_format)
}

get_line_input <- function(token, input_matrix){
  return(input_matrix[which(rownames(input_matrix) == token),])
}