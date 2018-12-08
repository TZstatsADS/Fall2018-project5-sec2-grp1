library(vwr)
library(stringi)
library(stringdist)
library(qualV)
library(rapportools)

#candidate search
# for each erroneous word, a set of candidate corrections is recommended within a limited number of character modifications based
# on calculating minimum edit distance between the erroneous word and the candidate correction. We make use of Levenshtein's edit distance (Levenshtein,
# 1966) to calculate the minimum edit distance consisting of the standard three operations, namely, insertion, deletion or substitution. The threshold is chosen heuristically on the basis of experiments
# conducted.

#assign score component to each correction candidate
# This module makes use of the output of previous module, i.e. a set of candidate corrections (wci) for each erroneous term (we), to assign
# a score to each candidate correction using simulated annealing (SA) algorithm (Kirkpatrick et al.,1983). SA requires an aperiodic Markov chain defined
# on a certain state space, and a cooling schedule to iteratively push the solution towards the optimum. In this module, the state space is set of all
# candidate corrections. We calculate a similarity score for each of the candidate corrections (wci) on the basis of the following three factors:

#wc: correction candidates
#we: error token

score_features <- function(wc, we, thres, lexicon, context_we, context_3grams_freq){
  
  #place_holder assignment

  len_wc <- nchar(wc)
  len_we <- nchar(we)
  num_cand <- length(wc)
  
  #score components:
  
  #Paper: OCR Post-Processing Text Correction using Simulated Annealing
  
  #1. Minimum edit distance d(wci, we) as calculated by Levenshtein's edit distance.
  # with reference in paper Statistical Learning for OCR Text Correction
  score1 <- as.vector(1-levenshtein.distance(wc, we)/(thres+1))

  # 2. String similarity
  # 2.1 Normalized longest common subsequence (Allison and Dix, 1986) which takes into account
  # the length of both the shorter and the longer string for normalization.
  score2 <- rep(0, num_cand)
  for (j in 1:num_cand){
    lcs <- sapply(seq_along(we), function(i)paste(LCS(substring(we[i], seq(1, nchar(we[i])), seq(1, nchar(we[i]))),
                                                      substring(wc[j][i], seq(1, nchar(wc[j][i])), seq(1, nchar(wc[j][i]))))$LCS,
                                                  collapse = ""))
    
    #twci <- drop(attr(adist(wci, we, counts=TRUE), "trafos"))
    #lcs <- paste(stri_sub(we, stri_locate_all_regex(twci, "M+")[[1]]), collapse = "")
    len_lcs <- nchar(lcs)
    nlcs <- 2*len_lcs^2/(len_wc[j]+len_we)
    
    # 2.2 Normalized maximal consecutive longest common subsequence, which is a modification
    # of aforementioned factor by limiting the common subsequences to be consecutive.
    ## nmnlcs1
    ## get all forward substrings of 'we'
    swe1 <- stri_sub(we, 1, 1:nchar(we))
    ## extract them from 'wci' if they exist
    sstr1 <- na.omit(stri_extract_all_coll(wc[j], swe1, simplify=TRUE))
    ## match the longest one
    len_mclcs1 <- ifelse(is.empty(nchar(sstr1))[1], 0, max(nchar(sstr1)))
    nmnlcs1 <- 2*len_mclcs1^2/(len_wc[j]+len_we)
    
    ## 2.3 nmnlcsn
    mclcsn <- rep(0,nchar(we)-1)
    for (i in 2:nchar(we)){
      n <- i
      swen <- stri_sub(we, n, n:nchar(we))
      sstrn <- na.omit(stri_extract_all_coll(wc[j], swen, simplify=TRUE))
      ## match the longest one
      mclcsn[i] <- ifelse(is.empty(nchar(sstrn))[1],0,max(nchar(sstrn)))
    }
    len_mclcsn <- max(mclcsn)
    nmnlcsn <- 2*len_mclcsn^2/(len_wc[j]+len_we)
    
    ## nmnlcsz
    mclcsz <- rep(0,nchar(we))
    for (i in 1:nchar(we)){
      swen <- substr(we, start=i, stop=nchar(we))
      sstrn <- na.omit(stri_extract_all_coll(wc[j], swen, simplify=TRUE))
      ## match the longest one
      mclcsz[i] <- ifelse(is.empty(nchar(sstrn)),0,max(nchar(sstrn)))
    }
    len_mclcsz <- max(mclcsz)
    nmnlcsz <- 2*len_mclcsz^2/(len_wc[j]+len_we)
    
    ## weights for nlcs
    alpha1 <- 0.25
    alpha2 <- 0.25
    alpha3 <- 0.25
    alpha4 <- 0.25
    
    score2[j] <- alpha1*nlcs+alpha2*nmnlcs1+alpha3*nmnlcsn+alpha4*nmnlcsz
  }

  # Paper: Statistical Learning for OCR Text Correction
  
  #3 language popularity
  wc_freq <- lexic_freq(wc, lexicon)
  score3 <- wc_freq/max(wc_freq)

  #4 lexicon existance
  exis <- ifelse(wc %in% lexicon, 1, 0)
  
  #5 context popularity: sliding window of 3-gram for a token: 5 words in total
  wc_context_freq <- context_freq(wc, context_we, context_3grams_freq)
  score4 <- wc_context_freq/max(wc_context_freq)

  #######################################################################
  feature_scores <- data.frame(L_dist=score1, str_sim=score2, lang_pop=score3, lexi_exist=exis, 
                               context_pop=score4 #,relaxed_context_pop=score5
                               )
  return(feature_scores)
}

lexic_freq <- function(tokens, lexic){
  # calculate the candidate frequency
  freq <- rep(0, length(tokens))
  for (i in 1:length(tokens)){
    freq[i] <- sum(lexic == tokens[i])
  }
  return(freq)
}

context_freq <- function(candidates, context_we, context_3grams_freq){
  # calculate the 3-gram context frequency
  context_fq <- rep(0, length(candidates))
  for (i in 1:length(candidates)){
    context_3gram_candi <- c(context_we[1:2], candidates[i], context_we[4:5])
    
    slide_window<- list()
    slide_window[[1]] <- context_3gram_candi[1:3]
    slide_window[[2]] <- context_3gram_candi[2:4]
    slide_window[[3]] <- context_3gram_candi[3:5]
    slide_freq <- unlist(lapply(slide_window, count_freq, context_3grams_freq))
    context_fq[i] <- sum(slide_freq, na.rm = TRUE)
  }
  return(context_fq)
}

relax_context_freq <- function(candidates, context_we, context_3grams_freq){
  # calculate the relaxed 3-gram context frequency
  context_fq <- rep(0, length(candidates))
  for (i in 1:length(candidates)){
    context_3gram_candi <- c(context_we[1:2], candidates[i], context_we[4:5])
    slide_window <- list()
    slide_window[[1]] <- context_3gram_candi[1:3]
    slide_window[[2]] <- context_3gram_candi[2:4]
    slide_window[[3]] <- context_3gram_candi[3:5]
    slide_freq <- unlist(lapply(slide_window, count_relaxed_freq, context_3grams_freq))
    context_fq[i] <- sum(slide_freq, na.rm = TRUE)
  }
  return(context_fq)
}

count_freq <- function(ngram, ngram_table){
  ngram_paste <- paste(ngram, collapse = " ")
  if(ngram_paste %in% ngram_table$ngram){
    return(ngram_table$count[ngram_table$ngram == ngram_paste])
  } else{
    return(0)
  }
}

count_relaxed_freq <- function(ngram, ngram_table){
  freq <- 0
  for (i in 1:length(ngram_table$ngram)){
    n_gram <- unlist(strsplit(ngram_table$ngram[i], ' '))
    if (sum(n_gram == ngram) >= 2){
      freq <- freq + ngram_table$count[i]
    }
  }
  return(freq)
}

assign_correction <- function(error_token, train_ground_truth_set = NULL, thres = 3, context_3gram, test_freq_ngrams){
  #a lexicon made of all english.words in library(vwr) and all unique ground truth in the training set(same set as detection)
  lexic <- unique(c(english.words, tolower(train_ground_truth_set)))
  #candidate search (levenshtein.neightbors does not consider reversion as it is rare in OCR)
  candidates <- unlist(levenshtein.neighbors(error_token, all_words)[1:thres])
  
  max_freq <- max(unlist(lapply(candidates, lexi_freq, lexic)))
  
  max_context_freq <- max(unlist(lapply(candidates, context_3gram, test_freq_ngrams)))
  
  scores <- unlist(lapply(candidates, score, error_token, thres, max_freq, lexic, train_ground_truth_set, context_3gram, test_freq_ngrams, max_context_freq))
  
  correction <- candidates[which.max(scores)]
  
  return(correction)
}



