library(vwr)
library(stringi)
library(stringdist)
library(qualV)
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

#wci: correction candidate i
#we: error token
score <- function(wci, we, thres, max_freq, lexic, Tset, context_3gram, test_freq_ngrams, max_context_freq){
  
  #place_holder assignment
  combined_score <- 0
  
  len_wci <- nchar(wci)
  len_we <- nchar(we)
  
  #score components:
  
  #Paper: OCR Post-Processing Text Correction using Simulated Annealing
  
  #1. Minimum edit distance d(wci, we) as calculated by Levenshtein's edit distance.
  # with reference in paper Statistical Learning for OCR Text Correction
  d_inverse <- 1-levenshtein.distance(wci, we)/(thres+1)
  
  # 2. Normalized longest common subsequence (Allison and Dix, 1986) which takes into account
  # the length of both the shorter and the longer string for normalization.
  lcs <- sapply(seq_along(we), function(i)paste(LCS(substring(we[i], seq(1, nchar(we[i])), seq(1, nchar(we[i]))),
              substring(wci[i], seq(1, nchar(wci[i])), seq(1, nchar(wci[i]))))$LCS,
          collapse = ""))
  
  #twci <- drop(attr(adist(wci, we, counts=TRUE), "trafos"))
  #lcs <- paste(stri_sub(we, stri_locate_all_regex(twci, "M+")[[1]]), collapse = "")
  len_lcs <- nchar(lcs)
  nlcs <- 2*len_lcs^2/(len_wci+len_we)

  
  
  # 3. Normalized maximal consecutive longest common subsequence, which is a modification
  # of aforementioned factor by limiting the common subsequences to be consecutive.
  ## nmnlcs1
  ## get all forward substrings of 'we'
  swe1 <- stri_sub(we, 1, 1:nchar(we))
  ## extract them from 'wci' if they exist
  sstr1 <- na.omit(stri_extract_all_coll(wci, swe1, simplify=TRUE))
  ## match the longest one
  len_mclcs1 <-max(nchar(sstr1))
  nmnlcs1 <- 2*len_mclcs1^2/(len_wci+len_we)
  
  ## nmnlcsn
  n <-2
  swen <- stri_sub(we, n, n:nchar(we))
  sstrn <- na.omit(stri_extract_all_coll(wci, swen, simplify=TRUE))
  ## match the longest one
  len_mclcsn <-max(nchar(sstrn))
  nmnlcsn <- 2*len_mclcsn^2/(len_wci+len_we)
  
  ## nmnlcsz
  
  
  # Paper: Statistical Learning for OCR Text Correction
  
  #4 language popularity
  pop <- sum(lexic==wci)/max_freq
  
  #5 lexicon existance
  exis <- ifelse(wci %in% Tset, 1, 0)
  
  #6 context popularity: sliding window of 3-gram for a token: 5 words in total
  context_3gram_candi<- c(context_3gram[1:2], wci, context_3gram[4:5])
  slide_window<- list()
  slide_window[[1]] <- context_3gram_candi[1:3]
  slide_window[[2]] <- context_3gram_candi[2:4]
  slide_window[[3]] <- context_3gram_candi[3:5]
  
  #test_freq_ngrams is in main.rmd, function "count_slide" is in lib/features.R
  slide_freq <- unlist(lapply(slide_window, count_slide, test_freq_ngrams))
  context_pop <- sum(slide_freq, na.rm = TRUE)/max_context_freq
  
  #######################################################################
  # combined_score <- some function of d_inverse, nlcs, nmnlcs, pop, exis, context_pop
  
  
  return(combined_score)
}



lexic_freq <- function(cur_token, lexic){
  return(sum(lexic == cur_token))
}

context_freq <- function(cur_candi, context_3gram, test_freq_ngrams){
  context_3gram_candi<- c(context_3gram[1:2], cur_candi, context_3gram[4:5])
  
  slide_window<- list()
  slide_window[[1]] <- context_3gram_candi[1:3]
  slide_window[[2]] <- context_3gram_candi[2:4]
  slide_window[[3]] <- context_3gram_candi[3:5]
  
  #test_freq_ngrams is in main.rmd, function "count_slide" is in lib/features.R
  slide_freq <- unlist(lapply(slide_window, count_slide, test_freq_ngrams))
  context_fq <- sum(slide_freq, na.rm = TRUE)
  return(context_fq)
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



