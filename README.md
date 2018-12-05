# ADS Project 5: 

Term: Fall 2018

+ Sec 2 Group 1 
+ Projec title: Improvement on Post-processing for OCR data
+ Team members(in alphabetical order)
	+ team member 1: Yang Cai
	+ team member 2: Yunsheng Ma
	+ team member 3: Jiaxi Wu
	+ team member 4: Huiming Xie
	+ team member 5: Jiaqian Yu
	
+ Project summary: This project is an improvement on Project 4: Opitical character recognition (OCR). We created an OCR post-processing procedure to enhance Tesseract OCR output based on the method described in Statistical Learning for OCR Text correction.

+ **Error detection**: We made our improvement based on D3 paper: Recognizing Garbage in OCR Output on Historical Documents and Statistical Learning for OCR Text correction. 

**Features**:
+ the length l of the input string

+ the number v of vowels and the number c of consonants in the string, as well as the quotients v/l, c/l, v/c (for c = 0)

+ the number of special (non-alphanumerical) symbols s and the quotient s/l

+ the number of digits d and the quotient d/l 

+ the number of lowercase letters low, the number of uppercase letters upp, and the quotients low/l, upp/l

+ for strings containing a sequence of at least three consecutive occurrences of the same symbol, we use the quotient of the length of the maximal sequence of identical letters divided by l. For other strings the feature receives value 0

+ We calculate the number of all alpha-numerical symbols lα occurring in the string, and the number k of other symbols s. For k > lα the value Feature 7 is 1,for other strings the value is 0

+ If the input string contains a subsequence of ≥ 6 directly consecutive consonants, Feature 8 received value 1, otherwise value 0

+ We delete the first and last symbol of the input string. If the remaining infix contained two or more non alpha-numerical symbols, Feature 9 receives value 1, and otherwise value 0

+ bigram sum(frequency of the ith bigram in the list Lb/10000)/number of bigrams in input string

+ We computed the number of occurrences i of the most frequent symbol of the input string of length l. For i ≥ 3 we used i/l as a feature value, for i ≤ 2 the value was set to 0

+ Let l1 denote the number of occurrences of alphabetical symbols in the input string, let l2 = l − l1 denote the number of occurrences of all other types of symbols. We used l2/l1 as a feature.

+ Levenshtein distance

+ Consider a common word is less likely to be an error word, the 1-gram frequency of a word should be greater than a frequency threshold. The frequency threshold varies with different word length.

+ A word is likely to be correct if this word with its context occurs in other places. We use a sliding window to construct n-gram contexts for a word. The frequency of one of the context in the n-gram corpus should be greater than a frequency threshold.


**Model**:
Change from SVM to Random Forest.

+ **Error Correction**:
+ Step 1. Candidate Search

  + Select a candidate for each error according to Levenshtein distance

+ Step 2. Compute Feature Scores
  + Levenshtein edit distance
  + String similarity
  + Language popularity
  + Lexicon existence
  + Exact-context popularity

+ Step 3.Train Model
  + Use scores as features
  + Label 1 if the candidate is the same as ground truth, label 0 otherwise.

+ Step 4. Candidate Ranking & Correct
  + Rank test set error candidates
  + Choose the one with highest score for correction



	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

**References**
+ Recognizing Garbage in OCR Output on Historical Documents
+ Statistical Learning for OCR Text Correction

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
