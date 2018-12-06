xgb_test <- function(model, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model list using training data
  ###  - processed score features from correction candidates
  ### Output: training model specification
  
  ### load libraries
  library("xgboost")
  
  prediction <- predict(model, newdata=dat_test)

  return(as.numeric(prediction))
}