xgb_train <- function(dat_train, label_train, par=NULL){

  ### Input: 
  ###  -  training features and labels
  ### Output: a list for trained models
  
  ### load libraries
  library("xgboost")
  
  ### Train gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }

  fit_xgb <- xgboost(data=dat_train, label_train,
                     eta=0.5,
                     gamma=0,
                     max.depth=depth,
                     min_child_weight=10,
                     subsample = 0.5,
                     colsample_bytree = 0.8,
                     nthread = 3, 
                     nrounds = 200,
                     early_stopping_rounds = 20,
                     objective = "reg:logistic",
                     scale_pos_weight=5,
                     verbose=0)
    
  return(fit_xgb)
}