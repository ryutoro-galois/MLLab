# xgboostSettings.R
#
# @date : 2018/09/18(Tue.)

# init
xgboostSettings <- list()


# eta_xgb
if(T){
  eta_xgb <- 0.1
  #eta_xgb <- 0.2
  #eta_xgb <- 0.3
  xgboostSettings[["eta"]] <- eta_xgb
}

# nfold
if(T){
  #nfold <- 3
  nfold <- 5
  #nfold <- 10
  xgboostSettings[["nfold"]] <- nfold
}

# max_depth
if(T){
  #max_depth <- 3
  #max_depth <- 4
  #max_depth <- 5
  max_depth <- 6
  #max_depth <- 7
  xgboostSettings[["max_depth"]] <- max_depth
}

# nroundMax_cv
if(T){
  #nroundMax_cv <- 200
  #nroundMax_cv <- 50
  nroundMax_cv <- 20
  #nroundMax_cv <- 10
  #nroundMax_cv <- 5
  xgboostSettings[["nroundMax_cv"]] <- nroundMax_cv
}


# 計算使用スレッド
if(T){
  #nthread <- 1
  #nthread <- parallel::detectCores() / 2
  nthread <- parallel::detectCores() - 1
  xgboostSettings[["nthread"]] <- nthread
}


# objective
if(T){
  objective <- "multi:softmax"
  #objective <- "multi:softprob"
  xgboostSettings[["objective"]] <- objective
}

# metric
if(T){
  metric <- "mlogloss"
  xgboostSettings[["metric"]] <- metric
}


#=== [END]:R-Script ===