# 20180918_02_xgboostModel_WhatsCook.R
#
# @date : 2018/09/18(Tue.)
# @note : xgboostモデル(勾配ブースティング)

# install.packages
if(F){
  install.packages("xgboost", dependencies = T)
  install.packages("Matrix", dependencies = T)
  install.packages("ggplot2", dependencies = T)
  install.packages("ROCR", dependencies = T)
  install.packages("caret", dependencies = T)
}

# load libraries
library(xgboost)
library(Matrix)
library(dplyr)
library(ggplot2)
library(ROCR)
library(caret)

# statisticalMethodNm
statisticalMethodNm <- "xgboost"

if(T){
  # set working directory
  target_wd_ID <- 1 # 適宜変更
  targetWDList <- list()
  targetWDList[[1]] <- "" # 適宜変更
  targetWDList[[2]] <- "" # 適宜変更
  setwd(targetWDList[[target_wd_ID]])
  #print(getwd())
  
  # input/output directory
  inputDir <- "Rin/"
  outputDir <- paste("Rout/xgboost/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")
  source("Rpgm/InitSettings_02_WhatsCook.R", encoding="utf-8")
  source("Rpgm/xgboostSettings.R", encoding="utf-8")
}

# init
lstOutFilePath <- list()
lstPrint <- list()


#############################################
### 学習用(train)/検証用(valid)データ分割 ###
#############################################

# init
datTrain_All <- datTrain

# set
lstPrint[["nSample"]] <- paste0("nSample = ", format(nrow(datTrain_All), big.mark=","))


# partition data (train/valid)
if(T){
  set.seed(1234)
  
  # 分割比率(学習用:検証用=trainDataRatio:(1-trainDataRatio))
  if(T){
    #trainDataRatio <- 0.7   # 70%
    #trainDataRatio <- 0.8  # 80%
    trainDataRatio <- 0.9  # 90%
  }
  lstPrint[["trainDataRatio"]] <- paste0("trainDataRatio=", trainDataRatio)
  
  # 層別サンプリング
  if(T){
    res <- stratified_sampling_lstForm(df=datTrain_All, vecStrataList=c(targetName), 
                                       numOfSamplingRatio=trainDataRatio, isDeleteKey=F)
    datTrain <- res[[1]]
    
    idx_Train <- datTrain$id
    datTrain$id <- NULL
    
    print(names(res))
  }
  
  datTrain <- datTrain_All[idx_Train,]
  datValid <- datTrain_All[-idx_Train,]
  
  # set
  lstPrint[["partitionData"]] <- res[["summaryInfo"]]
}

#=== [END]:学習/テスト用データ分割 ===


# init
datTrain_ext <- datTrain
datValid_ext <- datValid
datTest_ext <- datTest


################## 
### モデル構築 ###
##################

if(T)
{
  # set
  datTrain <- datTrain[,c(targetName, targetVariablesList)]
  datValid <- datValid[,c(targetName, targetVariablesList)]
  
  # formula
  formulaStr <- formula(paste(targetName,"~."))
  
  # sparse.model.matrix()
  train.mx <- sparse.model.matrix(formulaStr, datTrain)
  valid.mx <- sparse.model.matrix(formulaStr, datValid)
  
  # xgb.DMatrix()
  dtrain <- xgb.DMatrix(train.mx, label=datTrain[,targetName])
  dvalid <- xgb.DMatrix(valid.mx, label=datValid[,targetName])
  
  # xgb.cv
  if(T)
  {
    if(T){
      train.cv <- xgb.cv(data=dtrain, 
                         nround=xgboostSettings[["nroundMax_cv"]], 
                         nthread=xgboostSettings[["nthread"]], 
                         nfold=xgboostSettings[["nfold"]], 
                         metrics=list(xgboostSettings[["metric"]]), 
                         max_depth=xgboostSettings[["max_depth"]], 
                         eta=xgboostSettings[["eta"]], 
                         objective=xgboostSettings[["objective"]], 
                         num_class=num_objective_class, 
                         verbose=T)
    }
    train.cv.result <- train.cv$evaluation_log
    train.cv.result <- as.data.frame(train.cv.result)
    
    idx_cv_train <- which.min(train.cv.result[,paste0("train_", xgboostSettings[["metric"]], "_mean")])
    idx_cv_valid <- which.min(train.cv.result[,paste0("test_", xgboostSettings[["metric"]], "_mean")])
    
    # set nrounds
    nround_via_cv <- train.cv.result[idx_cv_valid, "iter"]
    xgb.cv_Info <- cbind(idx_cv_train, idx_cv_valid, nround_via_cv, nroundMax_cv)
    print(xgb.cv_Info)
  }
  
  
  # xgb.train
  if(T){
    if(T){
      nrounds_xgb <- nround_via_cv
      #nrounds_xgb <- 20
    }
    
    train.gdbt <- xgb.train(data=dtrain, 
                            nround=nrounds_xgb, 
                            nthread=xgboostSettings[["nthread"]], 
                            metrics=list(xgboostSettings[["metric"]]),  
                            max_depth=xgboostSettings[["max_depth"]], 
                            objective=xgboostSettings[["objective"]], 
                            num_class=num_objective_class, 
                            watchlist=list(eval=dvalid, train=dtrain))
  }
}


########################################
### 変数重要度 (variable importance) ###
########################################

# 特徴ラベル名の取得
featureNmList <- train.mx@Dimnames[[2]]

# 変数重要度: xgb.importance()
varImp <- xgb.importance(feature_names=featureNmList, model=train.gdbt)
if(T) varImp <- varImp %>% dplyr::arrange(desc(Gain))
print(varImp)

# output csv(varImp)
if(T){
  no <- c(1:dim(varImp)[1])
  varImp <- cbind(no, varImp, stringsAsFactors=F)
  outFileName <- paste(statisticalMethodNm, "_varImp_", GetDateTime_YYYYMMDD_HHMMSS(), ".csv", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(varImp, outFilePath, row.names=F)
}

# 加工
if(T){
  df <- varImp[,c("Feature","Gain")] # use [Gain]
  colnames(df) <- c("id","value")
  if(T) df <- df %>% dplyr::arrange(desc(value))
  df$id <- factor(df$id, levels=df$id[order(df$value)])
  df <- as.data.frame(df)
  df[,"value"] <- round(df[,"value"]*100, 1)
  df[,"value2"] <- paste0(df[,"value"], "%")
}


###################
### plot varImp ###
###################

# plotVarImp_topN (01_topN=30)
if(T){
  topN <- min(30, nrow(df)) # 適宜変更
  titleLabel_varImp <- paste("Variable Importance (", str_a0, ", Top ", topN, " Items)", sep="")
  outFileName <- paste(statisticalMethodNm, "_varImp_topN=", topN, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  plotVarImp_topN(topN, df, outFilePath, titleLabel_varImp)
  
  # set
  lstOutFilePath[["varImp"]] <- outFilePath
}


################
### 予測結果 ###
################

# datTrain/datValid
if(T){
  pred_Train <- predict(train.gdbt, newdata=dtrain)
  pred_Valid <- predict(train.gdbt, newdata=dvalid)
  # set
  varname_NEW <- paste0(targetName, "_pred")
  datTrain_ext[, varname_NEW] <- pred_Train
  datValid_ext[, varname_NEW] <- pred_Valid
}


# datTest
if(T){
  # set
  datTest <- datTest[,c(targetVariablesList)]
  datTest[,targetName] <- 0
  
  # formula
  formulaStr <- formula(paste(targetName,"~."))
  
  # sparse.model.matrix()
  test.mx <- sparse.model.matrix(formulaStr, datTest)
  
  # xgb.DMatrix()
  dtest <- xgb.DMatrix(test.mx, label=datTest[,targetName])
  
  pred_Test <- predict(train.gdbt, newdata=dtest)
  print(table(pred_Test))
  
  varname_NEW <- paste0(targetName, "_pred")
  datTest_ext[, varname_NEW] <- pred_Test
}



#####################################
### モデル評価 (混同行列・正解率) ###
#####################################

if(T){
  
  # 混同行列 (ConfusionMatrix train)
  if(T){
    ConfMat_train_0 <- table(pred_Train, datTrain_ext[,targetName]) %>% as.data.frame.matrix()
    
    ConfMat_train <- ConfMat_train_0
    colnames(ConfMat_train) <- targetNameList
    rownames(ConfMat_train) <- NULL
    ConfMat_train <- cbind(targetNameList, ConfMat_train)
    colnames(ConfMat_train)[1] <- targetName
    no <- c(1:nrow(ConfMat_train))
    ConfMat_train <- cbind(no, ConfMat_train)
    
    # output csv (ConfMat_train)
    if(T){
      outFileName <- paste0("ConfusionMatrix_train_[", targetName, "].csv")
      outFilePath <- paste(outputDir, outFileName, sep="")
      write.csv(ConfMat_train, outFilePath, row.names=F, quote=F)
    }
  }
  
  
  # 混同行列 (ConfusionMatrix valid)
  if(T){
    ConfMat_valid_0 <- table(pred_Valid, datValid_ext[,targetName]) %>% as.data.frame.matrix()
    
    ConfMat_valid <- ConfMat_valid_0
    colnames(ConfMat_valid) <- targetNameList
    rownames(ConfMat_valid) <- NULL
    ConfMat_valid <- cbind(targetNameList, ConfMat_valid)
    colnames(ConfMat_valid)[1] <- targetName
    no <- c(1:nrow(ConfMat_valid))
    ConfMat_valid <- cbind(no, ConfMat_valid)
    
    # output csv (ConfMat_valid)
    if(T){
      outFileName <- paste0("ConfusionMatrix_valid_[", targetName, "].csv")
      outFilePath <- paste(outputDir, outFileName, sep="")
      write.csv(ConfMat_valid, outFilePath, row.names=F, quote=F)
    }
  }
  
  if(T){
    ConfMat_train_valid <- rbind(cbind(ConfMat_train, dataType="train"), 
                                 cbind(ConfMat_valid, dataType="valid"), stringsAsFactors=F)
    
    # output csv (ConfMat_train_valid)
    if(T){
      outFileName <- paste0("ConfusionMatrix_train_valid_[", targetName, "].csv")
      outFilePath <- paste(outputDir, outFileName, sep="")
      write.csv(ConfMat_train_valid, outFilePath, row.names=F, quote=F)
    }
  }
  
  # 正解率 (accuracy)
  if(T){
    Accuracy_train <- sum(diag(as.matrix(ConfMat_train_0))) / sum(ConfMat_train_0)
    Accuracy_valid <- sum(diag(as.matrix(ConfMat_valid_0))) / sum(ConfMat_valid_0)
  }
  
  AccuracyInfo <- data.frame("Accuracy_train"=Accuracy_train, "Accuracy_valid"=Accuracy_valid,
                             "Accuracy_diff"=Accuracy_train-Accuracy_valid)
  AccuracyInfo <- round(AccuracyInfo, 4)
  
  # set
  lstPrint[["ConfMat_train"]] <- ConfMat_train
  lstPrint[["ConfMat_valid"]] <- ConfMat_valid
  lstPrint[["AccuracyInfo"]] <- AccuracyInfo
}


######################
### 予測結果の出力 ###
######################

# output csv (df_submission)
if(T){
  df_submission <- data.frame("id"=datTest_org[,"id"], 
                              "targetName"=datTest_ext[,paste0(targetName, "_pred")], stringsAsFactors=F)
  colnames(df_submission) <- c("id", targetName)
  
  # merge
  df_submission <- dplyr::left_join(df_submission, targetNameList_map, by=c("m_cuisine")) %>% as.data.frame()
  df_submission[,targetName] <- NULL
  head(df_submission)
  table(df_submission[,2])
  
  outFileName <- paste0("submission_[", targetName, "]_", GetDateTime_YYYYMMDD_HHMMSS(),".csv")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(df_submission, outFilePath, row.names=F, quote=F)
}


#=== [END]:R-Script ===
