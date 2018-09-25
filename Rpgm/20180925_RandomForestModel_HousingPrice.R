# 20180925_RandomForestModel_HousingPrice.R
#
# @date : 2018/09/25(Tue.)
# @note : ランダムフォレストモデル

# install.packages
if(F){
  install.packages("ROCR", dependencies = T)
  install.packages("caret", dependencies = T)
  install.packages("ranger", dependencies = T)
  install.packages("ggplot2", dependencies = T)
}

# load libraries
library(ROCR)
library(caret)
library(ranger)
library(ggplot2)

# statisticalMethodNm
statisticalMethodNm <- "RandomForestModel"


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
  outputDir <- paste("Rout/ranger/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")
  source("Rpgm/RandomForestSettings.R", encoding="utf-8")
  source("Rpgm/InitSettings_HousingPrice.R", encoding="utf-8")
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
    trainDataRatio <- 0.7   # 70%
    #trainDataRatio <- 0.8  # 80%
    #trainDataRatio <- 0.9  # 90%
  }
  lstPrint[["trainDataRatio"]] <- paste0("trainDataRatio=", trainDataRatio)
  
  # 層別サンプリング
  if(T){
    res <- stratified_sampling_lstForm(df=datTrain_All, vecStrataList=c("SalePriceScale"), 
                                       numOfSamplingRatio=trainDataRatio, isDeleteKey=F)
    datTrain <- res[[1]]
    
    idx_Train <- datTrain$id
    datTrain$id <- NULL
    
    print(names(res))
    
    datTrain <- datTrain_All[idx_Train,]
    datValid <- datTrain_All[-idx_Train,]
    
    round(table(datTrain[,"SalePriceScale"])/sum(table(datTrain[,"SalePriceScale"])),2)
    round(table(datValid[,"SalePriceScale"])/sum(table(datValid[,"SalePriceScale"])),2)
    
    # set
    lstPrint[["partitionData"]] <- res[["summaryInfo"]]
  }
  
  # ランダムサンプリング
  if(F){
    numOfTrainSample <- round(nrow(datTrain_All)*trainDataRatio, 0)
    idx_Train <- sample(nrow(datTrain_All), numOfTrainSample)
    datTrain <- datTrain_All[idx_Train,]
    datValid <- datTrain_All[-idx_Train,]
    
    summaryInfo <- data.frame("nSampleAll"=nrow(datTrain_All), "numOfTrainSample"=numOfTrainSample, "trainDataRatio"=trainDataRatio)
    
    # set
    lstPrint[["partitionData"]] <- summaryInfo
  }
}

#=== [END]:学習/テスト用データ分割 ===


# init
datTrain_ext <- datTrain
datValid_ext <- datValid
datTest_ext <- datTest



##################
### モデル構築 ###
##################

# set
datTrain <- datTrain[,c(targetName, targetVariablesList)]

# formula
formulaStr <- formula(paste(targetName,"~."))

# set minNodeSize
if(minNodeSize_type == 1){
  minNodeSize <- round(nrow(datTrain)*minNodeSizeRatio, 0)
} else if(minNodeSize_type == 2){
  minNodeSize <- min(minNodeSizeByManual, nrow(datTrain))
}

# RFSettinsInfo (for log)
RFSettinsInfo <- cbind(mtry_RF, nTree_RF, minNodeSize_type, minNodeSize, minNodeSizeRatio, minNodeSizeByManual, CNST_SEED)
print(RFSettinsInfo)

# execute rangerRF
if(T){
  system.time(
    rf <- ranger(formulaStr, data=datTrain, mtry=mtry_RF, num.trees=nTree_RF, write.forest=T,
                 importance="impurity", seed=CNST_SEED, probability=F, classification=F, 
                 num.threads=CNST_CALC_THREAD_RANGER, verbose=T, min.node.size=minNodeSize)
  )["elapsed"] 
}

summary(rf)


##################
### 変数重要度 ###
##################

# ranger::importance()関数
# 加工(convert VarImp)
varImp_RF <- convertVarImp_rangerRF(rf)

# output csv(varImp_RF)
if(T){
  no <- c(1:dim(varImp_RF)[1])
  varImp_RF <- cbind(no, varImp_RF, stringsAsFactors=F)
  outFileName <- paste(statisticalMethodNm, "_varImp_", GetDateTime_YYYYMMDD_HHMMSS(), ".csv", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(varImp_RF, outFilePath, row.names=F)
}


###################
### plot varImp ###
###################

# set
df <- varImp_RF
df[,"value2"] <- paste0(format(round(df[,"value"]*100, 1), nsmall=1), "%")

# plotVarImp_topN (topN=30)
if(T){
  topN <- min(30, nrow(df)) # 適宜変更
  titleLabel_varImp <- paste("変数重要度 (", str_a0, ", 上位", topN, "項目)", sep="")
  outFileName <- paste(statisticalMethodNm, "_varImp_topN=", topN, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  plotVarImp_topN(topN, df, outFilePath, titleLabel_varImp)
  
  # set
  lstOutFilePath[["varImp_topN_30"]] <- outFilePath
}


# plotVarImp_topN (topN=20)
if(T){
  topN <- min(20, nrow(df)) # 適宜変更
  titleLabel_varImp <- paste("変数重要度 (", str_a0, ", 上位", topN, "項目)", sep="")
  outFileName <- paste(statisticalMethodNm, "_varImp_topN=", topN, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  plotVarImp_topN(topN, df, outFilePath, titleLabel_varImp)
  
  # set
  lstOutFilePath[["varImp_topN_20"]] <- outFilePath
}


# plotVarImp_topN (topN=10)
if(T){
  topN <- min(10, nrow(df)) # 適宜変更
  titleLabel_varImp <- paste("変数重要度 (", str_a0, ", 上位", topN, "項目)", sep="")
  outFileName <- paste(statisticalMethodNm, "_varImp_topN=", topN, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  plotVarImp_topN(topN, df, outFilePath, titleLabel_varImp)
  
  # set
  lstOutFilePath[["varImp_topN_10"]] <- outFilePath
}


# plotVarImp_topN (topN=5)
if(T){
  topN <- min(5, nrow(df)) # 適宜変更
  titleLabel_varImp <- paste("変数重要度 (", str_a0, ", 上位", topN, "項目)", sep="")
  outFileName <- paste(statisticalMethodNm, "_varImp_topN=", topN, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  plotVarImp_topN(topN, df, outFilePath, titleLabel_varImp)
  
  # set
  lstOutFilePath[["varImp_topN_5"]] <- outFilePath
}



################
### 予測結果 ###
################

# datTrain
pred_train <- predict(rf, datTrain, predict.all=F, type="response", 
                      seed=CNST_SEED, num.threads=CNST_CALC_THREAD_RANGER, verbose=T)$predictions
# datValid
pred_valid <- predict(rf, datValid, predict.all=F, type="response", 
                      seed=CNST_SEED, num.threads=CNST_CALC_THREAD_RANGER, verbose=T)$predictions
# datTest
pred_test <- predict(rf, datTest, predict.all=F, type="response", 
                      seed=CNST_SEED, num.threads=CNST_CALC_THREAD_RANGER, verbose=T)$predictions

# set
varname_NEW <- paste0(targetName, "_pred")
datTrain_ext[, varname_NEW] <- pred_train
datValid_ext[, varname_NEW] <- pred_valid
datTest_ext[, varname_NEW] <- pred_test


############
### RMSE ###
############

if(T){
  # init
  RMSE <- list()
  # roundDigit
  if(T){
    roundDigit <- 2
    #roundDigit <- 3
    #roundDigit <- 4
  }
  RMSE[["train"]] <- round(sqrt(mean((pred_train - datTrain[,targetName])^2)), roundDigit)
  RMSE[["valid"]] <- round(sqrt(mean((pred_valid - datValid[,targetName])^2)), roundDigit)
  RMSE[["absErr"]] <- round(abs(RMSE[["train"]] - RMSE[["valid"]]), roundDigit)
  RMSE[["relErr"]] <- round(RMSE[["absErr"]] / RMSE[["train"]], roundDigit)
}


# output csv(RMSEInfo)
if(T){
  RMSEInfo <- data.frame("RMSE_train"=RMSE[["train"]], "RMSE_valid"=RMSE[["valid"]], 
                         "RMSE_absErr"=RMSE[["absErr"]], "RMSE_relErr"=RMSE[["relErr"]])
  outFileName <- paste("RMSE_Summary_", statisticalMethodNm, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".csv", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(RMSEInfo, outFilePath, row.names=F)
  print(RMSEInfo)
  
  # set
  lstPrint[["RMSEInfo"]] <- RMSEInfo
}


######################
### 予測結果の出力 ###
######################

# output csv (df_submission)
if(T){
  df_submission <- data.frame("Id"=datTest_org[,"Id"], 
                              "targetName"=datTest_ext[,paste0(targetName, "_pred")])
  colnames(df_submission) <- c("Id", targetName)
  table(df_submission[,targetName])
  
  outFileName <- paste0("output_", statisticalMethodNm, "_[", targetName, "]_", GetDateTime_YYYYMMDD_HHMMSS(), ".csv")
  outFilePath <- paste(inputDir, outFileName, sep="")
  #outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(df_submission, outFilePath, row.names=F, quote=F)
}


#=== [END]:R-Script ===

print(RMSEInfo)
head(cbind(datTrain[,targetName], pred_train))
head(df_submission)
