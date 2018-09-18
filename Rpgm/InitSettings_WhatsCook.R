# InitSettings_WhatsCook.R
#
# @date  : 2018/09/18(Tue.)
# @brief : What's Cookデータの読込・加工


################
### Settings ###
################

# target variable (目的変数)
if(F){
  targetNameList <- c("m01_italian", "m02_mexican", "m03_southern_us", "m04_indian", "m05_chinese",
                      "m06_french", "m07_cajun_creole", "m08_thai", "m09_japanese", "m10_greek",
                      "m11_spanish", "m12_korean", "m13_vietnamese", "m14_moroccan", "m15_british",
                      "m16_filipino", "m17_irish", "m18_jamaican", "m19_russian", "m20_brazilian")
  targetName <- targetNameList[9]
}


# isUseVariablesName
isUseVariablesName <- "isuse_Tree"

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train_ncolMax_65_forAnalysis_topN_300.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test_ncolMax_65_forAnalysis_topN_300.csv"    # 予測用データ
  lstInputFileNmList[["colList"]] <- "colList_train_WhatsCook.csv" # 説明変数候補リスト
}


#################
### load data ###
#################

# load data (datTrain)
if(T){
  inputFileName <- lstInputFileNmList[["train"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  datTrain_org <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  # init
  datTrain <- datTrain_org
}

# load data (datTest)
if(T){
  inputFileName <- lstInputFileNmList[["test"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  datTest_org <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  # init
  datTest <- datTest_org
}

# 目的変数：ファクター化
if(!is.factor(datTrain[,targetName])){
  datTrain[,targetName] <- as.factor(datTrain[,targetName])
  if(exists("responseLabel")) {
    levels(datTrain[,targetName]) <- responseLabel
  }
}


##########################
### 説明変数候補リスト ###
##########################

# explanatory variable candidates list (説明変数候補リスト)
if(T){
  inFileName_colList <- lstInputFileNmList[["colList"]]
  inFilePath <- paste(inputDir, inFileName_colList, sep="")
  colListOrg <- read.csv(inFilePath, header=T, stringsAsFactors=F)
  fillFlg <- (colListOrg[,isUseVariablesName]==1)
  targetVariablesList <- colListOrg[fillFlg,"varname"]
  if(T) {
    targetVariablesList <- gsub(" ", ".", targetVariablesList)
    targetVariablesList <- gsub("-", ".", targetVariablesList)
    targetVariablesList <- gsub("&", ".", targetVariablesList)
  }
  n <- length(targetVariablesList)
}

#str_a0 <- paste(n, "変数", sep="")
str_a0 <- paste(n, "var", sep="")
str_a <- paste(targetVariablesList, sep="", collapse=" | ")
str_a <- paste(str_a0, "_", str_a, sep="")
print(str_a0)


# 説明変数:factor
if(T){
  for(l in (1:n)){
    varname <- targetVariablesList[l]
    if(varname %in% colnames(datTrain)){
      #if(is.character(datTrain[,varname])){
      datTrain[,varname] <- as.factor(datTrain[,varname])
      #}
    }
  }
}

# 説明変数:factor
if(T){
  for(l in (1:n)){
    varname <- targetVariablesList[l]
    if(varname %in% colnames(datTest)){
      #if(is.character(datTest[,varname])){
      datTest[,varname] <- as.factor(datTest[,varname])
      #}
    }
  }
}

# filter explanatory variables
datTrain <- datTrain[,c(targetName, targetVariablesList)]
dim(datTrain)

datTest <- datTest[,c(targetVariablesList)]
# init
datTest[,targetName] <- NA


# 目的変数:NAを除外
if(T){
  fillFlg_NotNA <- !is.na(datTrain[,targetName])
  datTrain <- datTrain[fillFlg_NotNA,]
}

#=== [END]:R-Script ===