# InitSettings_HousingPrice.R
#
# @date  : 2018/09/25(Tue.)
# @brief : HousingPriceデータの読込・加工


################
### Settings ###
################

# target variable (目的変数)
targetName <- "SalePrice"

# isUseVariablesName
isUseVariablesName <- "isuse_Tree"

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test.csv"    # 予測用データ
  lstInputFileNmList[["colList"]] <- "colList_train_HousingPrice.csv" # 説明変数候補リスト
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
  
  # 目的変数：数値化
  #if(!is.numeric(datTrain[,targetName])){
    datTrain[,targetName] <- as.numeric(datTrain[,targetName])
  #}
  
  if(T){
    fillFlg_m01 <- datTrain[,"SalePrice"] > 250*10^3
    datTrain[,"SalePrice_m01_large"] <- 0
    datTrain[fillFlg_m01,"SalePrice_m01_large"] <- 1
  }
  if(T){
    fillFlg_m02 <- (datTrain[,"SalePrice"] > 150*10^3) & (datTrain[,"SalePrice"] <= 250*10^3)
    datTrain[,"SalePrice_m02_medium"] <- 0
    datTrain[fillFlg_m02,"SalePrice_m02_medium"] <- 1
  }
  if(T){
    fillFlg_m03 <- (datTrain[,"SalePrice"] <= 150*10^3)
    datTrain[,"SalePrice_m03_small"] <- 0
    datTrain[fillFlg_m03,"SalePrice_m03_small"] <- 1
  }
  if(T){
    datTrain$SalePriceScale <- NA
    datTrain[fillFlg_m01,"SalePriceScale"] <- 1
    datTrain[fillFlg_m02,"SalePriceScale"] <- 2
    datTrain[fillFlg_m03,"SalePriceScale"] <- 3
  }
}


# load data (datTest)
if(T){
  inputFileName <- lstInputFileNmList[["test"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  datTest_org <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  # init
  datTest <- datTest_org
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
  n <- length(targetVariablesList)
}

#str_a0 <- paste(n, "変数", sep="")
str_a0 <- paste(n, "var", sep="")
str_a <- paste(targetVariablesList, sep="", collapse=" | ")
str_a <- paste(str_a0, "_", str_a, sep="")
print(str_a0)


### 数値変数の欠損の補完 ###

complementVarList <- c("LotFrontage")
num_complementVarList <- length(complementVarList)

### datTrain ###
for(l in c(1:num_complementVarList)){
  varname <- complementVarList[l]
  med <- as.numeric(quantile(datTrain[,varname], probs=0.5, na.rm=T))
  fillFlg_NA <- is.na(datTrain[,varname])
  datTrain[fillFlg_NA,varname] <- med
}

### datTest ###
for(l in c(1:num_complementVarList)){
  varname <- complementVarList[l]
  med <- as.numeric(quantile(datTest[,varname], probs=0.5, na.rm=T))
  fillFlg_NA <- is.na(datTest[,varname])
  datTest[fillFlg_NA,varname] <- med
}


complementVarList_cat <- c("BsmtFinSF1","TotalBsmtSF","GarageCars","GarageArea",
                           "MasVnrType","MasVnrArea","BsmtQual","BsmtCond", "BsmtExposure",
                           "BsmtFinType1","BsmtFinType2","GarageType","GarageYrBlt",
                           "GarageFinish", "GarageQual","GarageCond","MiscFeature",
                           "MSZoning","Exterior1st","Exterior2nd","BsmtFinSF2","BsmtUnfSF",
                           "BsmtFullBath","BsmtHalfBath","KitchenQual","SaleType")
num_complementVarList_cat <- length(complementVarList_cat)

### datTrain ###
for(l in c(1:num_complementVarList_cat))
{
  varname <- complementVarList_cat[l]
  Freq_l <- table(datTrain[,varname]) %>% as.data.frame.table() %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()
  fillFlg_NA <- is.na(datTrain[,varname])
  datTrain[fillFlg_NA,varname] <- Freq_l[1,"Freq"]
}

### datTest ###
for(l in c(1:num_complementVarList_cat)){
  varname <- complementVarList_cat[l]
  Freq_l <- table(datTest[,varname]) %>% as.data.frame.table() %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()
  fillFlg_NA <- is.na(datTest[,varname])
  datTest[fillFlg_NA,varname] <- Freq_l[1,"Freq"]
}

# 説明変数:factor
if(T){
  for(l in (1:n)){
    varname <- targetVariablesList[l]
    if(varname %in% colnames(datTrain)){
      if(is.character(datTrain[,varname])){
        datTrain[,varname] <- as.factor(datTrain[,varname])
      }
    }
  }
}

# 説明変数:factor
if(T){
  for(l in (1:n)){
    varname <- targetVariablesList[l]
    if(varname %in% colnames(datTest)){
      if(is.character(datTest[,varname])){
        datTest[,varname] <- as.factor(datTest[,varname])
      }
    }
  }
}

# filter explanatory variables
#datTrain <- datTrain[,c(targetName, targetVariablesList)]
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


