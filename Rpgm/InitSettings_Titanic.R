# InitSettings_Titanic.R
#
# @date  : 2018/09/06
# @brief : Titanicデータの読込・加工


################
### Settings ###
################

# target variable (目的変数)
targetName <- "Survived"

# isUseVariablesName
isUseVariablesName <- "isuse_Tree"

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test.csv"    # 予測用データ
  lstInputFileNmList[["colList"]] <- "colList_titanic.csv" # 説明変数候補リスト
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


##########################
### データの補完・加工 ###
##########################

### datTrain ###
if(T){
  
  # Ageの補完
  if(T){
    Age_median <- as.numeric(quantile(datTrain[,"Age"], probs=0.5, na.rm=T))
    #Age_median <- summary(datTrain[,"Age"])["Median"]
    fillFlg_NA <- is.na(datTrain[,"Age"])
    datTrain[fillFlg_NA,"Age"] <- Age_median
  }
  
  # Embarkedの補完
  if(T){
    fillFlg_NA <- datTrain[,"Embarked"] %in% c("")
    datTrain[fillFlg_NA,"Embarked"] <- "S"
  }
  
  # Fareの補完
  if(T){
    Age_median <- as.numeric(quantile(datTrain[,"Fare"], probs=0.5, na.rm=T))
    #Age_median <- summary(datTrain[,"Fare"])["Median"]
    fillFlg_NA <- is.na(datTrain[,"Fare"])
    datTrain[fillFlg_NA,"Fare"] <- Age_median
  }
  
  # Cabin
  if(T){
    datTrain[,"Cabin"] <- substr(datTrain[,"Cabin"],1,1)
    table(datTrain[,"Cabin"])
  }
  
  # Parch
  if(T){
    datTrain[,"Parch"] <- ifelse(datTrain[,"Parch"]>6,6,datTrain[,"Parch"])
  }
  
  # as.factor
  if(T){
    datTrain[,"Pclass"] <- as.factor(datTrain[,"Pclass"])
    datTrain[,"Sex"] <- as.factor(datTrain[,"Sex"])
    datTrain[,"SibSp"] <- as.factor(datTrain[,"SibSp"])
    datTrain[,"Parch"] <- as.factor(datTrain[,"Parch"])
    datTrain[,"Cabin"] <- as.factor(datTrain[,"Cabin"])
    datTrain[,"Embarked"] <- as.factor(datTrain[,"Embarked"])
  }
}


### datTest ###
if(T){
  
  # Ageの補完
  if(T){
    Age_median <- as.numeric(quantile(datTest[,"Age"], probs=0.5, na.rm=T))
    #Age_median <- summary(datTest[,"Age"])["Median"]
    fillFlg_NA <- is.na(datTest[,"Age"])
    datTest[fillFlg_NA,"Age"] <- Age_median
  }
  
  # Embarkedの補完
  if(T){
    fillFlg_NA <- datTest[,"Embarked"] %in% c("")
    datTest[fillFlg_NA,"Embarked"] <- "S"
  }
  
  # Fareの補完
  if(T){
    Age_median <- as.numeric(quantile(datTest[,"Fare"], probs=0.5, na.rm=T))
    #Age_median <- summary(datTest[,"Fare"])["Median"]
    fillFlg_NA <- is.na(datTest[,"Fare"])
    datTest[fillFlg_NA,"Fare"] <- Age_median
  }
  
  # Cabin
  if(T){
    datTest[,"Cabin"] <- substr(datTest[,"Cabin"],1,1)
    table(datTest[,"Cabin"])
  }
  
  # Parch
  if(T){
    datTest[,"Parch"] <- ifelse(datTest[,"Parch"]>6,6,datTest[,"Parch"])
  }
  
  # as.factor
  if(T){
    datTest[,"Pclass"] <- as.factor(datTest[,"Pclass"])
    datTest[,"Sex"] <- as.factor(datTest[,"Sex"])
    datTest[,"SibSp"] <- as.factor(datTest[,"SibSp"])
    datTest[,"Parch"] <- as.factor(datTest[,"Parch"])
    datTest[,"Cabin"] <- as.factor(datTest[,"Cabin"])
    datTest[,"Embarked"] <- as.factor(datTest[,"Embarked"])
  }
}

#=== [END]:データの補完・加工 ===


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
  n <- length(targetVariablesList)
}

#str_a0 <- paste(n, "変数", sep="")
str_a0 <- paste(n, "var", sep="")
str_a <- paste(targetVariablesList, sep="", collapse=" | ")
str_a <- paste(str_a0, "_", str_a, sep="")
print(str_a)


# 説明変数:char=>factor
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

# 説明変数:char=>factor
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
