# InitSettings_Cook_03.R
#
# @date  : 2018/10/09(Tue.)
# @brief : What's Cookデータの読込・加工


################
### Settings ###
################

# target variable (目的変数)
if(T){
  targetNameList <- c("m01_italian", "m02_mexican", "m03_southern_us", "m04_indian", "m05_chinese",
                      "m06_french", "m07_cajun_creole", "m08_thai", "m09_japanese", "m10_greek",
                      "m11_spanish", "m12_korean", "m13_vietnamese", "m14_moroccan", "m15_british",
                      "m16_filipino", "m17_irish", "m18_jamaican", "m19_russian", "m20_brazilian")
  
  num_objective_class <- length(targetNameList)
  
  if(T){
    targetNameList_map <- data.frame("cuisine"=substr(targetNameList, 5, nchar(targetNameList)), 
                                     "m_cuisine"=targetNameList, stringsAsFactors=F) 
  }
  
  targetName <- "m_cuisine"
}


# isUseVariablesName
isUseVariablesName <- "isuse_Tree"

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train_ncolMax_65_forAnalysis_topN_300.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test_ncolMax_65_forAnalysis_topN_300.csv"    # 予測用データ
  lstInputFileNmList[["colList"]] <- "colList_train_ncolMax_65_topN_300.csv" # 説明変数候補リスト
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
  # merge
  datTrain <- dplyr::left_join(datTrain, targetNameList_map, by=c("cuisine")) %>% as.data.frame()
  colnames(datTrain)
}

# load data (datTest)
if(T){
  inputFileName <- lstInputFileNmList[["test"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  datTest_org <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  # init
  datTest <- datTest_org
}

# ingredients_str
if(T){
  datTrain[,"ingredients_str"] <- gsub(" ", ".", datTrain[,"ingredients_str"])
  datTrain[,"ingredients_str"] <- gsub("|"," ", datTrain[,"ingredients_str"])
}
if(T){
  datTest[,"ingredients_str"] <- gsub(" ", ".", datTest[,"ingredients_str"])
  datTest[,"ingredients_str"] <- gsub("|"," ", datTest[,"ingredients_str"])
}

# filter explanatory variables
datTrain <- datTrain[,c(targetName, "ingredients_str")]
dim(datTrain)

# init
datTest[,targetName] <- NA
datTest <- datTest[,c(targetName, "ingredients_str")]


# 目的変数:NAを除外
if(T){
  fillFlg_NotNA <- !is.na(datTrain[,targetName])
  datTrain <- datTrain[fillFlg_NotNA,]
}

#=== [END]:R-Script ===