# 20180828_Tree_rpart_2var_titanic.R
#
# @date : 2018/08/28(Tue.)
# @note : 2変数決定木(rpart)
#

# load libraries
library(rpart)
library(ROCR)
library(partykit)
library(rpart.plot)


if(!exists("batchFlg_renderHTML"))
{
  # set working directory
  target_wd_ID <- 1 # 適宜変更
  targetWDList <- list()
  targetWDList[[1]] <- "" # 適宜変更
  targetWDList[[2]] <- "" # 適宜変更
  setwd(targetWDList[[target_wd_ID]])
  print(getwd())
  
  # input/output directory
  inputDir <- paste("Rin/", sep="")
  outputDir <- paste("Rout/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")  
}


################
### Settings ###
################

# target variable
targetName <- "Survived"

# isUseVariablesName
isUseVariablesName <- "isuse_Tree"

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["submit"]] <- "gender_submission.csv"
  lstInputFileNmList[["train"]] <- "train.csv"
  lstInputFileNmList[["test"]] <- "test.csv"
  lstInputFileNmList[["colList"]] <- "colList_titanic.csv"
}



# 応答値ラベル名
if(T){
  responseLabel <- c("No", "Yes")
}

# prior
if(T){
  PriorProbByManual <- c(0.5, 0.5)
}

# 事前確率の設定
if(T){
  Prior <- PriorProbByManual
}

lst_png_rpart_2var <- list()


################
### Settings ###
################

# フォントサイズ
CNST_FONT_SIZE <- 16

#####################

isOutputCSV <- T
isOutputPNG <- T
isOutputXLS <- T
isOutputPPT <- T
strComment <- NA

#####################


###################
### 決定木param ###
###################

MinBucketTypeID <- 2 # 1=サンプル数, 2=サンプル比率

# MinBucket,MinBucketRate
if(T){
  MinBucket <- NA
  MinBucketRate <- NA
  if(MinBucketTypeID == 1) {
    MinBucket <- 10
  } else if(MinBucketTypeID == 2){
    #MinBucketRate <- 0.1
    MinBucketRate <- 0.05
    #MinBucketRate <- 0.01
    strMinBucketRate <- paste(MinBucketRate*100, "per", sep="")
    strMinBucketRate2 <- paste(MinBucketRate*100, "%", sep="")
  }
}

# 最大階層数
if(T){
  #MaxDepth <- 3
  MaxDepth <- 4
  #MaxDepth <- 5
  #MaxDepth<- 10
}

# Cp
if(T){
  Cp <- -1
  #Cp <- 0.01
}

# Split
Split <- "gini"

# ===[END]:決定木param===


# load data (datSample)
if(T){
  inputFileName <- lstInputFileNmList[["train"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  datSample <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
}

datSample[,"PassengerId"] <- as.factor(datSample[,"PassengerId"])
datSample[,"Pclass"] <- as.factor(datSample[,"Pclass"])
datSample[,"Name"] <- as.factor(datSample[,"Name"])
datSample[,"Sex"] <- as.factor(datSample[,"Sex"])
datSample[,"SibSp"] <- as.factor(datSample[,"SibSp"])
datSample[,"Parch"] <- as.factor(datSample[,"Parch"])
datSample[,"Ticket"] <- as.factor(datSample[,"Ticket"])
datSample[,"Cabin"] <- substr(datSample[,"Cabin"], 1, 1)
#datSample[,"Cabin"] <- substr(datSample[,"Cabin"], 1, 2)
datSample[,"Cabin"] <- as.factor(datSample[,"Cabin"])
datSample[,"Embarked"] <- as.factor(datSample[,"Embarked"])


table(datSample[(datSample[,"Survived"]==1),c("Embarked","Cabin")])

# 全体応答率
responseRatioTotal <- sum(datSample[,targetName]==1) / nrow(datSample)
responseRatioTotal_fmt <- paste(round(responseRatioTotal*100, 2), "%", sep="")

# 目的変数：ファクター化
if(T){
  if(!is.factor(datSample[,targetName])){
    datSample[,targetName] <- as.factor(datSample[,targetName])
  }
}
levels(datSample[,targetName]) <- responseLabel



###################
### filter data ###
###################

# 目的変数:NAを除外
if(T){
  fillFlg_NotNA <- !is.na(datSample[,targetName])
  datSample <- datSample[fillFlg_NotNA,]
}

# 分析対象サンプル数
nSample_fmt <- format(nrow(datSample), big.mark=",")
print(nSample_fmt)

# [END]:filter data



# 目的変数：ファクター化
if(!is.factor(datSample[,targetName])){
  datSample[,targetName] <- as.factor(datSample[,targetName])
}

# explanatory variable candidates list (説明変数候補リスト)
if(T){
  inFileName_colList <- lstInputFileNmList[["colList"]]
  inFilePath <- paste(inputDir, inFileName_colList, sep="")
  colListOrg <- read.csv(inFilePath, header=T, stringsAsFactors=F)
  fillFlg <- (colListOrg[,isUseVariablesName]==1)
  targetVariablesList <- colListOrg[fillFlg,"varname"]
  n <- length(targetVariablesList)
}


############################
### 説明変数リストの作成 ###
############################

# 任意の組み合わせ
if(T){
  matX <- combn(targetVariablesList, 2)
  n_matX <- dim(matX)[2]
  lst_Combin <- list()
  for(i in (1:n_matX)) lst_Combin[[i]] <- matX[,i]
  print(n_matX)
}

############################


# 説明変数:char=>factor
if(T){
  for(l in (1:n)){
    varname <- targetVariablesList[l]
    if(varname %in% colnames(datSample)){
      if(is.character(datSample[,varname])){
        datSample[,varname] <- as.factor(datSample[,varname])
      }
    }
  }
}

# set init
datSample_init <- datSample

# init
leafRules_All <- 0
initFlg <- F

##############
### k-loop ###
##############

#for(k in (1:1))
for(k in (1:n_matX))
{
  # 説明変数(2指標)
  varnameList_k <- lst_Combin[[k]]
  varname1 <- varnameList_k[1]
  varname2 <- varnameList_k[2]
  #strVarname12 <- paste("[", varname1, "]_[", varname2, "]", sep="")
  strVarname12 <- paste(varname1, "_", varname2, "", sep="")
  
  # init
  datSample <- datSample_init[,c(targetName, varnameList_k)]
  
  str_k <- paste("000", k, sep="")
  str_k <- substr(str_k, nchar(str_k)-3, nchar(str_k))
  str_k_label <- paste(varnameList_k, collapse="_")
  str_k_label <- paste("ptn", str_k, "_", str_k_label, sep="")
  
  varnameList_k_kakko <- paste("[", varnameList_k, "]", sep="")
  str_a1 <- paste("ptn", str_k, " : [", targetName, "] ～ ", paste(varnameList_k_kakko, collapse="+"), sep="")
  if(T){
    forPrint_k <- paste("[", str_k, "/", n_matX, "] :  (", varname1, ", ", varname2, ")", sep="")
    print(forPrint_k)
  }
  
  # Tree.rpart
  if(T){
    # formula
    formulaStr <- formula(paste(targetName,"~."))
    
    if(MinBucketTypeID == 2) {
      MinBucket <- round(nrow(datSample)*MinBucketRate, 0) # 上書き
    }
    
    model <- rpart(formulaStr, datSample, method="class", parms=list(split=Split),
                   control=rpart.control(minbucket=MinBucket, cp=Cp, maxdepth=MaxDepth, xval=3))
  }
  
  ################################
  ### plot Tree Result(樹形図) ###
  ################################
  
  if(isOutputPNG == T)
  {
    CNST_WIDTH <- 1500
    CNST_HEIGHT <- CNST_WIDTH * 0.6
    CNST_FONT_SIZE <- 16
    
    # titleLabel
    if(T){
      #titleLabel <- paste("Tree 2var ", str_a1, sep="")
      titleLabel <- str_a1
      # サンプル数
      titleLabel <- paste(titleLabel, ",\n  nSample=", nSample_fmt, sep="")
      # 階層数
      titleLabel <- paste(titleLabel, ",  ", MaxDepth, "depth", sep="")
      # リーフ数(=終端ノード数)
      numOfLeafsInTree <- length(nodeids(as.party(model), terminal=T))
      titleLabel <- paste(titleLabel, ", Terminal Nodes=", numOfLeafsInTree, sep="")
      # 後で利用
      titleLabel_partykit <- titleLabel
    }
    
    if(MinBucketTypeID == 1) {
      outFileName <- paste("tree_2var_rpart.plot_ptn", str_k, "_", strVarname12, "_", MaxDepth, "depth_", MinBucket, ".png", sep="")
    } else if(MinBucketTypeID == 2) {
      outFileName <- paste("tree_2var_rpart.plot_ptn", str_k, "_", strVarname12, "_", MaxDepth, "depth_", strMinBucketRate, ".png", sep="")
    }
    outFilePath <- paste(outputDir, outFileName, sep="")
    # set
    lst_png_rpart_2var[[k]] <- outFilePath
    
    # rpart.plot利用
    if(T){
      plotTree_rpart.plot_png(model=model, outFilePath=outFilePath, titleLabel=titleLabel, 
                              width=CNST_WIDTH, height=CNST_HEIGHT, fontsize=CNST_FONT_SIZE)
    }
  }
  # ===[END]:plotTree_rpart.plot_png===
  
  ########################
  ### 決定木ルール抽出 ###
  ########################
  
  if(T){
    # 各ノードへの分岐条件取得
    leafRules <- getLeafRules.rpart(model, isReplaceNodeIDInPartykit=F)
    
    # output csv (leafRules_All)
    if(F){
      outFileName <- paste("treeRules_rpart_", str_k, "_",  GetDateTime_YYYYMMDD_HHMMSS(), ".csv", sep="")
      outFilePath <- paste(outputDir, outFileName, sep="")
      write.csv(leafRules, outFilePath, row.names=F)
    }
  }
  # ===[END]:決定木ルール抽出===
  
  
  if(T){
    ptnNo <- paste("ptn", str_k, sep="")
    varname1 <- varnameList_k[1]
    varname2 <- varnameList_k[2]
    leafRules_k <- cbind(ptnNo, varname1, varname2, leafRules)
    
    # Append
    if(initFlg == F){
      leafRules_All <- leafRules_k
      initFlg <- T
    } else {
      leafRules_All <- rbind(leafRules_All, leafRules_k, stringsAsFactors=F)
    }
  }
  
}

# output csv (leafRules_All)
if(T){
  autoNo <- c(1:nrow(leafRules_All))
  leafRules_All <- cbind(autoNo, leafRules_All)
  outFileName <- paste("treeRules_rpart_AllPtn_2var_[", targetName, "]_", MaxDepth, "階層_", 
                       GetDateTime_YYYYMMDD_HHMMSS(), ".csv", sep="")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(leafRules_All, outFilePath, row.names=F)
}

# === [END]: R-Script ===

dim(leafRules_All)


num_lst_png_rpart_2var <- length(lst_png_rpart_2var)

