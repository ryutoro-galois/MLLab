# 20180918_TreeModel_WhatsCook.R
#
# @date : 2018/09/18(Tue.)
# @note : 決定木モデル(事前剪定; Pre-Pruning)

# install.packages
if(F){
  install.packages("rpart", dependencies = T)
  install.packages("partykit", dependencies = T)
  install.packages("rpart.plot", dependencies = T)
  install.packages("ROCR", dependencies = T)
  install.packages("caret", dependencies = T)
}

# load libraries
library(rpart)
library(partykit)
library(rpart.plot)
library(ROCR)
library(caret)

# statisticalMethodNm
statisticalMethodNm <- "TreeModel"


# T=事前剪定(Pre-Pruning; 最大階層数などで停止条件を規定する場合)
# F=事後剪定(Post-Pruning; 1se-rule等を利用して剪定を行う場合)
isPrePruing <- T


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
  outputDir <- paste("Rout/rpart/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")
  source("Rpgm/TreeSettings.R", encoding="utf-8")
  source("Rpgm/InitSettings_WhatsCook.R", encoding="utf-8")
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

if(T){
  # set
  dat <- datTrain[,c(targetName, targetVariablesList)]
  
  # formula
  formulaStr <- formula(paste(targetName,"~."))
  
  if(MinBucketTypeID == 2) {
    MinBucket <- round(nrow(dat)*MinBucketRate, 0) # 上書き
  }
  
  if(!isusePrior){
    model <- rpart(formulaStr, dat, method="class", parms=list(split=Split),
                   control=rpart.control(minbucket=MinBucket, cp=Cp, maxdepth=MaxDepth, xval=10))
  } else {
    model <- rpart(formulaStr, dat, method="class", parms=list(prior=PriorProbByManual, split=Split),
                   control=rpart.control(minbucket=MinBucket, cp=Cp, maxdepth=MaxDepth, xval=10))
  }
  
  ################################
  ### plot Tree Result(樹形図) ###
  ################################
  
  if(T)
  {
    CNST_WIDTH <- 1500
    CNST_HEIGHT <- CNST_WIDTH * 0.6
    CNST_FONT_SIZE <- 24
    
    if(MinBucketTypeID == 1){
      strTreeCondition <- paste0(str_a0, "_maxDepth_", MaxDepth, "_MinBucket_", MinBucket)
    } else if(MinBucketTypeID == 2) {
      strTreeCondition <- paste0(str_a0, "_maxDepth_", MaxDepth, "_MinBucketRate_", strMinBucketRate)
    }
    
    # titleLabel
    if(T){
      nSample_fmt <- format(nrow(datTrain), big.mark=",")
      
      titleLabel <- paste("\n[", targetName, "]～All (", str_a0, ") nSample=", nSample_fmt, sep="")
      # 階層数
      titleLabel <- paste(titleLabel, ",\n", MaxDepth, "depth", sep="")
      # リーフ数(=終端ノード数)
      numOfLeafsInTree <- length(nodeids(as.party(model), terminal=T))
      titleLabel <- paste(titleLabel, ", Terminal Nodes=", numOfLeafsInTree, "\n", sep="")
      
      # use later
      titleLabel_partykit <- titleLabel
    }
    outFileName <- paste("tree_rpart.plot_[", targetName, "]_", strTreeCondition, ".png", sep="")
    outFilePath <- paste(outputDir, outFileName, sep="")
    
    # set
    lstOutFilePath[["rpart.plot"]] <- outFilePath
    
    # rpart.plot
    if(T){
      plotTree_rpart.plot_png(model=model, outFilePath=outFilePath, titleLabel=titleLabel, 
                              width=CNST_WIDTH, height=CNST_HEIGHT, fontsize=CNST_FONT_SIZE)
    }
    
  }
  # ===[END]:plotTree_rpart.plot_png===
  
  
  ##################
  ### 変数重要度 ###
  ##################
  
  if(T){
    df0 <- varImp.rpart(model)
    df <- df0[,c("varname", "ratio")]
    colnames(df) <- c("id", "value")
    
    # with RankID
    strID <- round(rank(-df[,"value"]), 0)
    strID <- paste("0",strID, sep="")
    strID <- substr(strID, nchar(strID)-1, nchar(strID))
    df[,"id"] <- paste(strID, " ", df[,"id"], sep="")
    
    df$id <- factor(df$id, levels=df$id[order(df$value)])
    
    # value2
    df[,"value2"] <- paste0(format(round(df[,"value"]*100,1), nsmall=1),"%")
    if(T){
      #topN <- 10
      topN <- 20
      #topN <- 30
    }
    df <- df[c(1:topN),] 
  }
  
  # output png (plot varImp)
  if(T){
    CNST_WIDTH <- 800
    CNST_HEIGHT <- CNST_WIDTH * 0.9
    CNST_FONT_SIZE <- 16
    
    # titleLabel
    #titleLabel_varImp <- titleLabel_partykit
    titleLabel_varImp <- paste0(titleLabel_partykit, " topN=", topN, "var")
    
    # outFilePath
    outFileName <- paste("tree_varImp_[", targetName, "]_", strTreeCondition, ".png", sep="")
    outFilePath <- paste(outputDir, outFileName, sep="")
    
    # set
    lstOutFilePath[["varImp"]] <- outFilePath
    
    plotVarImp(df=df, titleLabel=titleLabel_varImp, outFilePath=outFilePath, 
               width=CNST_WIDTH, height=CNST_HEIGHT, fontsize=CNST_FONT_SIZE, face="plain", fill="gray", hjust=0.5)
    
  }
  # ===[END]:変数重要度===
  
  
  ########################
  ### 決定木ルール抽出 ###
  ########################
  
  if(T){
    # 各ノードへの分岐条件取得
    leafRules <- getLeafRules.rpart(model, isReplaceNodeIDInPartykit=F)
    rownames(leafRules) <- NULL
    # sort by lift desc
    if(T) {
      fillFlg_order <- order(leafRules[,"lift"], decreasing=T)
      leafRules <- leafRules[fillFlg_order,]
      leafRules$no <- c(1:nrow(leafRules))
    }
    
    # round
    if(T){
      leafRules[,"ratio"] <- round(leafRules[,"ratio"],3)
      leafRules[,"prob"] <- round(leafRules[,"prob"],3)
      leafRules[,"rootProb"] <- round(leafRules[,"rootProb"],3)
      leafRules[,"lift"] <- round(leafRules[,"lift"],3)
    }
    
    # set
    lstPrint[["leafRules"]] <- leafRules
    
    # output csv (leafRules_All)
    if(T){
      outFileName <- paste("tree_leafRules_rpart_[", targetName, "]_", strTreeCondition, ".csv", sep="")
      outFilePath <- paste(outputDir, outFileName, sep="")
      write.csv(leafRules, outFilePath, row.names=F)
    }
  }
  # ===[END]:決定木ルール抽出===
  
  
  # 予測生存確率の算出 (predProb)
  if(T){
    varname_NEW <- "predProb"
    datTrain_ext[, varname_NEW] <- as.numeric(predict(model, dat, method="prob")[,2])
    datValid_ext[, varname_NEW] <- as.numeric(predict(model, datValid, method="prob")[,2])
    datTest_ext[, varname_NEW] <- as.numeric(predict(model, datTest, method="prob")[,2])
  }
  
  # 予測生存有無 (Survived_pred)
  if(T){
    if(T){
      cutoff <- 0.5
      #cutoff <- 0.6
      #cutoff <- 0.7
    }
    varname_NEW <- paste0(targetName, "_pred")
    datTrain_ext[, varname_NEW] <- ifelse(datTrain_ext[,"predProb"]>=cutoff, 1, 0)
    datValid_ext[, varname_NEW] <- ifelse(datValid_ext[,"predProb"]>=cutoff, 1, 0)
    datTest_ext[, varname_NEW] <- ifelse(datTest_ext[,"predProb"]>=cutoff, 1, 0)
  }
  
}


#####################################
### モデル評価 (混同行列・正解率) ###
#####################################

if(T){
  
  # 混同行列 (confusionMatrix)
  if(T){
    pred_Train <- datTrain_ext[,paste0(targetName, "_pred")]
    pred_Train <- as.factor(pred_Train)
    levels(pred_Train) <- responseLabel
    ConfMat_train <- caret::confusionMatrix(pred_Train, datTrain_ext[,targetName]) 
  }
  
  if(T){
    pred_Valid <- datValid_ext[,paste0(targetName, "_pred")]
    pred_Valid <- as.factor(pred_Valid)
    levels(pred_Valid) <- responseLabel
    ConfMat_vaild <- caret::confusionMatrix(pred_Valid, datValid_ext[,targetName]) 
  }
  
  # 正解率 (accuracy)
  if(T){
    Accuracy_train <- ConfMat_train$overall["Accuracy"]
    Accuracy_valid <- ConfMat_vaild$overall["Accuracy"]
    #Accuracy_train <- sum(diag(ConfMat_train)) / sum(ConfMat_train)
    #Accuracy_valid <- sum(diag(ConfMat_vaild)) / sum(ConfMat_vaild)
  }
  
  AccuracyInfo <- data.frame("Accuracy_train"=Accuracy_train, "Accuracy_valid"=Accuracy_valid,
                             "Accuracy_diff"=Accuracy_train-Accuracy_valid)
  AccuracyInfo <- round(AccuracyInfo, 4)
  
  # set
  lstPrint[["ConfMat_train"]] <- ConfMat_train$table
  lstPrint[["ConfMat_vaild"]] <- ConfMat_vaild$table
  lstPrint[["AccuracyInfo"]] <- AccuracyInfo
}


#############################
### モデル評価 (ROCCurve) ###
#############################

if(T){
  ROCInfo_train <- CalcROCInfo_ROCR(datTrain_ext[,"predProb"], datTrain_ext[,targetName])
  ROCInfo_valid <- CalcROCInfo_ROCR(datValid_ext[,"predProb"], datValid_ext[,targetName])
  
  AUC_train <- ROCInfo_train[[1]]
  AUC_valid <- ROCInfo_valid[[1]]
  AUC_diff <- AUC_train - AUC_valid
  
  ROCTbl_train <- ROCInfo_train[[2]]
  ROCTbl_valid <- ROCInfo_valid[[2]]
  
  ###############################
  ### plot ROC Curve (ggplot) ###
  ###############################
  
  if(T){
    if(T){
      mainTitle <- paste("【ROC Curve】\n", "AUC(Train)=",round(AUC_train,3), ",  AUC(Valid)=",round(AUC_valid,3), sep="")
      subTitle <- NULL
      colourList=c("black","black","black")
      CNST_WIDTH <- 600
      CNST_HEIGHT <- CNST_WIDTH * 0.8
      CNST_FONT_SIZE <- 16
    }
    
    outFileName <- paste("ROCCurve_[", targetName, "]_trainDataRatio_", trainDataRatio, ".png", sep="")
    outFilePath <- paste(outputDir, outFileName, sep="")
    
    # set
    lstOutFilePath[["ROCCurve"]] <- outFilePath
    
    PlotROCCurve(outFilePath=outFilePath, 
                 ROCTbl_train=ROCTbl_train, ROCTbl_valid=ROCTbl_valid, 
                 mainTitle=mainTitle, subTitle=subTitle, colourList=colourList, 
                 width=CNST_WIDTH, height=CNST_HEIGHT, fontsize=CNST_FONT_SIZE, AUC=AUC_valid,
                 legendList=c("Train","Valid"))
  }
}

print(round(cbind(trainDataRatio, AUC_train, AUC_valid, AUC_diff), 3))



######################
### 予測結果の出力 ###
######################

# output csv (df_submission)
if(T){
  df_submission <- data.frame("id"=datTest_org[,"id"], 
                              "targetName"=datTest_ext[,paste0(targetName, "_pred")])
  colnames(df_submission) <- c("id", targetName)
  table(df_submission[,targetName])
  
  outFileName <- paste0("output_[", targetName, "]_cutoff_", cutoff, ".csv")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(df_submission, outFilePath, row.names=F, quote=F)
}


#=== [END]:R-Script ===
