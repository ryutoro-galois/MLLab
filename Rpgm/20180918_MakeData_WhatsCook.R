# 20180918_MakeData_WhatsCook.R
#
# @date  : 2018/09/18(Tue.)
# @brief : 分析データの作成
# - 説明変数：食材フラグ(1~topN_ingredients)
# - 目的変数：各料理ジャンルフラグ(m01_..~m20_..)


# load libraries
library(dplyr)

# set working directory
target_wd_ID <- 1
targetWDList <- list()
targetWDList[[1]] <- "" # 適宜変更
targetWDList[[2]] <- "" # 適宜変更
setwd(targetWDList[[target_wd_ID]])

# 入出力フォルダ
inputDir <- "Rin/"

# is.integer0
is.integer0 <- function(x){ is.integer(x) && length(x) == 0L }

# dataTypeID
if(T){
  ##datasetTypeID <- 1  # 1 or 2
  datasetTypeList <- c("train", "test")
  ##datasetTypeNm <- datasetTypeList[datasetTypeID]
}

if(T){
  #ncolMax <- 30
  #ncolMax <- 40
  #ncolMax <- 50
  ncolMax <- 65
}

strid <- paste0("0",c(1:ncolMax))
strid <- substr(strid, nchar(strid)-1, nchar(strid))
strid <- paste0("x", strid)

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- paste0("train_ncolMax_", ncolMax, ".csv")  # 学習用データ
  lstInputFileNmList[["test"]] <- paste0("test_ncolMax_", ncolMax, ".csv")    # 予測用データ
  lstInputFileNmList[["ingredientsList"]] <- "ingredientsList_WhatsCook.csv"
}

# load ingredientsList
if(T){
  inputFileName <- lstInputFileNmList[["ingredientsList"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  dat_ingredientsList_org <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  
  # init
  dat_ingredientsList <- dat_ingredientsList_org
  print(head(dat_ingredientsList))
  
  # filter
  if(T){
    #topN_ingredients <- 100
    #topN_ingredients <- 200
    topN_ingredients <- 300
  }
  dat_ingredientsList <- dat_ingredientsList[c(1:topN_ingredients),]
  
  ingredientsList <- as.character(dat_ingredientsList[,"ingredients"])
  num_ingredientsList <- length(ingredientsList)
  print(num_ingredientsList)
}


##############
### k-loop ###
##############

for(k in c(1:2))
{
  datasetTypeID <- k 
  datasetTypeNm <- datasetTypeList[datasetTypeID]
  
  # load data
  inputFileName <- lstInputFileNmList[[datasetTypeNm]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  dat <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  
  # init dat_out
  cList_out <- c("autoNo","no","id","cuisine","num_ingredients")
  cList_out <- cList_out[cList_out %in% colnames(dat)]
  dat_out <- dat[,cList_out]
  
  # ingredients_str
  dat_out$ingredients_str <- NULL
  dat_out[,"ingredients_str"] <- sapply((1:nrow(dat)), function(l) paste(dat[l,strid], collapse="|"))
  dat_out[,"ingredients_str"] <- gsub("|.","",dat_out[,"ingredients_str"], fixed=T)
  
  
  for(j in c(1:num_ingredientsList))
  {
    str_j <- paste0("000",j)
    str_j <- substr(str_j, nchar(str_j)-3, nchar(str_j))
    str_j <- paste0("ing", str_j)
    
    ingredients_j <- ingredientsList[j]
    varname_NEW <- paste0(str_j, "_", ingredients_j)
    
    # init
    varx <- rep(0,nrow(dat))
    fillFlg_l <- sapply(1:nrow(dat), function(l) !is.integer0(grep(ingredients_j, dat_out[l,"ingredients_str"])))
    cnt_l <- sum(fillFlg_l)
    cnt_fmt_l <- format(cnt_l, big.mark=",")
    varx[fillFlg_l] <- 1
    
    # add
    dat_out[,varname_NEW] <- varx
    
    if(T){
      forPrint_j <- paste0("[",j,"/",num_ingredientsList,"]: ingredients=[",ingredients_j,"], cnt=", cnt_fmt_l)
      print(forPrint_j)
    }
    
  } # j-loop
  
  
  # make dat_cuisineDef
  if(datasetTypeID == 1)
  {
    dat_cuisineDef <- dat %>% dplyr::group_by(cuisine) %>% 
      dplyr::summarise(cnt=n()) %>% dplyr::arrange(desc(cnt)) %>% as.data.frame()
    
    dat_cuisineDef$total <- sum(dat_cuisineDef[,"cnt"])
    dat_cuisineDef[,"ratio"] <- dat_cuisineDef[,"cnt"]/dat_cuisineDef[,"total"]
    
    strlabel <- paste0("0",c(1:nrow(dat_cuisineDef)))
    strlabel <- substr(strlabel, nchar(strlabel)-1, nchar(strlabel))
    strlabel <- paste0("m", strlabel)
    
    dat_cuisineDef[,"label"] <- paste0(strlabel, "_", dat_cuisineDef[,"cuisine"])
    
    no <- c(1:nrow(dat_cuisineDef))
    dat_cuisineDef <- cbind(no, dat_cuisineDef) 
    
    num_cuisine <- nrow(dat_cuisineDef)
    
    # attach flg
    for(l in c(1:num_cuisine))
    {
      cuisine_l <- dat_cuisineDef[l,"cuisine"]
      varname_NEW <- dat_cuisineDef[l,"label"]
      
      # init
      varx <- rep(0,nrow(dat))
      fillFlg_l <- dat[,"cuisine"] %in% cuisine_l
      cnt_l <- sum(fillFlg_l)
      cnt_fmt_l <- format(cnt_l, big.mark=",")
      varx[fillFlg_l] <- 1
      
      # add
      dat_out[,varname_NEW] <- varx
      
      if(T){
        forPrint_l <- paste0("[",l,"/",num_cuisine,"]: cuisine=[",cuisine_l,"], cnt=", cnt_fmt_l)
        print(forPrint_l)
      }
      
    } # l-loop
  }
  
  
  # output csv (dat_out)
  if(T){
    outFileName0 <- paste0(gsub(".csv","",lstInputFileNmList[[datasetTypeNm]]), "_forAnalysis_topN_", topN_ingredients)
    outFileName <- paste0(outFileName0, ".csv")
    outFileName_forAnalysis <- outFileName
    outFilePath <- paste(inputDir, outFileName, sep="")
    write.csv(dat_out, outFilePath, row.names=F, na=".")
    print(dim(dat_out))
  }
  
  
  # output csv (colList_df)
  if(T){
    colList <- colnames(dat_out)
    num_colList <- length(colList)
    varclass <- unlist(sapply(1:ncol(dat_out), function(l) class(dat_out[,l])[1]))
    colList_df <- data.frame("no"=c(1:num_colList), "varname"=colList, "class"=varclass, 
                             "type"="category", "isuse"=1, "fileName"=outFileName_forAnalysis, stringsAsFactors=F)
    outFileName <- paste0("colList_", outFileName0, ".csv")
    outFilePath <- paste(inputDir, outFileName, sep="")
    write.csv(colList_df, outFilePath, row.names=F, quote=F)
    head(colList_df)
  }
  
} # k-loop


#=== [END]:R-Script ===
