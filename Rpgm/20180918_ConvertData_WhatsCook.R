# 20180918_ConvertData_WhatsCook.R
#
# @date  : 2018/09/18(Tue.)
# @brief : json -> csv

# load libraries
library(dplyr)
library(jsonlite)

# set working directory
target_wd_ID <- 1
targetWDList <- list()
targetWDList[[1]] <- "" # 適宜変更
targetWDList[[2]] <- "" # 適宜変更
setwd(targetWDList[[target_wd_ID]])


# 入出力フォルダ
inputDir <- "Rin/"


# dataTypeID
if(T){
  datasetTypeID <- 1  # 1 or 2
  datasetTypeList <- c("train", "test")
  datasetTypeNm <- datasetTypeList[datasetTypeID]
}

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train.json"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test.json"    # 予測用データ
}

# load data
inputFileName <- lstInputFileNmList[[datasetTypeNm]]
inputFilePath <- paste(inputDir, inputFileName, sep="")
dat <- jsonlite::read_json(inputFilePath)
#str(dat)
print(unlist(dat[1]))
n <- length(dat)
print(n)


if(T){
  #ncolMax <- 30
  #ncolMax <- 40
  #ncolMax <- 50
  ncolMax <- 65
}

resMat <- matrix(NA, nrow=n, ncol=ncolMax) %>% as.data.frame()
strid <- paste0("0",c(1:ncolMax))
strid <- substr(strid, nchar(strid)-1, nchar(strid))
strid <- paste0("x", strid)
colnames(resMat) <- strid
#head(resMat)


##############
### l-loop ###
##############

initFlg <- F
resAll <- 0

for(l in c(1:n))
{
  tmp_l <- dat[[l]]
  
  no <- l
  id <- tmp_l$id
  if(datasetTypeNm %in% "train") {
    cuisine <- tmp_l$cuisine
  }
  ingredients <- tmp_l$ingredients
  num_ingredients <- length(ingredients)
  
  if(num_ingredients < ncolMax){
    resMat[l, c(1:num_ingredients)] <- unlist(ingredients)
  } else {
    resMat[l, c(1:ncolMax)] <- unlist(ingredients)[c(1:ncolMax)]
  }
  
  if(T){
    if(num_ingredients >= ncolMax){
      forPrint_l <- paste0("[",l,"/",n,"], id=[", id, "], num_ingredients=", num_ingredients)
    } else {
      forPrint_l <- paste0("[",l,"/",n,"], id=[", id, "]")
    }
    print(forPrint_l)
  }
  
  res_l <- resMat[l,]
  if(datasetTypeNm %in% "train") {
    res_l <- cbind(no, id, cuisine, num_ingredients, res_l)
  } else if(datasetTypeNm %in% "test"){
    res_l <- cbind(no, id, num_ingredients, res_l)
  }
  
  if(initFlg==F){
    resAll <- res_l
    initFlg <- T
  } else {
    resAll <- rbind(resAll, res_l) 
  }
}


autoNo <- c(1:nrow(resAll))
filename <- lstInputFileNmList[[datasetTypeNm]]
resAll <- cbind(autoNo, resAll, filename)

colList <- colnames(resAll)
num_colList <- length(colList)
for(j in c(1:num_colList)){
  varname <- colList[j]
  resAll[,varname] <- gsub(",", "", resAll[,varname])
  resAll[,varname] <- as.character(resAll[,varname])
}


# output csv (resAll)
if(T){
  outFileName0 <- gsub(".json","",lstInputFileNmList[[datasetTypeNm]])
  outFileName <- paste0(outFileName0, "_ncolMax_", ncolMax, ".csv")
  outFilePath <- paste(inputDir, outFileName, sep="")
  write.csv(resAll, outFilePath, row.names=F, na=".")
}
dim(resAll)
#head(resAll)
colnames(resAll)

#=== [END]:R-Script ===
