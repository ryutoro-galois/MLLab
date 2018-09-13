# 20180913_SummaryInfo.R
#
# @date  : 2018/09/13(Thu.)
# @brief : データのサマリー情報を出力する 

# load libraries
library(dplyr)

# init
lstPrint <- list()

# True=初めてデータを読み込む場合
# False=2回目以降
if(T){
  initLoadFlg <- T
  #initLoadFlg <- F
}

# 出力最大カテゴリ数(5以上)
if(T){
  nCategoryMax <- 5
  #nCategoryMax <- 10
  #nCategoryMax <- 20
  if(nCategoryMax < 5) stop("error!!")
  
  # set
  lstPrint[["nCategoryMax"]] <- nCategoryMax
}

if(T){
  # set working directory
  target_wd_ID <- 1 # 適宜変更
  targetWDList <- list()
  targetWDList[[1]] <- "" # 適宜変更
  targetWDList[[2]] <- "" # 適宜変更
  setwd(targetWDList[[target_wd_ID]])
  print(getwd())
  
  # input/output directory
  inputDir <- "Rin/"
  outputDir <- paste("Rout/summaryInfo/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
}


# @name : is.integer0
is.integer0 <- function(x) { is.integer(x) && length(x) == 0L }


# @name : GetDateTime_YYYYMMDD_HHMMSS
GetDateTime_YYYYMMDD_HHMMSS <- function()
{
  timex <- Sys.time()
  yyyymmdd <- gsub("/", "", as.character(format(timex, "%Y/%m/%d")))
  hhmmdd <- gsub(":", "", as.character(format(timex, "%H:%M:%S")))
  res <- paste(yyyymmdd, "_", hhmmdd, sep="")
  return(res)
}


# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test.csv"    # 予測用データ
}

# load data
if(T){
  inputFileName <- lstInputFileNmList[["train"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  dat <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  
  colList <- colnames(dat)
  num_colList <- length(colList)
}


# load data (colList_df)
if(!initLoadFlg){
  
  inputFilePath <- paste0(inputDir, paste0("colList_", gsub(".csv","",inputFileName), ".csv"))
  colList_df <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
  
} else {
  
  varclass <- unlist(sapply(1:ncol(dat), function(l) class(dat[,l])[1]))
  colList_df <- data.frame("no"=c(1:num_colList), "varname"=colList, "class"=varclass, 
                           "type"="category", "isuse"=1, "fileName"=inputFileName, stringsAsFactors=F)
}

nCategory_plus_others <- (nCategoryMax + 1)

strid <- paste0("0",c(1:nCategoryMax))
strid <- substr(strid, nchar(strid)-1, nchar(strid))
strid <- paste0("cat", strid)

resMat <- matrix(NA, nrow=2, ncol=nCategory_plus_others) %>% as.data.frame()
colnames(resMat) <- c(strid, "others")
resMat <- resMat[1,]
resMat2 <- resMat



##############
### l-loop ###
##############

initFlg <- F
resAll <- 0

#for(l in c(1:1))
for(l in c(1:num_colList))
{
  no <- l
  varname <- colList_df[l,"varname"]
  varname2 <- varname
  type <- colList_df[l,"type"]
  isuse <- colList_df[l,"isuse"]
  
  if(isuse == 1)
  {
    nSample <- format(nrow(dat), big.mark=",")
    nNA <- format(sum(is.na(dat[,varname])), big.mark=",")
    NA_ratio <- paste0(round((sum(is.na(dat[,varname])) / nrow(dat) *100), 2), "%")
    
    # カテゴリ型 (非数値型またはカテゴリとして扱いたい数値型): 
    if(type == "category")
    {
      cntInfo_l <- table(dat[,varname], useNA="ifany") %>% as.data.frame() 
      cntInfo_l <- cntInfo_l %>% dplyr::arrange(desc(Freq)) %>% as.data.frame()
      if(T) {
        cntInfo_l[,"Var1"] <- as.character(cntInfo_l[,"Var1"])
        cntInfo_l[(cntInfo_l[,"Var1"] %in% c(NA)),"Var1"] <- "NA(欠損)"
      }
      cntInfo_l$Total <- sum(cntInfo_l[,"Freq"])
      cntInfo_l[,"Ratio"] <- cntInfo_l[,"Freq"]/cntInfo_l[,"Total"]
      num_cntInfo_l <- nrow(cntInfo_l)
      
      nCategory <- as.character(format(num_cntInfo_l, big.mark=","))
      
      cntInfo_l[,"Freq_fmt"] <- format(cntInfo_l[,"Freq"], big.mark=",")
      cntInfo_l[,"Ratio_fmt"] <- paste0(round(cntInfo_l[,"Ratio"]*100, 1), "%")
      cntInfo_l[,"Freq_Ratio_fmt"] <- paste0(cntInfo_l[,"Freq_fmt"], "(", cntInfo_l[,"Ratio_fmt"], ")")
      print(head(cntInfo_l))
      
      if(num_cntInfo_l > nCategoryMax)
      {
        col_idx <- (1:nCategoryMax)
        col_idx_others <- ((nCategoryMax+1):num_cntInfo_l)
        
        resMat[1,] <- c(cntInfo_l[col_idx,"Var1"], "others")
        
        x1 <- gsub(" ", "", cntInfo_l[col_idx, "Freq_Ratio_fmt"])
        x2 <- gsub(" ", "", paste0(format(sum(cntInfo_l[col_idx_others, "Freq"]), big.mark=","),
                                   "(", round(sum(cntInfo_l[col_idx_others, "Ratio"])*100, 2), "%", ")"))
        resMat2[1,] <- c(x1, x2)
        
      } else {
        
        col_idx <- (1:nCategoryMax)
        resMat[1,col_idx] <- cntInfo_l[col_idx, "Var1"]
        resMat2[1,col_idx] <- gsub(" ", "", cntInfo_l[col_idx, "Freq_Ratio_fmt"])
        
      }
      
      if(T){
        forPrint_l <- paste0("[",l,"/",num_colList,"]: varname=[", varname, "], nCategory=", nCategory)
        print(forPrint_l)
      }
      
      res_l <- rbind(resMat[1,], resMat2[1,], stringsAsFactors=F)
      res_l <- cbind(no, varname, varname2, type, nSample, nNA, NA_ratio, nCategory, res_l)
      res_l[2,c("varname2")] <- NA
      
    } else if(type == "numeric") {
      
      # 数値型の場合: 基本統計量を算出
      resMat[1,(1:6)] <- c("Min.", "1st Qu.", "Median", "Median", "3rd Qu.", "Max.")
      resMat2[1,(1:6)] <- as.numeric(summary(dat[,varname])[1:6])
      res_l <- rbind(resMat[1,], resMat2[1,], stringsAsFactors=F)
      
      nCategory <- NA
      res_l <- cbind(no, varname, varname2, type, nSample, nNA, NA_ratio, nCategory, res_l)
      res_l[2,c("varname2")] <- NA
      
    }
    
    # append
    if(initFlg==F){
      resAll <- res_l
      initFlg <- T
    } else {
      resAll <- rbind(resAll, res_l, stringsAsFactors=F)
    }
    
  } # (isuse==1)
  
} # l-loop



# 初回のみ
# output csv (colList_df)
if(initLoadFlg==T)
{
  resAll_filter <- resAll[is.na(resAll[,"varname2"]),c("varname","nCategory")]
  resAll_filter$varname <- as.character(resAll_filter$varname)
  
  # merge
  colList_df <- dplyr::left_join(colList_df, resAll_filter, by=c("varname"))
  
  colList_df[,"nCategory"] <- as.numeric(gsub(",","",colList_df[,"nCategory"]))
  
  fillFlg <- (colList_df[,"nCategory"]>=100)
  colList_df[fillFlg,"type"] <- "numeric"
  
  # output csv (colList_df)
  outFileName0 <- gsub(".csv","",inputFileName)
  outFileName <- paste0("colList_", outFileName0, ".csv")
  outFilePath <- paste(inputDir, outFileName, sep="")
  write.csv(colList_df, outFilePath, row.names=F, quote=F)
}



# output csv (summaryInfo)
if(T){
  autoNo <- c(1:nrow(resAll))
  resAll <- cbind(autoNo, resAll, nCategoryMax)
  outFileName0 <- gsub(".csv","",inputFileName)
  outFileName <- paste0("summaryInfo_", outFileName0, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".csv")
  outFilePath <- paste(outputDir, outFileName, sep="")
  write.csv(resAll, outFilePath, row.names=F, quote=T, na=".")
}


if(T){
  rownames(resAll) <- NULL
  resAll$varname2 <- NULL
  resAll$nSample <- NULL
  resAll$nCategoryMax <- NULL
}

# set
lstPrint[["summaryInfo"]] <- resAll

#=== [END]:R-Script ===
