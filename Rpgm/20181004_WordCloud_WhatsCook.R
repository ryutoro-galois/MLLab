# 20181004_WordCloud_WhatsCook.R
#
# @date : 2018/10/04(Thu.)
# @note : 料理ジャンル(cuisine)ごとにwordCloudを出力

# install.packages
if(F){
  install.packages("wordcloud", dependencies = T)
}

# load libraries
library(dplyr)
library(wordcloud)

par(family="HiraKakuProN-W3")

################
### settings ###
################

# stopwords
if(T){
  stopwords <- NA
  stopwords <- c("salt","sugar","water")
  #stopwords <- c("salt","sugar","water","soy_sauce","all-purpose_flour")
  #stopwords <- c("salt","sugar","water","soy_sauce","all-purpose_flour","onions","olive_oil","soy_sauce","milk","eggs")
}

# topN_word
if(T){
  topN_word <- 100
  #topN_word <- 200
}

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
  outputDir <- paste("Rout/wordcloud/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")
}

# set dataset filename
if(T){
  lstInputFileNmList <- list()
  lstInputFileNmList[["train"]] <- "train_ncolMax_65_forAnalysis_topN_300.csv"  # 学習用データ
  lstInputFileNmList[["test"]] <- "test_ncolMax_65_forAnalysis_topN_300.csv"    # 予測用データ
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
  datTrain[,"ingredients_str"] <- gsub(" ", "_", datTrain[,"ingredients_str"])
}

colListAll <- colnames(datTrain)
colList_cuisine <- colListAll[grep("^m",colListAll)]
print(colList_cuisine)
num_cuisine <- length(colList_cuisine)


# init
lstOutFilePath <- list()
lstPrint <- list()


##############
### k-loop ###
##############

for(k in c(1:num_cuisine))
{
  cuisine_k <- colList_cuisine[k]
  
  # filter cuisine
  fillFlg_k <- (datTrain[,cuisine_k] %in% c(1))
  datTrain_k <- datTrain[fillFlg_k,]
  cnt_k <- nrow(datTrain_k)
  
  # split ingredients word
  ingredients_word <- unlist(sapply((1:cnt_k), function(l) unlist(strsplit(datTrain_k[l,"ingredients_str"], "|", fixed=T))))
  
  # word cnt
  df_wordFreq <- table(ingredients_word) %>% as.data.frame.table() %>% dplyr::arrange(desc(Freq))
  df_wordFreq[,"ratio"] <- round(df_wordFreq[,"Freq"]/sum(df_wordFreq[,"Freq"]),3)
  
  # remove stopwords
  if(!is.na(stopwords) & length(stopwords)>0){
    fillFlg_stopwords <- df_wordFreq[,"ingredients_word"] %in% stopwords
    df_wordFreq <- df_wordFreq[!fillFlg_stopwords,]   
  }

  # wordcloud
  if(T){
    outputFileName_wordcloud_png <- paste0("WhatsCook_wordcloud_[", cuisine_k, "].png")
    outputFilePath_wordcloud_png <- paste(outputDir, outputFileName_wordcloud_png, sep="")
    
    width <- 800
    height <- width*0.8
    png(outputFilePath_wordcloud_png, width=width, height=height)
    
    par(xpd=NA)
    try ({
      set.seed(1234)
      min.freq <- df_wordFreq[min(topN_word, nrow(df_wordFreq)),"Freq"]
      wordcloud::wordcloud(df_wordFreq[,"ingredients_word"], df_wordFreq[,"Freq"], 
                           min.freq=min.freq, random.order=F, rot.per=0, 
                           scale=c(4,1), colors = brewer.pal(8,"Dark2"))
    }, TRUE)
    dev.off()
    
    # set
    lstOutFilePath[[paste0("wordcloud_", cuisine_k)]] <- outputFilePath_wordcloud_png
    
    print(outputFilePath_wordcloud_png)
    print(head(df_wordFreq))
  }

} # k-loop

#=== [END]:R-Script ===
