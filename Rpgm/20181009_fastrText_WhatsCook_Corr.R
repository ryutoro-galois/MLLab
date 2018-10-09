# 20181009_fastrText_WhatsCook_Corr.R
#
# @date : 2018/10/09(Tue.)

if(F){
  devtools::install_github("pommedeterresautee/fastrtext")
  install.packages("Rtsne", dependencies=T)
  install.packages("ggrepel", dependencies=T)
}

library(fastrtext)
packageVersion("fastrtext")


library(dplyr)
library(ggplot2)
library(ggrepel)
library(ROCR)
library(caret)
library(Rtsne)


if(T){
  #topN_ingredients <- 20
  topN_ingredients <- 30
  #topN_ingredients <- 40
}

# statisticalMethodNm
statisticalMethodNm <- "fastrtextModel"

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
  outputDir <- paste("Rout/fastText/", sep="")
  
  # make subfolder
  if(!file.exists(inputDir)) dir.create(inputDir)
  if(!file.exists(outputDir)) dir.create(outputDir)
  
  # load other sources
  source("Rpgm/utility.R", encoding="utf-8")
  source("Rpgm/InitSettings_Cook_03.R", encoding="utf-8")
}


# サブフォルダ作成
if(T){
  strComment <- NA
  if(is.na(strComment)){
    folderNm <- GetDateTime_YYYYMMDD_HHMMSS()
  } else {
    folderNm <- paste(strComment, "_", GetDateTime_YYYYMMDD_HHMMSS(), sep="")
  }
  outputDir <- paste(outputDir, folderNm, "/", sep="")
  dir.create(outputDir)
}

# init
lstOutFilePath <- list()
lstPrint <- list()


# output text file for model input
if(T){
  outFileName <- paste0(gsub(".csv","",lstInputFileNmList[["train"]]), ".txt")
  outFilePath <- paste(inputDir, outFileName, sep="")
  train_tmp_file_txt <- outFilePath
  write.table(datTrain, outFilePath, row.names=F, quote=F, sep=" ")
}

outFileName <- paste0(gsub(".csv","",lstInputFileNmList[["train"]]), "_model")
outFilePath <- paste(inputDir, outFileName, sep="")
tmp_file_model <- outFilePath

# learn model
fastrtext::execute(commands = c("skipgram", "-input", train_tmp_file_txt, 
                                "-output", tmp_file_model, 
                                "-dim", 100, "-lr", 0.01, "-epoch", 100, "-wordNgrams", 2, "-verbose", 1))
# load model
model <- load_model(tmp_file_model)


##########################
### cuisine similarity ###
##########################

# get_word_vectors
df_cuisine <- fastrtext::get_word_vectors(model, words=targetNameList)

# t-SNE
set.seed(1234)
cuisine_tsne <- Rtsne::Rtsne(df_cuisine, perplexity=2.25, max_iter=1e5)$Y %>% as.data.frame()
colnames(cuisine_tsne) <- c("x","y")
rownames(cuisine_tsne) <- rownames(df_cuisine)

# plot
if(T){
  outFileName <- paste0("cuisineSimilarity_", GetDateTime_YYYYMMDD_HHMMSS(), ".png")
  outFilePath <- paste0(outputDir, outFileName) 
  # set
  lstOutFilePath[["cuisineSimilarity"]] <- outFilePath
  
  width <- 1200
  height <- width*0.7
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try ({
    if(T){
      titleLabel <- "Cuisine Similarity"
      p <- ggplot(cuisine_tsne, aes(x=x, y=y))
      p <- p + geom_point(color="black", size=5)
      p <- p + ggrepel::geom_label_repel(aes(x=x, y=y, label=rownames(cuisine_tsne)), 
                                         size=7, label.size=0.5, segment.size=0.5,
                                         box.padding=0.5, point.padding=0.5, 
                                         segment.color='gray')
      p <- p + ggtitle(titleLabel)
      p <- p + theme(plot.title=element_text(hjust=0.5, size=24))
    }
    plot(p)
  }, TRUE)
  dev.off()
}
#=== [cuisine similarity] ===


#############################
### ingredient Similarity ###
#############################

#for(k in c(1:1))
for(k in c(1:num_objective_class))
{
  cuisine_k <- targetNameList[k]
  
  # filter
  fillFlg_k <- datTrain[,targetName] %in% cuisine_k
  cnt_k <- sum(fillFlg_k)
  cnt_k_fmt <- format(cnt_k, big.mark=",")
  
  if(T){
    forPrint_k <- paste0("[",k,"/",num_objective_class, "]: [", cuisine_k, "], cnt=", cnt_k_fmt)
    print(forPrint_k)
  }
    
  # ingredients_freq_topN
  ingredients_str <- datTrain[fillFlg_k,"ingredients_str"]
  ingredients <- unlist(sapply((1:cnt_k), function(j) str_split(ingredients_str[j], " ")))
  ingredients_freq <- table(ingredients) %>% as.data.frame.table() %>% dplyr::arrange(desc(Freq))
  ingredients_freq_topN <- ingredients_freq[c(1:topN_ingredients), ]
  
  # get_word_vectors
  df_ingredient <- fastrtext::get_word_vectors(model, words=as.character(ingredients_freq_topN[,1]))
  
  # t-SNE
  set.seed(1234)
  ingredients_tsne <- Rtsne::Rtsne(df_ingredient, perplexity=2.25, max_iter=1e5)$Y %>% as.data.frame()
  colnames(ingredients_tsne) <- c("x","y")
  rownames(ingredients_tsne) <- rownames(df_ingredient)
  
  # plot
  if(T){
    outFileName <- paste0("ingredientSimilarity_[", cuisine_k, "]_topN_", topN_ingredients, "_", GetDateTime_YYYYMMDD_HHMMSS(), ".png")
    outFilePath <- paste0(outputDir, outFileName) 
    # set
    lstOutFilePath[[paste0("ingredientSimilarity_", cuisine_k)]] <- outFilePath
    
    width <- 1200
    height <- width*0.7
    png(outFilePath, width=width, height=height)
    par(xpd=NA)
    try ({
      if(T){
        titleLabel <- paste0("\n Ingredient Similarity (topN ingredients=",topN_ingredients, ") \n [ ", cuisine_k, " ]\n")
        p <- ggplot(ingredients_tsne, aes(x=x, y=y))
        p <- p + geom_point(color="black", size=5)
        p <- p + ggrepel::geom_label_repel(aes(x=x, y=y, label=rownames(ingredients_tsne)), 
                                           size=7, label.size=0.5, segment.size=0.5,
                                           box.padding=0.5, point.padding=0.5, 
                                           segment.color='gray')
        p <- p + ggtitle(titleLabel)
        p <- p + theme(plot.title=element_text(hjust=0.5, size=24))
      }
      plot(p)
    }, TRUE)
    dev.off()
  }
  
} # k-loop

#=== [ingredient Similarity] ===


#=== [END]:R-Script ===
