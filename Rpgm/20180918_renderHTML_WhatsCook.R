# 20180918_renderHTML_WhatsCook.R
#
# @date  : 2018/09/18(Tue.)
# @brief : convert Rmarkdown to html

batchFlg_renderHTML <- T

# sessionInfo
sessionInfo()

# install.packages
if(F){
  install.packages("dplyr", dependencies = T)
  install.packages("ggplot2", dependencies = T)
  install.packages("rmarkdown", dependencies = T)
  install.packages("knitr", dependencies = T)
}

# load libraries
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(knitr)

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


# target variable (目的変数)
if(T){
  targetNameList <- c("m01_italian", "m02_mexican", "m03_southern_us", "m04_indian", "m05_chinese",
                      "m06_french", "m07_cajun_creole", "m08_thai", "m09_japanese", "m10_greek",
                      "m11_spanish", "m12_korean", "m13_vietnamese", "m14_moroccan", "m15_british",
                      "m16_filipino", "m17_irish", "m18_jamaican", "m19_russian", "m20_brazilian")
  num_targetName <- length(targetNameList)
}

##############
### i-loop ###
##############

for(i in c(1:num_targetName))
{
  targetName <- targetNameList[i]
  
  if(T){
    forPrint_i <- paste0("[",i,"/",num_targetName,"]: targetName=[", targetName,"]")
    print(forPrint_i)
  }
  
  # load other sources
  source("Rpgm/20180918_TreeModel_WhatsCook.R", encoding="utf-8")
  
  knitr::opts_knit$set(root.dir = getwd())
  
  # Rmarkdown(.Rmd) filename
  inputFileNm_Rmd <- "Rpgm/20180918_TreeModel_WhatsCook.Rmd"
  
  # output html filename
  #outFilePath_html <- paste0(inputDir, "sample_", GetDateTime_YYYYMMDD_HHMMSS(), ".html")
  outFilePath_html <- paste0(inputDir, "TreeModel_[", targetName, "].html")
  
  
  # convert Rmarkdown to html
  rmarkdown::render(input=inputFileNm_Rmd, 
                    output_format="html_document", 
                    output_file=paste0("../", outFilePath_html), encoding='utf-8')
} # i-loop

#=== [END]:R-Script ===
