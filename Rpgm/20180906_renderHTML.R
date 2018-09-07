# 20180906_renderHTML.R
#
# @date  : 2018/09/06(Thu.)
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

# load other sources
if(T){
  source("Rpgm/20180906_TreeModel.R", encoding="utf-8")
}

knitr::opts_knit$set(root.dir = getwd())

# Rmarkdown(.Rmd) filename
inputFileNm_Rmd <- "Rpgm/20180906_sample.Rmd"

# output html filename
outFilePath_html <- paste0(inputDir, "sample_", GetDateTime_YYYYMMDD_HHMMSS(), ".html")

# convert Rmarkdown to html
rmarkdown::render(input=inputFileNm_Rmd, 
                  output_format="html_document", 
                  output_file=paste0("../", outFilePath_html), encoding='utf-8')

#=== [END]:R-Script ===

