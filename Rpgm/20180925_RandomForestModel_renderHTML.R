# 20180925_RandomForestModel_renderHTML
#
# @date  : 2018/09/25(Tue.)
# @brief : convert Rmarkdown to html

batchFlg_renderHTML <- T

# sessionInfo
#sessionInfo()

# install.packages
if(F){
  install.packages("dplyr", dependencies = T)
  install.packages("ggplot2", dependencies = T)
  install.packages("rmarkdown", dependencies = T)
  install.packages("knitr", dependencies = T)
  install.packages("DT", dependencies = T)
}

# load libraries
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(DT)

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
source("Rpgm/20180925_RandomForestModel_HousingPrice.R", encoding="utf-8")

knitr::opts_knit$set(root.dir = getwd())

# Rmarkdown(.Rmd) filename
inputFileNm_Rmd <- "Rpgm/20180925_RandomForestModel_sample.Rmd"

# output html filename
outFilePath_html <- paste0(inputDir, "RandomForestModel_[", targetName, "]_", GetDateTime_YYYYMMDD_HHMMSS(), ".html")


if(T){
  # convert Rmarkdown to html
  rmarkdown::render(input=inputFileNm_Rmd, 
                    output_format="html_document", 
                    output_file=paste0("../", outFilePath_html), encoding='utf-8')
}

#=== [END]:R-Script ===
