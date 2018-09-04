# 20180828_geom_bar_titanic.R
#
# @date : 2018/08/28(Tue.)
# @note : 1軸/2軸集計

# load libraries
library(ggplot2)
library(grid)
library(gridExtra)


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

# load data (datSample)
if(T){
  inputFileName <- lstInputFileNmList[["train"]]
  inputFilePath <- paste(inputDir, inputFileName, sep="")
  dat <- read.csv(inputFilePath, header=T, stringsAsFactors=F)
}

if(T){
  dat[,"Cabin"] <- substr(dat[,"Cabin"], 1, 1)
  dat[,"Age"] <- as.integer(dat[,"Age"]/10)
}


###############
### 1軸集計 ###
###############

# init
lst_df <- list()
list_plot <- list()
list_png_1var <- list()
list_png_2var <- list()

colList <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Cabin", "Embarked")
num_colList <- length(colList)

for(l in c(1:num_colList))
{
  strid <- paste0("00",l)
  strid <- substr(strid, nchar(strid)-2, nchar(strid))
  
  varname <- colList[l]
  df <- geom_bar_1var(dat, targetName, varname, isAddTotal=T)
  lst_df[[varname]] <- df
  
  titleLabel <- paste0("[", targetName, "] ~ ", strid, ": [", varname, "]")
  outFileName <- paste0("geom_bar_1var_", strid, "_", varname, ".png")
  outFilePath <- paste0(outputDir, outFileName)
  # set
  list_png_1var[[l]] <- outFilePath
  
  width <- 800
  height <- width * 0.7
  
  p <- ggplot_geom_bar_1var(df, titleLabel, outFilePath, width, height, xlab="", ylab="ratio",
                            fontsize=18, face="bold", fill="gray", hjust=0.5)
  list_plot[[varname]] <- ggplotGrob(p)
  print(outFilePath)
}



###############
### 2軸集計 ###
###############

# 任意の組み合わせ
if(T){
  matX <- combn(colList, 2)
  n_matX <- dim(matX)[2]
  lst_Combin <- list()
  for(i in (1:n_matX)) lst_Combin[[i]] <- matX[,i]
  print(n_matX)
}

cnt <- 0
for(i in c(1:n_matX))
{
  strid <- paste0("00",i)
  strid <- substr(strid, nchar(strid)-2, nchar(strid))
  
  varname1 <- lst_Combin[[i]][1]
  varname2 <- lst_Combin[[i]][2]
  print(cbind(varname1, varname2))
  lst_df <- geom_bar_2var(dat, targetName, varname1, varname2, roundDigit=2, isAddTotal=T)
  
  nCat1 <- nrow(lst_df[[1]])
  nCat2 <- length(lst_df)
  
  forPrint_i <- paste0(strid, ": varname1=[",varname1,"], varname2=[",varname2,"], nCat1=",
                       nCat1, ", nCat2=",nCat2)
  print(forPrint_i)
  
  if(nCat1 <= 5 & nCat2 <= 5)
  {
    cnt <- cnt + 1
    titleLabel <- paste0("[", targetName, "] ~ ", strid, ": [", varname1, "] + [", varname2,"]")
    outFileName <- paste0("geom_bar_2var_", strid, "_", varname1, "_", varname2 ,".png")
    outFilePath <- paste0(outputDir, outFileName)
    # set
    list_png_2var[[cnt]] <- outFilePath
    
    width <- 1200
    height <- width * 0.6
    
    ggplot_geom_bar_2var(lst_df, titleLabel, outFilePath, width, height, xlab="", ylab="ratio",
                         fontsize=18, face="bold", fill="gray", hjust=0.5)
    print(outFilePath)
  }
}

num_list_png_1var <- length(list_png_1var)
num_list_png_2var <- length(list_png_2var)

#=== [END]:R-Script ===
