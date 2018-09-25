# RandomForestSettings.R

###################
### RF Settings ###
###################

# 乱数シード
CNST_SEED <- 1234


# rangerRFの計算使用スレッド
#CNST_CALC_THREAD_RANGER <- 3
CNST_CALC_THREAD_RANGER <- 4
#CNST_CALC_THREAD_RANGER <- 6
#CNST_CALC_THREAD_RANGER <- 8


# mtry_RF
if(T){
  #mtry_RF <- 2
  #mtry_RF <- 3
  #mtry_RF <- 4
  #mtry_RF <- 5
  #mtry_RF <- 6
  #mtry_RF <- 7
  #mtry_RF <- 8
  #mtry_RF <- 9
  mtry_RF <- 10
  #mtry_RF <- 15
  #mtry_RF <- 20
  #mtry_RF <- 25
  #mtry_RF <- 30
}

# nTree_RF
if(T){
  #nTree_RF <- 100
  #nTree_RF <- 500
  #nTree_RF <- 1000
  #nTree_RF <- 5000
  nTree_RF <- 10000
}

# samplesize_RF
if(F){
  samplesize_RF <- 10000
}

# minNodeSize_type: 1=比率, 2=サンプル数で指定
if(T){
  minNodeSize_type <- 1
  #minNodeSize_type <- 2
}

# minNodeSizeRatio
if(T){
  #minNodeSizeRatio <- 0.1
  #minNodeSizeRatio <- 0.05
  minNodeSizeRatio <- 0.01
}

# minNodeSizeByManual
if(T){
  minNodeSizeByManual <- 500
  #minNodeSizeByManual <- 100
}

strRFSettings <- paste("mtry=",mtry_RF, "_nTree=",nTree_RF, sep="")

if(minNodeSize_type == 1){
  strRFSettings <- paste(strRFSettings, "_minNodeSize=", round(minNodeSizeRatio*100,0),"per", sep="")
} else if(minNodeSize_type == 2){
  strRFSettings <- paste(strRFSettings, "_minNodeSize=", minNodeSizeByManual ,sep="")
}

# ===[END]:RF Settings===