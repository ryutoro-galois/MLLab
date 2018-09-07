# TreeSettings.R

###################
### 決定木param ###
###################

isusePrior <- F

# 応答値ラベル名
if(T){
  responseLabel <- c("No", "Yes")
}

# prior
if(T){
  PriorProbByManual <- c(0.5, 0.5)
}

# 最大階層数
if(T){
  #MaxDepth <- 3
  MaxDepth <- 4
  #MaxDepth <- 5
  #MaxDepth <- 6
  #MaxDepth<- 10
}

# Cp
if(T){
  Cp <- -1
  #Cp <- 0.01
}

# Split
Split <- "gini"


MinBucketTypeID <- 2 # 1=サンプル数, 2=サンプル比率


# MinBucket,MinBucketRate
if(T){
  # init
  MinBucket <- NA
  MinBucketRate <- NA
  
  if(MinBucketTypeID == 1)
  {
    #MinBucket <- 20
    MinBucket <- 10
    
  } else if(MinBucketTypeID == 2) {
    if(T){
      #MinBucketRate <- 0.1
      MinBucketRate <- 0.05
      #MinBucketRate <- 0.01
      #MinBucketRate <- 0.001
    }
    strMinBucketRate <- paste(MinBucketRate*100, "per", sep="")
    strMinBucketRate2 <- paste(MinBucketRate*100, "%", sep="")
    
  }
}

# ===[END]:決定木param===

