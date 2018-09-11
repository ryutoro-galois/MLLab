# TreeSettings.R


###################
### 決定木param ###
###################


# 応答値ラベル名(適宜変更)
if(T){
  responseLabel <- c("No", "Yes")
  #responseLabel <- c("n", "y")
  #responseLabel <- c("non-survived", "survived")
  #responseLabel <- c("0", "1")
}

isusePrior <- F

# prior
if(isusePrior){
  PriorProbByManual <- c(0.5, 0.5)
}

# Cp
if(T){
  Cp <- -1
  #Cp <- 0.01
}

# Split
Split <- "gini"


# 事前剪定
if(isPrePruing == T)
{
  # 最大階層数
  if(T){
    #MaxDepth <- 3
    MaxDepth <- 4
    #MaxDepth <- 5
    #MaxDepth <- 6
    #MaxDepth<- 10
  }
  
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
  
} else {
  
  # 事後剪定
  if(T){
    #MaxDepth_cp0 <- 10
    #MaxDepth_cp0 <- 15
    #MaxDepth_cp0 <- 20
    MaxDepth_cp0 <- 30
  }
  
}






# ===[END]:決定木param===

