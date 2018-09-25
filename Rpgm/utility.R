# utility.R
#
# @date : 2018/08/28(Tue.)


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

############################################

# @name : geom_bar_1var (1軸集計)
geom_bar_1var <- function(dat, targetName, varname, roundDigit=2, isAddTotal=T)
{
  catList <- unique(dat[,varname])
  catList <- catList[order(catList)]
  nCat <- length(catList)
  
  df <- as.data.frame(matrix(NA, nrow=nCat+1, ncol=3))
  colnames(df) <- c("id", "value", "label")
  
  for(l in c(1:(nCat+1)))
  {
    if(l<=nCat){
      catNm_l <- catList[l]
      fillFlg_l <- dat[,varname] %in% catNm_l
    } else {
      catNm_l <- "total"
      fillFlg_l <- rep(T,nrow(dat))
    }
    
    cnt_total <- sum(fillFlg_l)
    cnt1 <- sum(dat[fillFlg_l,targetName])
    ratio1 <- cnt1 / cnt_total
    
    cnt_total_fmt <- format(cnt_total, big.mark = ",")
    cnt1_fmt <- format(cnt1, big.mark = ",")
    label_l <- paste0(round(ratio1, roundDigit), " (", cnt1_fmt, "/", cnt_total_fmt, ")")
    
    df[l,"id"] <- catNm_l
    df[l,"value"] <- ratio1
    df[l,"label"] <- label_l
  }
  if(!isAddTotal) df <- df[-(nCat+1),]
  return(df)
}

############################################

# @name : geom_bar_2var  (2軸集計)
geom_bar_2var <- function(dat, targetName, varname1, varname2, roundDigit=2, isAddTotal=T)
{
  catList1 <- unique(dat[,varname1])
  catList1 <- catList1[order(catList1)]
  nCat1 <- length(catList1)
  
  catList2 <- unique(dat[,varname2])
  catList2 <- catList2[order(catList2)]
  nCat2 <- length(catList2)
  
  # yobi
  dat_init <- dat
  
  # init
  lst_df <- list()
  
  for(k in c(1:(nCat2+1)))
  {
    # init
    dat <- dat_init
    
    # filter
    if(k<=nCat2){
      catNm2_k <- catList2[k]
      fillFlg_k <- dat[,varname2] %in% catNm2_k
      dat <- dat[fillFlg_k,]
    } else {
      catNm2_k <- "total"
    }
    
    # init df
    df <- as.data.frame(matrix(NA, nrow=nCat1+1, ncol=3))
    colnames(df) <- c("id", "value", "label")
    
    for(l in c(1:(nCat1+1)))
    {
      if(l<=nCat1){
        catNm_l <- catList1[l]
        fillFlg_l <- dat[,varname1] %in% catNm_l
      } else {
        catNm_l <- "total"
        fillFlg_l <- rep(T,nrow(dat))
      }
      
      cnt_total <- sum(fillFlg_l)
      cnt1 <- sum(dat[fillFlg_l,targetName])
      ratio1 <- cnt1 / cnt_total
      
      cnt_total_fmt <- format(cnt_total, big.mark = ",")
      cnt1_fmt <- format(cnt1, big.mark = ",")
      label_l <- paste0(round(ratio1, roundDigit), " (", cnt1_fmt, "/", cnt_total_fmt, ")")
      
      df[l,"id"] <- catNm_l
      df[l,"value"] <- ratio1
      df[l,"label"] <- label_l
    } # l-loop
    if(!isAddTotal) df <- df[-(nCat1+1),]
    
    df <- cbind(df, varname1, varname2, catNm2_k)
    
    # set
    lst_df[[k]] <- df
  } # k-loop
  
  return(lst_df)
}


############################################


# @name : ggplot_geom_bar_1var (1軸集計のプロット)
ggplot_geom_bar_1var <- function(df, titleLabel, outFilePath, width, height, xlab="", ylab="",
                                 fontsize=18, face="bold", fill="gray", hjust=0.5)
{
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try ({
    if(T){
      p <- ggplot(df, aes(x=reorder(id, value), y=value))
      p <- p + geom_bar(stat="identity", position="dodge", fill=fill)
      p <- p + xlab(xlab) + ylab(ylab)
      p <- p + theme(axis.text.x=element_text(size=fontsize, face=face),
                     axis.text.y=element_text(size=fontsize, face=face),
                     plot.title=element_text(size=fontsize, face=face))
      p <- p + geom_text(aes(label=paste(df[,"label"],sep="")), hjust=0.8, color="black", size=7)
      p <- p + coord_flip()
      p <- p + ggtitle(titleLabel)
      p <- p + theme(plot.title=element_text(hjust=hjust))
    }
    plot(p)
  }, TRUE)
  dev.off()
  return(p)
}

############################################


# @name : ggplot_geom_bar_2var (2軸集計のプロット)
ggplot_geom_bar_2var <- function(lst_df, titleLabel, outFilePath, width, height, xlab="", ylab="",
                                 fontsize=16, face="bold", fill="gray", hjust=0.5)
{
  ncol <- length(lst_df)
  
  # init
  list_plot <- list()
  
  for(k in c(1:ncol))
  {
    # set
    df <- lst_df[[k]]
    titleLabel_k <- as.character(df[1,"catNm2_k"])
    
    # with RankID
    strID <- c(1:nrow(df))
    strID <- paste("0",strID, sep="")
    strID <- substr(strID, nchar(strID)-1, nchar(strID))
    df[,"id"] <- paste(strID, " ", df[,"id"], sep="")
    
    level_reverse <- df[(order(df[,"id"], decreasing = T)), "id"]
    df_reverse <- transform(df, id=factor(id, level=level_reverse))
    
    p <- ggplot(df, aes(x=df_reverse[,1], y=df[,2]))
    p <- p + geom_bar(stat="identity", position="dodge", fill=fill)
    p <- p + xlab(xlab) + ylab(ylab)
    p <- p + theme(axis.text.x=element_text(size=fontsize, face=face),
                   axis.text.y=element_text(size=fontsize, face=face),
                   plot.title=element_text(size=fontsize, face=face))
    p <- p + geom_text(aes(label=paste(df[,"label"],sep="")), hjust=0.85, color="black", size=5)
    p <- p + coord_flip(ylim=c(0.0, 1.0))
    p <- p + ggtitle(titleLabel_k)
    p <- p + theme(plot.title=element_text(hjust=hjust))
    list_plot[[k]] <- ggplotGrob(p)
  }
  
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try ({
    args <- c(list_plot, list(nrow=1, ncol=ncol, 
                              top=textGrob(titleLabel, gp=gpar(fontsize=18))))
    do.call(grid.arrange, args)
  }, TRUE)
  dev.off()
}



####################################
### 樹形図のプロット(rpart.plot) ###
####################################

# @name : split.fun_rpart.plot
split.fun_rpart.plot <- function(x, labs, digits, varlen, faclen)
{
  labs <- gsub(" = ", "\n=", labs)
  labs <- gsub(" >= ", "\n>=", labs)
  labs <- gsub(" <= ", "\n<=", labs)
  labs <- gsub(" < ", "\n<", labs)
  labs <- gsub(" > ", "\n>", labs)
  return(labs)
}

# @name : plotTree_rpart.plot_png
plotTree_rpart.plot_png <- function(model, outFilePath, titleLabel, width, height, fontsize)
{
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try({
    
    if(T){
      box.col_yval2 <- ifelse(model$frame$yval2[,5]>model$frame[1,]$yval2[,5],1,2)
      table(box.col_yval2)
      # リーフのみ色をつける
      if(T){
        is.leaf <- model$frame$var == "<leaf>"
        box.col_yval2[!is.leaf] <- 2
        table(box.col_yval2)
      }
      # prp()関数
      if(T){
        prp(model, main=titleLabel, cex.main=2.0, type=2, extra=106, tweak=1.5, digits=3, varlen=0, faclen=0,
            nn=T, ni=F, yesno=1, compress=T, ycompress=T, fallen.leaves=F, xflip=F, yflip=F,
            Margin=0, gap=5.0, leaf.round=0, facsep=",", split.fun=split.fun_rpart.plot, nn.round=0, 
            box.col=c("grey", "white")[box.col_yval2], shadow.col="grey", under=T)
      }
    } 
    
  },TRUE)
  dev.off()
}


############################################

# @name : getLeafRules.rpart
# @brief : 決定木_各ノードへの分岐条件抽出(2値分類の場合)
getLeafRules.rpart <- function(model, isProbNo=T, isReplaceNodeIDInPartykit=T)
{
  if(!inherits(model, "rpart")){
    stop("Not alegitimate rpart tree !!")
  }
  
  if(isProbNo == T){
    colID_prob <- 5 # V5=noの確率(フラグ=1)
  } else {
    colID_prob <- 4 # V4=yesの確率(フラグ=0)
  }
  
  # set
  if(T){
    frm <- model$frame
    names <- row.names(frm)
    ylevels <- attr(model, "ylevels")
    ds.size <- frm[1,]$n
    rootProb <- frm[1,]$yval2[,colID_prob] # colID_prob(no/yes)
    nLeaf <- sum(frm[,1]=="<leaf>")
  }
  
  # init
  res <- matrix(NA, nrow=nLeaf, ncol=10)
  colnames(res) <- c("no","node","count","ratio","response",
                     "prob","rootProb","lift","depth","rule")
  res <- as.data.frame(res)
  
  leafCnt <- 0
  for(i in (1:nrow(frm)))
  {
    if(frm[i,1] == "<leaf>")
    {
      leafCnt <- leafCnt+1
      nodeID <- names[i]
      count <- frm[i,]$n
      if(T){
        ratio <- frm[i,]$n / ds.size
        #ratio <- round(100*frm[i,]$n/ds.size)
        #ratio <- frm[i,]$yval2[,"nodeprob"] # nodeprob
      }
      response <- ylevels[frm[i,]$yval]
      prob <- frm[i,]$yval2[,colID_prob] # V5=no, V4=yes
      lift <- prob / rootProb
      # 各ノードへの条件分岐: path.rpart()関数
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=F)
      pth_unlist_trim <- gsub(" ", "", unlist(pth)[-1]) # [1]=rootを除く
      # 各条件に括弧を付ける場合
      if(T){
        strLeafRules <- paste("[", pth_unlist_trim, "]", sep="")
        strLeafRules <- paste(strLeafRules, collapse=" & ")
      }
      # 括弧を付けない場合
      if(F){
        strLeafRules <- paste(strLeafRules, collapse=" & ")
      }
      # 各ノードへ至るまでの階層数
      if(T){
        depth <- length(pth_unlist_trim)
      }
      
      # set values
      if(T){
        res[leafCnt,"no"] <- leafCnt
        res[leafCnt,"node"] <- nodeID
        res[leafCnt,"count"] <- count
        res[leafCnt,"ratio"] <- ratio
        res[leafCnt,"response"] <- response
        res[leafCnt,"rootProb"] <- rootProb
        res[leafCnt,"prob"] <- prob
        res[leafCnt,"lift"] <- lift
        res[leafCnt,"depth"] <- depth
        res[leafCnt,"rule"] <- strLeafRules
      }
      
    } # if
  } # i-loop
  
  # partykitのIDの修正(上書き)する場合
  if(isReplaceNodeIDInPartykit==T){
    # partykit::nodeids()関数
    nodeID_InPartykit <- partykit::nodeids(as.party(model), terminal=T)
    # 上書き
    res[,"node"] <- nodeID_InPartykit
  }
  return(res)
}
#=== [END]:getLeafRules.rpart ===


############################################


# @name  : one_se_rule_rpart
# @brief : 1SEルールに基づく最適樹木の剪定(tree for post-pruning)
one_se_rule_rpart <- function(model)
{
  x <- model$cptable
  j0 <- which.min(x[,"xerror"])
  Rcv.min <- x[j0,"xerror"]
  one.se <- x[j0,"xstd"]
  j1 <- 1
  while(x[j1,"xerror"] > Rcv.min + one.se) { j1 <- j1 + 1 }
  
  # j0=min, j1=1se
  if(j0 != j1){
    res <- x[c(j0,j1),]
  } else {
    res <- rbind(x[j0,], x[j1,])
  }
  res <- data.frame(res)
  type <- c("min", "1se")
  xval <- model$control$xval
  maxdepth <- model$control$maxdepth
  
  res <- cbind(type, res, xval, maxdepth)
  
  return(res)
}
#=== [END]:one_se_rule_rpart ===


############################################


# @name  : varImp.rpart
# @brief : 変数重要度の計算(rpart) 
varImp.rpart <- function(fit)
{
  tmp <- rownames(fit$splits)
  allVars <- colnames(attributes(fit$terms)$factors)
  rownames(fit$splits) <- c(1:nrow(fit$splits))
  splits <- data.frame(fit$splits)
  splits$var <- tmp
  splits$type <- ""
  frame <- as.data.frame(fit$frame)
  index <- 0
  
  for(i in c(1:nrow(frame))){
    if(frame$var[i] != "<leaf>"){
      index <- index + 1
      splits$type[index] <- "primary"
      if(frame$ncompete[i] > 0){
        for(j in (1:frame$ncompete[i])){
          index <- index + 1
          splits$type[index] <- "competing"
        }
      }
      if(frame$nsurrogate[i] > 0){
        for(j in (1:frame$nsurrogate[i])){
          index <- index + 1
          splits$type[index] <- "surrogate"
        }
      }
    }
  } # i-loop
  
  splits$var <- factor(as.character(splits$var))
  splits <- subset(splits, type != "surrogate")
  
  # set out
  out <- aggregate(splits$improve, list(Variable=splits$var), sum, na.rm=T)
  allVars <- colnames(attributes(fit$terms)$factors)
  if(!all(allVars %in% out$Variable)){
    missingVars <- allVars[!(allVars %in% out$Variable)]
    zeros <- data.frame(x=rep(0, length(missingVars)), Variable=missingVars)
    out <- rbind(out, zeros, stringsAsFactors=F)
  }
  
  # set out2
  out2 <- data.frame(varname=out$Variable, importance=out$x)
  rownames(out2) <- NULL
  # ratio
  out2[,"ratio"] <- out2[,"importance"] / sum(out2[,"importance"])
  if(T) out2[,"ratio"] <- round(out2[,"ratio"], 3)
  # sort
  out2 <- out2[order(out2[,"importance"], decreasing=T),]
  # no
  no <- c(1:nrow(out2))
  out2 <- cbind(no, out2)
  
  return(out2)
}
#=== [END]:varImp.rpart ===


############################################

# @name  : plotVarImp
# @brief : 変数重要度プロット
plotVarImp <- function(df, titleLabel, outFilePath, width, height, 
                       fontsize, face, fill="gray", hjust=0.5)
{
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try ({
    if(T){
      p <- ggplot(df, aes(x=reorder(id, value), y=value))
      p <- p + geom_bar(stat="identity", position="dodge", fill=fill)
      p <- p + xlab("") + ylab("variable importance")
      p <- p + theme(axis.text.x=element_text(size=fontsize, face=face),
                     axis.text.y=element_text(size=fontsize, face=face),
                     plot.title=element_text(size=18, face=face))
      p <- p + geom_text(aes(label=paste(df[,"value2"],sep="")), hjust=hjust, color="black", size=7)
      p <- p + coord_flip()
      p <- p + ggtitle(titleLabel)
      p <- p + theme(plot.title=element_text(hjust=hjust))
    }
    plot(p)
  }, TRUE)
  dev.off()
}
#=== [END]:plotVarImp ===



############################################


# @name : plotVarImp_topN
plotVarImp_topN <- function(topN, df, outFilePath, titleLabel) 
{
  # settings
  if(T){
    CNST_HEIGHT <- 1200
    CNST_WIDTH <- CNST_HEIGHT * 0.8
    if(T){
      #CNST_FONT_SIZE <- 16
      CNST_FONT_SIZE <- 14
    }
  }
  
  if(T){
    # with RankID
    strID <- round(rank(-df[,"value"]), 0)
    strID <- paste("0",strID, sep="")
    strID <- substr(strID, nchar(strID)-1, nchar(strID))
    df[,"id"] <- paste(strID, " ", df[,"id"], sep="")
    
    # plotVarImp()
    plotVarImp(df=df[c(1:topN),], titleLabel=titleLabel, outFilePath=outFilePath,  
               width=CNST_WIDTH, height=CNST_HEIGHT, fontsize=CNST_FONT_SIZE, 
               face="plain", fill="gray", hjust=0.5)
  }
}
#=== [END]:plotVarImp_topN ===




# @name : stratified_sampling_lstForm
# @brief : 層別サンプリング
stratified_sampling_lstForm <- function(df, vecStrataList, numOfSamplingRatio, isDeleteKey=F)
{
  nTotal <- nrow(df)
  numOfStrataList <- length(vecStrataList)
  colList_df <- colnames(df) # 元カラム
  df$id <- c(1:nTotal) # 元ID(tmp変数)
  
  tbl <- table(df[,vecStrataList]) # 層別カウント(実績)
  tbl_sampling <- round(tbl*numOfSamplingRatio, 0) # 層別サンプル数(出力)
  vecSizeListByStrata <- as.numeric(t(tbl_sampling)) # 層別サンプル数(unlist形式)
  
  # 事前に層別変数でソートしておく
  for(j in (numOfStrataList:1)){
    df <- df[order(df[,vecStrataList[j]], decreasing=F),]
  }
  
  # sampling::strata()関数
  idx_strata <- sampling::strata(data=df, stratanames=vecStrataList,
                                 size=vecSizeListByStrata, method="srswor")
  
  # sampling::getdata()関数
  df_samping <- sampling::getdata(data=df, m=idx_strata)[,colnames(df)] # "id"も含む
  
  # 最後に元データ順に戻す(sort by 元ID)
  df_samping <- df_samping[(order(df_samping[,"id"], decreasing=F)),]
  
  # 元カラムのみ
  if(isDeleteKey) df_samping <- df_samping[,colList_df]
  
  numOfSamping <- nrow(df_samping)
  numOfSampingRatio_output <- numOfSamping / nTotal
  
  # summaryInfo
  sm <- data.frame("nTotal"=nTotal, "numOfSamping"=numOfSamping, 
                   "numOfSampingRatio_output"=numOfSampingRatio_output, 
                   "numOfSampingRatio_input"=numOfSamplingRatio)
  
  # 層別サンプリング比率
  ratioByStrata <- round(tbl_sampling/tbl, 4)
  tbl_sampling_fmt <- format(tbl_sampling, big.mark=",")
  tbl_fmt <- format(tbl, big.mark=",")
  ratioByStrata_withNumOfSample <- paste(ratioByStrata, "(", tbl_sampling_fmt, "/", tbl_fmt, ")", sep="")
  ratioByStrata_withNumOfSample <- gsub(" ", "", ratioByStrata_withNumOfSample)
  
  # 出力リスト
  if(T){
    res <- list()
    res[[1]] <- df_samping
    res[[2]] <- sm
    res[[3]] <- tbl
    res[[4]] <- tbl_sampling
    res[[5]] <- ratioByStrata
    res[[6]] <- ratioByStrata_withNumOfSample
    names(res) <- c("df_samping", "summaryInfo", "tblByStrata", "tblByStrata_sampling",
                    "ratioByStrata", "ratioByStrata_withNumOfSample")
  }
  
  return(res)
}
#=== [END]:stratified_sampling_lstForm ===



# @name : CalcROCInfo_ROCR
# @brief : ROC情報の計算
CalcROCInfo_ROCR <- function(x, y, isPlot=T)
{
  n <- length(x)
  pred <- prediction(x, y)
  pref <- performance(pred, "tpr", "fpr")
  
  # AUC
  auc <- as.numeric(performance(pred, "auc")@y.values[[1]])
  
  # ROC Table
  ROCTbl <- data.frame(
    Cutoff=unlist(pred@cutoffs), TP=unlist(pred@tp), FP=unlist(pred@fp),
    FN=unlist(pred@fn), TN=unlist(pred@tn), 
    TPR=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),
    FPR=1.0-unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),
    Accuracy=((unlist(pred@tp)+unlist(pred@tn))/n)
  )
  
  rownames(ROCTbl) <- NULL
  
  if(isPlot){
    plot(pref)
    abline(a=0, b=1, col="black")
    abline(h=(1:10)/10, v=(1:10)/10, lty=3)
    par(new=TRUE)
    plot(ROCTbl$FPR, ROCTbl$TPR, xlab="FPR", ylab="TPR")
  }
  
  res <- list()
  res[[1]] <- auc
  res[[2]] <- ROCTbl
  names(res) <- c("AUC", "ROCTable")
  
  return(res)
}
#=== [END]:CalcROCInfo_ROCR ===


# @name  : PlotROCCurve
# @brief : ROC曲線の描画(ggplot)
PlotROCCurve <- function(outFilePath, ROCTbl_train, ROCTbl_valid, mainTitle, subTitle, 
                         colourList=c("black","black","black"), width, height, fontsize, AUC=NA,
                         legendList=c("Train", "Valid"))
{
  # settings
  minVal <- 0.0
  maxVal <- 1.0
  nDiv <- 5
  
  limit_ <- c(minVal, maxVal)
  width_ <- (maxVal - minVal) / nDiv
  break_ <- seq(minVal, maxVal, by=width_)
  strBreak <- paste(round(break_*100, 0), "%", sep="")
  
  # (x,y)に変更
  df_train <- data.frame(x=ROCTbl_train$FPR, y=ROCTbl_train$TPR)
  df_valid <- data.frame(x=ROCTbl_valid$FPR, y=ROCTbl_valid$TPR)
  
  if(T){
    df_train <- round(df_train[], 2)
    df_valid <- round(df_valid[], 2)
  }
  df_abline <- data.frame("x"=limit_, "y"=limit_)
  
  strLegendList <- c("Train", "Valid")
  
  png(outFilePath, width=width, height=height)
  par(xpd=NA)
  try ({
    if(T){
      
      # 1=train
      if(T){
        p <- ggplot(df_train)
        p <- p + geom_path(aes(x,y,color=strLegendList[1],linetype=strLegendList[1]), size=0.9, show.legend=T)
      }
      
      # 2=valid
      p <- p + geom_line(data=df_valid, aes(x,y,color=strLegendList[2],linetype=strLegendList[2]), size=1.1, show.legend=T)
      p <- p + geom_path(data=df_abline, aes(x,y), size=0.5, colour="gray", show.legend=F)
      p <- p + theme_gray(base_family="", base_size=fontsize)
      
      # legend.box(凡例)
      p <- p + theme(legend.box="horizontal", 
                     legend.position=c(0.87, 0.2),
                     legend.key=element_blank(), 
                     legend.background=element_rect(fill="transparent", colour=NA),
                     legend.box.margin=margin(0,0,0,0),
                     axis.text.x=element_text(colour="black",size=16, angle=0, hjust=c(0,0.5,0.5,0.5,0.5,1), vjust=-1, face="plain"),
                     axis.text.y=element_text(colour="black",size=16, angle=0, hjust=1, vjust=c(0,0.5,0.5,0.5,0.5,1), face="plain")
      )
      # scale_x
      if(T){
        p <- p + scale_x_continuous("FP ratio (False Positive)", labels=scales::percent, limits=c(0, 1.0),
                                    expand=c(0,0), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
        p <- p + expand_limits(x=0.0)
      }
      # scale_y
      if(T){
        p <- p + scale_y_continuous("TP ratio (True Positive)", labels=scales::percent, limits=c(0, 1.0),
                                    expand=c(0,0), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
      }
      if(T){
        p <- p + scale_color_manual("", breaks=c(1,2), values=c("black","black"))
        p <- p + scale_linetype_manual("", values=c("dashed","solid"))
        p <- p + guides(linetype=guide_legend(keywidth=4.5, keyheight=1.0))
        p <- p + ggtitle(label=mainTitle)
        p <- p + theme(plot.title=element_text(size=20, hjust=0.5))
      }
      # annotate
      if(!is.na(AUC)) p <- p + annotate("text", x=0.5, y=0.5, label=round(AUC,2),size=24)
    }
    plot(p)
  }, TRUE)
  dev.off()
}  
#=== [END]:PlotROCCurve ===


# ランダムフォレスト(ranger用): 変数重要度データの加工
# n=変数の数, id=変数名, value=変数重要度, 
# total=変数重要度の合計値, ratio=変数重要度の構成比
convertVarImp_rangerRF <- function(rf, isSort=T, isNormalize=T)
{
  Imp <- ranger::importance(rf)
  
  if(T){
    n <- length(Imp)
    value <- as.numeric(Imp)
    total <- sum(Imp)
    ratio <- value/total
  }
  
  # Normalize
  if(isNormalize) {
    value <- ratio
    total <- sum(value)
  }
  
  df <- data.frame("n"=n, "id"=names(Imp), "value"=value, 
                   "total"=total, "ratio"=ratio, "type"="rangerRF")
  
  # sort
  if(isSort) df <- df[order(df[,"value"], decreasing=T),]
  
  no <- c(1:nrow(df))
  df <- cbind(no, df)
  df$id <- factor(df$id, levels=df$id[order(df$value)])
  
  return(df)
}



#=== [END]:R-Script ===
