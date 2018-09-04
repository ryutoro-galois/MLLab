# utilty.R
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
      if(F){
        prp(model, main=titleLabel, cex.main=2.0, type=2, extra=106, tweak=1.5, digits=3, varlen=0, faclen=0,
            nn=T, ni=F, yesno=1, compress=T, ycompress=T, fallen.leaves=F, xflip=F, yflip=F,
            Margin=0, gap=5.0, leaf.round=0, facsep=",", nn.round=0, 
            box.col=c("grey","white")[box.col_yval2], shadow.col="grey", under=T)
      }
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


#=== [END]:R-Script ===
