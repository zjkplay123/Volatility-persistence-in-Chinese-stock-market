
#descriptive statistics

per_table=function(x){
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  dm=c()
  for (i in 1:length(x[,1])){
    if (x$per[i]<median(x$per)){
      dm[i]=1
    } else{
      dm[i]=0
    }
  }
  x=data.frame(x,dm)
  k=matrix(NA,nrow=21,ncol=8)
  k[1,1:8]=c(summary(x$per),sd(x$per),length(x$per))
  hper=subset(x,x$dm==1)$per
  lper=subset(x,x$dm==0)$per
  k[2,1:8]=c(summary(hper),sd(hper),length(hper))
  k[3,1:8]=c(summary(lper),sd(lper),length(lper))
  
  k[4,1:8]=c(summary(x$size),sd(x$size),length(x$size))
  hsize=subset(x,x$dm==1)$size
  lsize=subset(x,x$dm==0)$size
  k[5,1:8]=c(summary(hsize),sd(hsize),length(hsize))
  k[6,1:8]=c(summary(lsize),sd(lsize),length(lsize))
  
  k[7,1:8]=c(summary(x$yturn),sd(x$yturn),length(x$yturn))
  hturn=subset(x,x$dm==1)$yturn
  lturn=subset(x,x$dm==0)$yturn
  k[8,1:8]=c(summary(hturn),sd(hturn),length(hturn))
  k[9,1:8]=c(summary(lturn),sd(lturn),length(lturn))
  
  k[10,1:8]=c(summary(x$yvola),sd(x$yvola),length(x$yvola))
  hyvola=subset(x,x$dm==1)$yvola
  lyvola=subset(x,x$dm==0)$yvola
  k[11,1:8]=c(summary(hyvola),sd(hyvola),length(hyvola))
  k[12,1:8]=c(summary(lyvola),sd(lyvola),length(lyvola))
  
  k[13,1:8]=c(summary(x$o1),sd(x$o1),length(x$o1))
  ho1=subset(x,x$dm==1)$o1
  lo1=subset(x,x$dm==0)$o1
  k[14,1:8]=c(summary(ho1),sd(ho1),length(ho1))
  k[15,1:8]=c(summary(lo1),sd(lo1),length(lo1))
  
  k[16,1:8]=c(summary(x$o5),sd(x$o5),length(x$o5))
  ho5=subset(x,x$dm==1)$o5
  lo5=subset(x,x$dm==0)$o5
  k[17,1:8]=c(summary(ho5),sd(ho5),length(ho5))
  k[18,1:8]=c(summary(lo5),sd(lo5),length(lo5))
  
  k[19,1:8]=c(summary(x$o10),sd(x$o10),length(x$o10))
  ho10=subset(x,x$dm==1)$o10
  lo10=subset(x,x$dm==0)$o10
  k[20,1:8]=c(summary(ho10),sd(ho10),length(ho10))
  k[21,1:8]=c(summary(lo10),sd(lo10),length(lo10))
  
  rownames(k)=c(rep(c("Whole Sample","persistence<median","persistence>=median"),7))
  colnames(k)=c("min","25th","Median","Mean","75th","Max","Standard Deviation","N")
  return(k)
}

