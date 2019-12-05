#Key Code for computation of range based volatility persistence using BIC information


range_base2=function(x,y){
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  y=subset(y,y$date>="2010-01-01")
  x=subset(x,x$date<="2017-12-31")
  y=subset(y,y$date<="2017-12-31")
  mm=length(x[,1])
  y=data.frame(y,logvola=log(y$vola))
  per100=c(rep(NA,mm))
  order100=c(rep(NA,mm))
  code1=x$code[1]
  date1=x$date[1]
  yyear=year(date1)
  set1=subset(y,y$code==code1)
  set12=subset(set1,year(set1$date)==yyear)
  set12=subset(set12,set12$logvola!=-Inf)
  set12=subset(set12,set12$logvola!=Inf)
  da=set12$logvola
  da=na.omit(da)
  if (length(set12[,1])>=100){
    a=SelectModel(da,lag.max=22,ARModel = c("AR"), Criterion = "default")
    lag=a[length(a[,1]),1]
    b=ar(da,aic=FALSE,order.max=lag)
    per100[1]=sum(b$ar)
    order100[1]=lag
  }
  for (i in 2:length(x[,1])) {
    if (x$code[i]==x$code[i-1]&year(x$date[i])==year(x$date[i-1])){
      per100[i]=per100[i-1]
      order100[i]=order100[i-1]
    } else{
      code1=x$code[i]
      date1=x$date[i]
      yyear=year(date1)
      set1=subset(y,y$code==code1)
      set12=subset(set1,year(set1$date)==yyear)
      set12=subset(set12,set12$logvola!=-Inf)
      set12=subset(set12,set12$logvola!=Inf)
      da=set12$logvola
      da=na.omit(da)
      if (length(set12[,1])>=100){
        a=SelectModel(da,lag.max=22,ARModel = c("AR"), Criterion = "default")
        lag=a[length(a[,1]),1]
        if (lag==0){
          m1=lm(logvola~lag(logvola),data=set12)
          m2=lm(logvola~lag(logvola)+lag(logvola,2),data=set12)
          m3=lm(logvola~lag(logvola)+lag(logvola,2)+lag(logvola,3),data=set12)
          m4=lm(logvola~lag(logvola)+lag(logvola,2)+lag(logvola,3)+lag(logvola,4),data=set12)
          m5=lm(logvola~lag(logvola)+lag(logvola,2)+lag(logvola,3)+lag(logvola,4)+lag(logvola,5),data=set12)
          model=list(m1,m2,m3,m4,m5)
          bic=c(BIC(m1),BIC(m2),BIC(m3),BIC(m4),BIC(m5))
          s=which(bic==min(bic))
          best=model[[s]]
          per100[i]=sum(best$coefficients[-1])
          order100[i]=s
        } else{
          b=ar(da,aic=FALSE,order.max=lag)
          per100[i]=sum(b$ar)
          order100[i]=lag
        }
      }
    }
  }
  new=data.frame(x,per100,order100)
  return(new)
}

range_sh_2=range_base2(range_sh,sh.all3[[1]])
range_sz_2=range_base2(range_sz,sz.all3[[1]])


#include turnover data
add_turn=function(x){
  turn=x$volume/x$cap
  turn=turn/1000
  x=data.frame(x,turn)
  return(x)
}

sh.all4=lapply(sh.all3, add_turn)
sz.all4=lapply(sz.all3, add_turn)
#include other data
no_owner2=function(x,y){
  y$date=as.Date(as.character(y$date),format="%Y-%m-%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  y$cap=y$cap*1000
  dturn=y$volume/y$cap
  y=data.frame(y,dturn)
  mturn=c()
  mmturn=c()
  illiq=c()
  size=c()
  for (i in 1:length(x$code)){
    a=subset(y,y$code==x[i,1])
    yyear=year(x[i,2])
    month=month(x[i,2])
    da=subset(a,year(a$date)==yyear&month(a$date)==month)
    mturn[i]=mean(da$dturn)
    mmturn[i]=sum(da$volume)/sum(da$cap)
    illiq[i]=1/sum(abs(da$return)/da$volume)
    size[i]=log(da$cap[length(da[,1])])
  }
  x=data.frame(x,mturn,mmturn,illiq,size)
  return(x)
}

range_sh2=no_owner2(range_sh_2,sh.all4[[1]])
range_sz2=no_owner2(range_sz_2,sz.all4[[1]])

head(range_sh_2)


shfinal4=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal4.csv")
szfinal4=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal4.csv")
head(range_sh_2)
head(shfinal4)

manage=function(x,y){
  x=x[,-1]
  y$date=as.Date(as.character(y$date),format="%Y-%m-%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  ll=length(x[,1])
  p1=c(rep(NA,ll))
  l1=c(rep(NA,ll))
  p2=c(rep(NA,ll))
  l2=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    date1=x$date[i]
    set1=subset(y,y$code==code1&y$date==date1)
    if (is.na(set1$per100[1])){
      p1[i]=NA
      l1[i]=NA
    } else{
      p1[i]=set1$per100[1]
      l1[i]=set1$order100[1]
    }
    if (is.na(set1$per200[1])){
      p2[i]=NA
      l2[i]=NA
    } else{
      p2[i]=set1$per200[1]
      l2[i]=set1$order200[1]
    }
  }
  new=data.frame(x,p1,l1,p2,l2)
  return(new)
}

shfinal5=manage(shfinal4,range_sh_2)
szfinal5=manage(szfinal4,range_sz_2)

write.csv(shfinal5,file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal55.csv")
write.csv(szfinal5,file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal55.csv")

shfinal5=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal55.csv")
szfinal5=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal55.csv")

#include book to market data
btm=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\booktomarket.csv")

btm2=code_completion(btm)

btm2$date=as.Date(btm2$date,format="%Y/%m/%d")
btm3=arrange(btm2,code,date)
head(btm3)
matbtm=function(x,y){
  x=x[,-1]
  y$date=as.Date(as.character(y$date),format="%Y-%m-%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  ll=length(x[,1])
  ratio1=c(rep(NA,ll))
  ratio2=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      ratio1[i]=set1$ratioA[ttt]
      ratio2[i]=set1$ratioB[ttt]
    } else {
      ratio1[i]=NA
      ratio2[i]=NA
    }
  }
  new=data.frame(x,ratio1,ratio2)
  return(new)
}

shfinal6=matbtm(shfinal5,btm3)
szfinal6=matbtm(szfinal5,btm3)

write.csv(shfinal6,file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal66.csv")
write.csv(szfinal6,file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal66.csv")

shfinal6=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal66.csv")
szfinal6=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal66.csv")

#regression
library(plm)
library(stargazer)
p1=plm(p2~size+yturn+yvola+mb+o1+o1:soe
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=shfinal831,model="within",index=c("id"))
p2=plm(p2~size+yturn+yvola+mb+o1+o1:soe
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=szfinal831,model="within",index=c("id"))
haha=stargazer(p1,p2,align = T,type="html",out="soe.htm")
shfinal831=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\shfinal831.csv")
szfinal831=read.csv(file="C:\\Users\\qsb15138\\Desktop\\perperper\\szfinal831.csv")


p2=plm(p2~size+yturn+yvola+o5
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=shfinal5,model="within",index=c("id"))
p3=plm(p2~size+yturn+yvola+o10
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=shfinal5,model="within",index=c("id"))
p4=plm(p2~size+yturn+yvola+o1
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=szfinal5,model="within",index=c("id"))
p5=plm(p2~size+yturn+yvola+o5
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=szfinal5,model="within",index=c("id"))
p6=plm(p2~size+yturn+yvola+o10
       +y2010+y2011+y2012+y2013+y2014+y2015+y2016+y2017,
       data=szfinal5,model="within",index=c("id"))


