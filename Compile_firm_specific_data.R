#Add firm characteristic data such as size, BTM, turnover
#important 831
shfinal831=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\shfinal831.csv")
szfinal831=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\szfinal831.csv")

shfinal832=shfinal831[,c(-1,-4,-(6:21))]
szfinal832=szfinal831[,c(-1,-4,-(6:21))]

names(shfinal831)
#import lot of data
ANA=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\ANA.csv")
FOR=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\FOR.csv")
GOV=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\GOV.csv")
LEV=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\LEV.csv")
MTB=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\MTB.csv")
NI=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\NI.csv")
ROA=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\ROA.csv")
ROE=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\ROE.csv")
OWN=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\OWN.csv")
TURN=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\variables\\TURN.csv")

#LEV
LEV2=code_completion(LEV)
head(LEV2)
as.Date(LEV2$date[1],format="%Y/%m/%d")
flev=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  lev=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      lev[i]=set1$LEV[ttt]
    } else {
      lev[i]=NA
    }
  }
  new=data.frame(x,lev)
  return(new)
}
shfinal833=flev(shfinal832,LEV2)
szfinal833=flev(szfinal832,LEV2)

#ROA
ROA2=code_completion(ROA)
froa=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  roa=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      roa[i]=set1$ROA[ttt]
    } else {
      roa[i]=NA
    }
  }
  new=data.frame(x,roa)
  return(new)
}
shfinal834=froa(shfinal833,ROA2)
szfinal834=froa(szfinal833,ROA2)

head(shfinal838)

#ROE
ROE2=code_completion(ROE)
froe=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  roe=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      roe[i]=set1$ROE[ttt]
    } else {
      roe[i]=NA
    }
  }
  new=data.frame(x,roe)
  return(new)
}
shfinal835=froe(shfinal834,ROE2)
szfinal835=froe(szfinal834,ROE2)

#OWN
OWN2=code_completion(OWN)
fown=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  own1=c(rep(NA,ll))
  own3=c(rep(NA,ll))
  own5=c(rep(NA,ll))
  own10=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      own1[i]=set1$OWN1[ttt]
      own3[i]=set1$OWN3[ttt]
      own5[i]=set1$OWN5[ttt]
      own10[i]=set1$OWN10[ttt]
    } else {
      own1[i]=NA
      own3[i]=NA
      own5[i]=NA
      own10[i]=NA
    }
  }
  new=data.frame(x,own1,own3,own5,own10)
  return(new)
}
shfinal836=fown(shfinal835,OWN2)
szfinal836=fown(szfinal835,OWN2)

#GOV
GOV2=code_completion(GOV)
fgov=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  gov=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    year2=year1-1
    set1=subset(y,y$code==code1&year(y$date)==year2)
    ttt=length(set1[,1])
    if (ttt!=0){
      gov[i]=set1$GOV[ttt]
    } else {
      gov[i]=NA
    }
  }
  new=data.frame(x,gov)
  return(new)
}
shfinal837=fgov(shfinal836,GOV2)
szfinal837=fgov(szfinal836,GOV2)

head(ANA2)

#ANA
ANA2=code_completion(ANA)
for (i in 1:length(ANA2[,1])) {
  if (is.na(ANA2$ANA[i])){
    ANA2$ANA[i]=0
  }
}
head(shfinal836)

fana=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  y=y[,-6]
  ll=length(x[,1])
  ana=c(rep(NA,ll))
  big4=c(rep(NA,ll))
  outside=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    set1=subset(y,y$code==code1&year(y$date)==year1)
    ttt=length(set1[,1])
    if (ttt!=0){
      ana[i]=set1$ANA[ttt]
      if (set1$Big4[ttt]=="Y"){
        big4[i]=1
      } else {
        big4[i]=0
      }
      if (set1$Outside[ttt]=="Y"){
        outside[i]=1
      } else {
        outside[i]=0
      }
    } else {
      ana[i]=NA
      big4[i]=NA
      outside[i]=NA
    }
  }
  new=data.frame(x,ana,big4,outside)
  return(new)
}
shfinal838=fana(shfinal837,ANA2)
szfinal838=fana(szfinal837,ANA2)

#FOR
FOR2=code_completion(FOR)
head(FOR2)
for (j in 3:5) {
  for (i in 1:length(FOR2[,1])) {
    if (FOR2[i,j]!=0){
      FOR2[i,j]=1
    }
  }
}

ffor=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  ash=c(rep(NA,ll))
  bsh=c(rep(NA,ll))
  hsh=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    set1=subset(y,y$code==code1&year(y$date)==year1)
    ttt=length(set1[,1])
    if (ttt!=0){
      ash[i]=set1$ASH[ttt]
      bsh[i]=set1$BSH[ttt]
      hsh[i]=set1$HSH[ttt]
    } else {
      ash[i]=NA
      bsh[i]=NA
      hsh[i]=NA
    }
  }
  new=data.frame(x,ash,bsh,hsh)
  return(new)
}
shfinal839=ffor(shfinal838,FOR2)
szfinal839=ffor(szfinal838,FOR2)


ffor2=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    if (year1==2010){
      set1=subset(y,y$code==code1&y$date<=x$date[i])
      ttt=length(set1[,1])
      if (ttt!=0) {
        if (is.na(x$ash[i])){
          x$ash[i]=set1$ASH[ttt]
          x$bsh[i]=set1$BSH[ttt]
          x$hsh[i]=set1$HSH[ttt]
        } 
      }
    }
  }
  return(x)
}

shfinal840=ffor2(shfinal839,FOR2)
szfinal840=ffor2(szfinal839,FOR2)

ffor3=function(x){
  ll=length(x[,1])
  for (i in 2:ll) {
    if (x$code[i]==x$code[i-1]){
      aa=subset(x,x$code==x$code[i])
      if (length(aa[,1]!=0)){
        if (is.na(x$ash[i])){
          x$ash[i]=x$ash[i-1]
          x$bsh[i]=x$bsh[i-1]
          x$hsh[i]=x$hsh[i-1]
        }
      }
    }
  }
  return(x)
}

shfinal841=ffor3(shfinal840)
szfinal841=ffor3(szfinal840)

#MTB
MTB2=code_completion(MTB)
fmtb=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  mtb=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    set1=subset(y,y$code==code1&year(y$date)==year1)
    ttt=length(set1[,1])
    if (ttt!=0){
      mtb[i]=set1$MTB[ttt]
    } else {
      mtb[i]=NA
    }
  }
  new=data.frame(x,mtb)
  return(new)
}
shfinal842=fmtb(shfinal841,MTB2)
szfinal842=fmtb(szfinal841,MTB2)

#TURN NEED FIXED!!!
TURN2=code_completion(TURN)

head(TURN2)

fturn=function(x,y){
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,nyear)
  ll=length(x[,1])
  turn=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    set1=subset(y,y$code==code1&y$nyear==year1)
    ttt=length(set1[,1])
    if (ttt!=0){
      turn[i]=set1$volumn[ttt]
    } else {
      turn[i]=NA
    }
  }
  new=data.frame(x,turn)
  return(new)
}
shfinal8421=fturn(shfinal842,TURN2)
szfinal8421=fturn(szfinal842,TURN2)

cc4=fturn(cc3,TURN2)

cc5$turn=cc5$turn/exp(cc5$size)

#NI NEED FIXED!!!
NI2=code_completion(NI)
fni=function(x,y){
  y$date=as.Date(y$date,format="%Y/%m/%d")
  x$code=as.character(x$code)
  y$code=as.character(y$code)
  x$date=as.Date(as.character(x$date),format="%Y-%m-%d")
  y=arrange(y,code,date)
  ll=length(x[,1])
  ni=c(rep(NA,ll))
  for (i in 1:ll) {
    code1=x$code[i]
    year1=year(x$date[i])
    set1=subset(y,y$code==code1&year(y$date)==year1)
    ttt=length(set1[,1])
    if (ttt!=0){
      ni[i]=set1$NI[ttt]
    } else {
      ni[i]=NA
    }
  }
  new=data.frame(x,ni)
  return(new)
}


shfinal8431=fni(shfinal8421,NI2)
szfinal8431=fni(szfinal8421,NI2)

write.csv(shfinal8431,file="C:\\Users\\zjkplay123\\Desktop\\perperper\\shfinal8431.csv")
write.csv(szfinal8431,file="C:\\Users\\zjkplay123\\Desktop\\perperper\\szfinal8431.csv")

shfinal8431=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\shfinal8431.csv")
szfinal8431=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\szfinal8431.csv")



library(plm)
funadj=function(x){
  x$own1=x$own1/100
  x$own3=x$own3/100
  x$own5=x$own5/100
  x$own10=x$own10/100
  x$turn=x$turn/exp(x$size)
  return(x)
}
aa1=funadj(aa)
head(aa1)
aa2=data.frame(aa1,own12=aa1$own1*aa1$own1,own321=aa1$own1-(aa1$own3-aa1$own1))

substring(aa2$code[1],1,1)!=2

funexlu=function(x){
  x=subset(x,substring(x$code,1,1)!=2 & substring(x$code,1,1)!=9)
  return(x)
}


#begin aa7
addquantile=function(x){
  q1=quantile(x$own1,probs=c(0.2,0.4,0.6,0.8))
  q2=quantile(x$size,probs=c(0.2,0.4,0.6,0.8))
  q3=quantile(x$turn,probs=c(0.2,0.4,0.6,0.8))
  qown=c()
  qsize=c()
  qturn=c()
  for (i in 1:length(x[,1])) {
    if (x$own1[i]<=q1[1]){
      qown[i]=1
    } else if (x$own1[i]>q1[1]&x$own1[i]<=q1[2]) {
      qown[i]=2
    } else if (x$own1[i]>q1[2]&x$own1[i]<=q1[3]) {
      qown[i]=3
    } else if (x$own1[i]>q1[3]&x$own1[i]<=q1[4]) {
      qown[i]=4
    } else if (x$own1[i]>q1[4]) {
      qown[i]=5
    }
  }
  for (i in 1:length(x[,1])) {
    if (x$size[i]<=q2[1]){
      qsize[i]=1
    } else if (x$size[i]>q2[1]&x$size[i]<=q2[2]) {
      qsize[i]=2
    } else if (x$size[i]>q2[2]&x$size[i]<=q2[3]) {
      qsize[i]=3
    } else if (x$size[i]>q2[3]&x$size[i]<=q2[4]) {
      qsize[i]=4
    } else if (x$size[i]>q2[4]) {
      qsize[i]=5
    }
  }
  for (i in 1:length(x[,1])) {
    if (x$turn[i]<=q3[1]){
      qturn[i]=1
    } else if (x$turn[i]>q3[1]&x$turn[i]<=q3[2]) {
      qturn[i]=2
    } else if (x$turn[i]>q3[2]&x$turn[i]<=q3[3]) {
      qturn[i]=3
    } else if (x$turn[i]>q3[3]&x$turn[i]<=q3[4]) {
      qturn[i]=4
    } else if (x$turn[i]>q3[4]) {
      qturn[i]=5
    }
  }
  x=data.frame(x,qown,qsize,qturn)
  return(x)
}


#dua listed
hugangtong=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\final2018\\hugangtong.csv")

hugangtongfun=function(x){
  hgt=c()
  yhgt=c()
  x$code=as.character(x$code)
  for (i in 1:length(x[,1])) {
    if (x$code[i] %in% hugangtong$code){
      hgt[i]=1
    } else {
      hgt[i]=0
    }
  }
  yhgt=hgt
  for (i in 1:length(x[,1])) {
    if (substring(x$code[i],8,9)=="SH"){
      if (substring(x$date[i],1,4)==2010|
          substring(x$date[i],1,4)==2011|
          substring(x$date[i],1,4)==2012|
          substring(x$date[i],1,4)==2013|
          substring(x$date[i],1,4)==2014){
        yhgt[i]=0
      }
    }
  }
  for (i in 1:length(x[,1])) {
    if (substring(x$code[i],8,9)=="SZ"){
      if (substring(x$date[i],1,4)==2010|
          substring(x$date[i],1,4)==2011|
          substring(x$date[i],1,4)==2012|
          substring(x$date[i],1,4)==2013|
          substring(x$date[i],1,4)==2014|
          substring(x$date[i],1,4)==2015|
          substring(x$date[i],1,4)==2016){
        yhgt[i]=0
      }
    }
  }
  x=data.frame(x,hgt,yhgt)
  return(x)
}


sh_class=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\classification\\sh_type.csv")
sz_class=read.csv(file="C:\\Users\\zjkplay123\\Desktop\\perperper\\classification\\sz_type.csv")

addsoe=function(x,y){
  csoe=c()
  lsoe=c()
  soe=c()
  y$code=as.character(y$code)
  x$code=as.character(x$code)
  for (i in 1:length(x[,1])) {
    a=x$code[i]
    if (a %in% y$code){
      ha=subset(y,y$code==a)
      if (ha$class[1]=="NSOE"){
        csoe[i]=0
        lsoe[i]=0
        soe[i]=0
      } else if (ha$class[1]=="CSOE"){
        csoe[i]=1
        lsoe[i]=0
        soe[i]=1
      } else if (ha$class[1]=="LSOE"){
        csoe[i]=0
        lsoe[i]=1
        soe[i]=1
      }
    } else {
      csoe[i]=0
      lsoe[i]=0
      soe[i]=0
    }
  }
  x=data.frame(x,csoe,lsoe,soe)
  return(x)
}

Fun_gov=function(x){
  ccsoe=c()
  llsoe=c()
  for (i in 1:length(x[,1])) {
    if (x$gov[i]==0){
      ccsoe[i]=0
      llsoe[i]=0
    } else if (x$gov[i]==1) {
      if (x$csoe[i]==1) {
        ccsoe[i]=1
        llsoe[i]=0
      } else {
        ccsoe[i]=0
        llsoe[i]=1
      }
    }
  }
  x=data.frame(x,ccsoe,llsoe)
  return(x)
}


Fun_id=function(x){
  x$code=as.character(x$code)
  x=subset(x,x$hgt==1)
  id=c()
  id[1]=1
  for (i in 2:length(x[,1])) {
    if (x[i,1]==x[i-1,1]){
      id[i]=id[i-1]
    } else {
      id[i]=id[i-1]+1
    }
  }
  y=data.frame(x,id)
  z=subset(y,y$id==1)
  print(length(z[,1]))
}

