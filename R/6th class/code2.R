rm(list=ls())
library(tibble)
library(readxl)
library(lawstat)
y=read_excel("C:/Users/there/Desktop/survey/data/leg3deg.xlsx",sheet="Sheet1",range="A1:J56",col_names=TRUE,col_types="guess",na="")
gp=y[[1]]
y1=y[[2]]
y2=y[[3]]
y3=y[[4]]
y4=y[[5]]
y5=y[[6]]
y6=y[[7]]
y7=y[[8]]
y8=y[[9]]
y9=y[[10]]
yy=c(y1,y2,y3,y4,y5,y6,y7,y8,y9)
ym=as.matrix(y[,2:10])
ggp=rep(gp,9)
dat=data.frame(yy,gp)
dat$gp<-factor(dat$gp,labels=c("남자","여자"))
attach(dat)
boxplot(yy~gp,data=dat,main="성별 간 자료분포",xlab="성별",ylab="점수")
levene.test(yy,ggp)
bartlett.test(yy~gp,dat)
shapiro.test(yy)
wilcox.test(yy~gp,dat)

