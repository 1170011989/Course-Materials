rm(list=ls())
library(tibble)
library(readxl)
library(lawstat)
y=read_excel("C:/Users/there/Desktop/survey/data/arm.xlsx",sheet="Sheet1",range="A1:J111",col_names=TRUE,col_types="guess",na="")
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
dat=data.frame(y1,gp)
dat$gp<-factor(dat$gp,labels=c("2학년","3학년"))
attach(dat)
boxplot(y1~gp,data=dat,main="성별 간 자료분포",xlab="성별",ylab="목표/기대수준")
levene.test(y1,gp)
bartlett.test(y1~gp,dat)
shapiro.test(y1)
wilcox.test(y1~gp,dat)

