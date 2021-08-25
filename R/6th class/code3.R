rm(list=ls())
library(tibble)
library(readxl)
library(psych)
y=read_excel("C:/Users/there/Desktop/survey/data/leg3deg.xlsx",sheet="Sheet1",range="B1:J56",col_names=TRUE,col_types="guess",na="")
xd=list()
y1=y[[1]]
y2=y[[2]]
y3=y[[3]]
y4=y[[4]]
y5=y[[5]]
y6=y[[6]]
y7=y[[7]]
y8=y[[8]]
y9=y[[9]]
yy=c(y1,y2,y3,y4,y5,y6,y7,y8,y9)
yd=as.data.frame(y)
count=0;
for (i in 1:8){
	for (j in i:8){
		count=count+1;
		xd[[count]] = cbind(yd[,i],yd[,j+1]);
		ss = as.matrix(xd[[count]]);
		print(cor.test(ss[,1],ss[,2],method='spearman'))}}
alpha(yd)

