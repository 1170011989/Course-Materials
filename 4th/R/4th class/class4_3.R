# 함수 mean2.ci() 실행 (표본평균, 표준편차, 표본개수 입력)
mean2.ci(xb1=198.5, xb2=201.3, s1=5, s2=5, n1=25, n2=34, 
	pvar="known")


# mean2test.plot() 실행 (표본평균, 표준편차, 표본개수 입력)
mean2test.plot(xb1=198.5, xb2=201.3, s1=5, s2=5, n1=25, n2=34, 
	pvar="known")



# 모평균 차이의 신뢰구간 (모분산을 모르지만 같은 경우)
# 함수 mean2.ci() 실행 (표본평균, 표본표준편차, 표본개수 입력)
mean2.ci(xb1=198.5, xb2=201.3, s1=4.8, s2=5.1, n1=25, n2=34, 
	pvar="equal")



# 저장된 데이터 셋(exa11_4)을 불러옴
data(exa11_4); str(exa11_4)

x = exa11_4$x; y = exa11_4$y
mean(x); sd(x);  mean(y); sd(y)

# 기본 함수 t.test() 사용 (등분산 가정) ⇒ 앞의 풀이에 반올림 오차
t.test(x, y, var.equal=T)$conf[1:2]

# 등분산 가정을 하지 않은 경우 ⇒ 다소 폭이 좁아짐
t.test(x, y)$conf[1:2]


# 모평균 차이 신뢰구간 함수 mean2.ci() 사용
mean2.ci(x, y, pvar="equal")



# mean2test.plot(표본평균, 표본표준편차, 표본개수) 실행
mean2test.plot(xb1=198.5, xb2=201.3, s1=4.8, s2=5.1, n1=25, n2=34, 
	pvar="equal")



# 데이터 입력, 함수 mean2test.plot() 사용
mean2test.plot(x, y, pvar="equal")



# 모평균 차이에 대한 신뢰구간 (모분산을 모르며 다른 경우)
# 함수 mean2.ci(표본평균, 표본표준편차, 표본개수) 실행
mean2.ci(xb1=198.5, xb2=201.3, s1=2.8, s2=5.5, n1=25, n2=34, 
	pvar="unequal")



# 저장된 데이터 셋(exa11_8)을 불러옴 & mean2.ci() 사용
data(exa11_8); x = exa11_8$x; y = exa11_8$y
mean2.ci(x, y, pvar="unequal")


# 데이터 사전조사
win.graph(7, 5)
boxplot(list(라인1=x, 라인2=y), col=c(7, "cyan"), boxwex=0.6, 
	horizontal=T, main="두 라인에서 생산된 초콜릿 무게의 분포")
# 데이터 점 표시
points(x, rep(1,length(x))); points(y, rep(2,length(y)))
# 통계량 계산 및 표시 fivenum()  ⇒ (최소값, Q1, Q2, Q3, 최대값)
xfn = fivenum(x); yfn = fivenum(y)
text(xfn, 0.6, labels=xfn, cex=0.9, col=4)
text(yfn, 1.6, labels=yfn, cex=0.9, col=4)


# 데이터 입력
# [참고] R 기본함수 t.test() 사용 (등분산 가정 안함)
xo2 = t.test(x, y)
cat("Stat =", round(xo2$stat, 4), "\tP-v =", round(xo2$p.val, 5), "\n")



# 두 모평균 차이에 대한 검정 (모분산을 모르며 다른 경우)
mean2test.plot(x, y, pvar="unequal")



# 저장된 데이터 셋(exa11_10)을 불러옴
data(exa11_10)
x = exa11_10$x; y = exa11_10$y
d = x-y; mean(d); sd(d)


# 단일 모집단 모평균에 대한 신뢰구간 pmean.ci2() 함수 (10장)
pmean.ci2(d)


# 단일 모집단 모평균에 대한 가설검정 meantest2.plot() 함수 (10장)
meantest2.plot(d, mu0=0)




# 함수 prob2.ci() 실행
prob2.ci(n1=160, x1=12, n2=200, x2=13, alp=0.1)


# [참고] 유의수준 0.05인 경우 ⇒ 신뢰구간의 폭이 넓어짐
prob2.ci(n1=160, x1=12, n2=200, x2=13, alp=0.05)



# 함수 prob2test.plot() 실행
prob2test.plot(n1=150, x1=12, n2=250, x2=10, side="up", dig=4)



# 데이터 입력
data(exa11_8); x = exa11_8$x; y = exa11_8$y
# 모분산비에 대한 신뢰구간 ⇒ 기본함수 var.test( ) 사용
round(var.test(x, y)$conf[1:2], 4)



# var.test( ) 함수 실행 및 결과 정리
vo = var.test(x, y)
cat("자유도 =", vo$para, "\tF0 =", round(vo$stat, 4), "\tP-v =", round(vo$p.val, 4), 
"\n95% 신뢰구간 =",paste0("[",round(vo$conf[1], 4),", ", round(vo$conf[2], 4),"]\n"))

# 함수 ftest.plot() 실행
ftest.plot(fstat=vo$stat, deg=as.vector(vo$para), pmax=0.9999, side="two")


# 모의실험 함수 civar2.sim() 실행
civar2.sim(n1=25, n2=16, sig1=sqrt(8), sig2=2)

# 모의실험 10,000회 반복
civar2.sim(n1=25, n2=16, sig1=sqrt(8), sig2=2, N=10000, plot=F)


