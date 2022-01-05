# 기본함수 chisq.test(x) 사용 (p를 지정하지 않으면 균일하게 배정됨)
x = c(31,26,22,18,13,10); (ct = chisq.test(x))

# 카이제곱 검정 그래프 함수 chitest.plot2() 실행
chitest.plot2(stat=ct$stat, df=ct$para, side="up")



# 관측도수, 예상비율 입력
x = c(32, 65, 47, 38, 18); p = c(0.15, 0.3, 0.25, 0.2, 0.1)
# 기본함수를 이용하여 적합도 검정
(ct = chisq.test(x, p=p))


# 카이제곱 검정 그래프 함수 chitest.plot2() 실행
chitest.plot2(stat=ct$stat, df=ct$para, side="up", ppt=50)


# 저장된 데이터 셋(exa12_3)을 불러옴
data(exa12_3)
# 관측 데이터 정리, table() 함수 사용
(x = table(exa12_3))

 
# 테이블 범주를 수치형으로 변환
y = as.numeric(names(x)); x = as.numeric(x)
# 확률 및 기대도수 계산
py = dpois(y, 4); npy = 80*py; round(npy, 3)

# 기대도수가 작은 범주 (0,1), (7,8,9) 합병
p = c(py[1]+py[2], py[3:7], 1-sum(py[1:7])); np = 80*p; round(np, 3)

(x = c(x[1]+x[2], x[3:7], sum(x[8:10])))

# 기본함수를 이용하여 적합도 검정 ⇒ p-값이 0.05보다 크므로 귀무가설 채택
(ct = chisq.test(x, p=p))

# chitest.plot2() 실행
chitest.plot2(stat=ct$stat, df=ct$para, side="up")




# 관측 데이터의 기댓값 추정
(mv = mean(exa12_3))

# 확률 및 기대도수 계산
py = dpois(y, mv); npy = 80*py; round(npy, 3)

# 범주 합병
p = c(py[1]+py[2], py[3:7], 1-sum(py[1:7])); np = 80*p; round(np, 3)

# 적합도 검정 통계량 ⇒ 자유도 1 공제해야 하므로 p-값이 틀림
(ct = chisq.test(x, p=p))

# p-값 계산 (자유도 1 공제) ⇒ p-값이 0.05보다 작으므로 귀무가설 기각
deg = ct$para[[1]]-1; pv = 1-pchisq(ct$stat[[1]], deg); round(pv, 4)







# 저장된 데이터 셑(exa12_5)을 불러옴
data(exa12_5)
# 기본함수 chisq.test() 사용 ⇒ p-값이 유의수준보다 작으므로 귀무가설 기각
(ct = chisq.test(exa12_5))

# chitest.plot2() 실행
chitest.plot2(stat=ct$stat, df=ct$para, side="up")




# 앞의 예제에 이어서..., 4행 제거
x2 = exa12_5[-4,]
# 카이제곱 검정 ⇒ p-값이 유의수준보다 크므로 귀무가설 채택
ct = chisq.test(x2)
# chitest.plot2() 실행 ⇒ 귀무가설 채택
chitest.plot2(stat=ct$stat, df=ct$para, side="up")









# 저장된 데이터 셋(exa12_7)을 불러옴
data(exa12_7); str(exa12_7)

# 카이제곱 검정 ⇒ p-값이 유의수준보다 작으므로 귀무가설 기각
(ct = chisq.test(exa12_7))

# 카이제곱 검정 그래프 함수 chitest.plot2() 실행
chitest.plot2(stat=ct$stat, df=ct$para, side="up", pup=0.9995)

# 모자이크 플롯 (영어로 입력, 한글 변수는 오류 발생)
Subject = c("Kor", "Eng", "Math", "Etc")
Hope = c("Sam", "Pub", "Exp", "Sal", "Etc")
# 행렬을 테이블로 변환하고 변수 이름과 범주 지정
t1 = as.table(x)
dimnames(t1) = list(Subject=Subject, Hope=Hope)
# ‘vcd' 라이브러리 설치 및 적재, 
install.packages("vcd") # 설치 안 된 경우
library(vcd)
# 모자이크 플롯 함수 mosaic2() 실행
win.graph(7, 6)
mosaic2(tab=t1, mt="학생 선호과목과 장래희망")















# 저장된 데이터(exa13_1)를 불러옴
data(exa13_1)
y = exa13_1[[1]]
f = exa13_1[[2]]
# 일원 분산분석 함수 anova1() 실행 (단계 0~3)
anova1(y, f, xl="온도", yl="수율", step=0:3)

# [참고] R 기본함수 aov()를 사용하여 직접 수행하는 방법 --------------
af = as.factor(f); an1 = aov(y ~ af); summary(an1)

# 진단 그래프 작성
win.graph(7,4); par(mfrow=c(1,2)); plot(an1, which=1:2) 

# 일원 분산분석 (step=4, 5)
anova1(y, f, xl="온도", yl="수율", step=4:5, alp=0.05, dig=4)

# 일원 분산분석 (step=6, 7)
anova1(y, f, xl="온도", yl="수율", step=6:7, alp=0.05, dig=4)







# 저장된 데이터(exa13_4)를 불러옴, attach() 함수 실행 ⇒ 하위 변수 직접 사용
data(exa13_4); str(exa13_4); attach(exa13_4)
# 수준을 요인으로 변환
온도 = as.factor(온도); 압력 = as.factor(paste0(압력, "기압")); levels(압력)


# 이원 분산분석 함수 anova2() 실행 (단계 0~3)
# 교호작용 그림 (step=1)
anova2(수율, 온도, 압력, step=0:3)


# [참고] 교호작용 그림 직접 작성 ⇒ R의 interaction.plot() 함수 사용
interaction.plot(온도, 압력, 수율, col=c(1,2,4), lwd=2, ylab="수율 평균", 
	main="수율에 대한 온도와 압력의 교호작용 그림")


# [참고] 분산분석 (step=2) 직접 수행 ⇒ R의 aov() 함수 사용
an2 = aov(수율 ~ 온도 * 압력)
win.graph(7,3.5); par(mfrow=c(1,2)); plot(an2, which=1:2)







# 수준조합별 평균 구하기 ⇒ tapply( ) 함수 사용
ym <- tapply(수율, list(압력, 온도), mean); ym


# 이원 분산분석 최적 수준에서의 신뢰구간
anova2(수율, 온도, 압력, step=4:5)


# 이원 분산분석 최적 4수준에서의 신뢰구간 비교
anova2(수율, 온도, 압력, step=6:7)








# 이전 데이터를 모두 지우고, 저장된 데이터(exa13_6)를 불러옴
rm(list=ls()); data(exa13_6); attach(exa13_6)
# 수준을 요인으로 변환
온도 = as.factor(온도); 압력 = as.factor(paste0(압력, "기압"))
# 이원 분산분석 (반복하지 않은 경우) 수행 (step 0~3)
anova2(수율, 온도, 압력, step=0:3, inter=FALSE)



# [참고] 교호작용 그래프를 직접 작성하는 방법
interaction.plot(온도, 압력, 수율, col=c(1,2,4), lwd=2, ylab="수율", 
	main="수율에 대한 온도와 압력의 교호작용 그림")



# [참고] 진단 그래프를 직접 작성하는 방법
an2 = aov(수율 ~ 온도 + 압력)
win.graph(7,3.5); par(mfrow=c(1,2))
plot(an2, which=1:2)



# 이원 분산분석 실행 (step 4)
anova2(수율, 온도, 압력, step=4, inter=FALSE)















data(exa14_1)
# 과목별 1, 2학기 점수를 각각 리스트로 할당
과목 = c("국어", "영어", "수학", "사회", "과학", "예체능")
xd = list()
for (k in 1:6) xd[[k]] = cbind(exa14_1[[2*k-1]], exa14_1[[2*k]])
# 함수 corr.mplot() 실행 (단계 1~2)
corr.mplot(X=xd, item=과목, xl="1학기", yl="2학기", step=1:2)


# 위에 이어서 단계 3~5 실행, t-검정 그래프 (단계 5)
corr.mplot(X=xd, item=과목, xl="1학기", yl="2학기", step=3:5)




# 데이터 입력 및 함수 corr.reg1() 실행 (단계 1~4) (산점도 그래프 생략)
x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
corr.reg1(x, y, step=1:4)


# [참고] R 기본함수 cor.test( ) 함수를 사용하여 상관관계 검정
cor.test(x, y)








# 수출액을 10만 달러 단위로 변환 (앞의 예에 이어서)
(y2 = y*x/1000)

# (1) 상관관계 검정 (단계 1~3) (산점도 그래프 생략)
corr.reg1(x, y2, step=1:4)

# (2) 상관계수 검정 (단계 3~4, rho0=0.9)
corr.reg1(x, y2, r0=0.9, step=3:4)



# 앞의 예에 이어서 산점도 작성 (단계 6)
corr.reg1(x, y2, xl="환율", yl="수출액", step=6)




# 앞의 예제에 이어서 단계 7 실행
corr.reg1(x, y2, xl="환율", yl="수출액", step=7)



# 앞의 예제에 이어서 단계 8 실행
corr.reg1(x, y2, xl="환율", yl="수출액", step=8)



# [참고] 기본함수 lm()과 anova()를 사용하여 직접 구하는 방법 ---------------
rg1 = lm(y2 ~ x)
# 분산분석 ⇒ p-값이 매우 작으므로 회귀식은 의미가 있음
anova(rg1)
# 결정계수
summary(rg1)$r.sq



# 앞의 예제에 이어서 단계 9 실행
corr.reg1(x, y2, xl="환율", yl="수출액", step=9)


# [참고] 기본함수 lm(), confint(), summary()를 사용하여 직접 구하는 방법 --
# 회귀모형 적합 ⇒ 신뢰구간 및 가설검정
rg1 = lm(y2 ~ x)

summary(rg1)$coef




# x=1200에서의 반응치의 기댓값에 대한 신뢰구간
# 앞의 예제에 이어서 단계 10 실행
corr.reg1(x, y2, xl="환율", yl="수출액", step=10, x0=1200)

# 반응치의 기댓값에 대한 신뢰밴드와 예측밴드 (단계 11) ⇒ [그림 14-7]
corr.reg1(x, y2, xl="환율", yl="수출액", step=11, x0=1200, 
	xrng=c(1050, 1200), by=5)

# [참고] 기본함수 lm()과 predict()를 사용하여 직접 구하는 방법 -------------
rg1 = lm(y2 ~ x)
# predict( ) 함수를 사용하려면 데이터를 데이터프레임 형태로 저장해야 함
new = data.frame(x=1200)
predict(rg1, new, interval="confidence")

# x=1200에서의 예측치에 대한 예측구간
predict(rg1, new, interval="prediction")














# 저장된 데이터(exa14_10)를 불러옴
data(exa14_10); attach(exa14_10)
xd = data.frame(국어, 독서); form = 성적 ~ 국어 + 독서
# 함수 corr.mreg() 실행 (단계 0~1) ⇒ 데이터 사전조사 및 회귀계수 추정
corr.mreg(xd, 성적, form, step=0:1)

# [참고] 기본함수 lm()을 사용하여 직접 회귀분석 수행 ----------------------
(rg2 = lm(성적 ~ 국어 + 독서))

# 앞의 예에 이어서 단계 2 실행
corr.mreg(xd, 성적, form, step=2)


# [참고] 기본함수 lm()과 anova()를 사용하여 직접 분산분석 ------------------
rg2 = lm(성적 ~ 국어 + 독서); anova(rg2)
# F-통계량, 결정계수, 수정결정계수
summary(rg2)$fstat; summary(rg2)$r.sq; summary(rg2)$adj.r



# 앞의 예에 이어서 단계 3 실행
corr.mreg(xd, 성적, form, step=3)

# [참고] 기본함수 summary()를 이용하여 직접 검정 --------------------------
summary(rg2)


# (1) 앞의 예에 이어서 단계 6 실행
nd= data.frame(국어=5, 독서=5)
corr.mreg(xd, 성적, form, step=6, newd=nd)

# (2) 국어: 95% 신뢰밴드와 95% 예측밴드 (단계 7)
corr.mreg(xd, 성적, form, step=7, xrng=c(0, 15), pvx=1, nx=30)
# (2) 독서: 95% 신뢰밴드와 95% 예측밴드 (단계 7)
corr.mreg(xd, 성적, form, step=7, xrng=c(0, 15), pvx=2, nx=30)


# 앞의 예에 이어서 수행 (“+”=선형모형, “*”=상호작용 포함 모형)
xd2 = data.frame(국어, 독서, 국어.독서=국어*독서)
form2 = 성적 ~ 국어 * 독서
# 모형 2 회귀분석 단계 4 실행
corr.mreg(xd, 성적, form, xd2, form2, step=4)



# 회귀모형 진단 (단계 5)
corr.mreg(xd, 성적, form, xd2, form2, step=5)


# [참고] R 기본함수를 이용하여 직접 수행하는 방법 -----------------------
# 회귀분석 수행 (“+”=선형모형, “*”=상호작용 포함 모형)
rg2 = lm(성적 ~ 국어+독서)
rg3 = lm(성적 ~ 국어*독서)

# 회귀계수 추정치만 출력
coefficients(rg3)

# 분산분석표 출력 (이하 출력은 생략함)
anova(rg3)
# 회귀계수의 신뢰구간 출력
confint(rg3, level=0.95)
# 회귀분석 결과 요약 출력 (회귀계수 유의성 검정)
summary(rg3)
# 회귀모형 비교 (분산분석)
anova(rg2, rg3) 
# 회귀모형 진단 그림
par(mfrow=c(2,2)); plot(rg2); plot(rg3)
