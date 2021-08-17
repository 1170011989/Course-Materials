# 함수 pmean.ci() 실행
pmean.ci(xb=199.5, sig=5, n=50)


pmean.ci(xb=199.5, sig=5, n=50, dig=3)



# 함수 정의 및 실행
spn = function(del, sig, alp) ceiling((qnorm(1-alp/2)*sig/del)^2)
spn(1:3, 5, 0.05)



# 함수 meantest1.plot() 실행
meantest1.plot(xb=12.64, mu0=12.5, sig=0.5, n=40, side="up")



# 모분산을 아는 경우 모평균에 대한 검정(양측) 함수 실행
meantest1.plot(xb=199.5, mu0=200, sig=1.8, n=50, side="two")


# 함수 pmean.ci2() 실행
pmean.ci2(xb=199.5, sig=5, n=16, alp=0.05, dig=3)


# 데이터 셋(exa10_9)을 불러옴
data(exa10_9); x = exa10_9
# 표본평균, 표준편차 계산
mean(x); sd(x)

# 기본 함수 t.test( ) 사용 (데이터가 있는 경우 사용 가능)
t.test(x)$conf[1:2]

# 신뢰구간 함수 pmean.ci2() 실행 (데이터 직접 사용)
pmean.ci2(x, dig=3)




# 함수 meantest2.plot() 실행
meantest2.plot(xb=12.65, mu0=12.5, sig=0.57, n=40, side="up")



# 모분산을 모르는 경우 모평균에 대한 검정
meantest2.plot(xb=199.5, mu0=200, sig=1.8, n=50, side="two")

pmean.ci2(xb=199.5, sig=1.8, n=50)


# 데이터 입력
data(exa10_9); x = exa10_9
# 모평균에 대한 검정 함수 meantest2.plot() 실행
meantest2.plot(x, mu0=200, side="two")



# 함수 prob.ci() 실행 
prob.ci(n=200, x=15)


# 기본 함수 binom.test()를 이용한 정확한 신뢰구간 추정 (다소 차이)
binom.test(15, 200)$conf[1:2]




# 표본 개수를 구하는 함수 정의 (오차한계, 유의수준, 비율추정치)
nsample <- function(err, alp=0.05, ph=0.5) {
	n <- qnorm(1-alp/2)^2 * ph*(1-ph) /err^2
	cat("n ≥", n, "⇒ n ≥", ceiling(n), "\n")}
# 함수 실행
nsample(0.02, ph=15/200)

nsample(0.02)


# 표본 개수를 구하는 함수 nsamp() 정의 (오차한계, 유의수준, 비율추정치)
nsamp = function(err, alp=0.05, ph=0.5) {
	n = ceiling(qnorm(1-alp/2)^2 * ph*(1-ph) /err^2)}
# 그래프 작성
win.graph(9,4); par(mfrow=c(1,3))
# (1) 오차한계를 줄일수록 표본크기는 급격히 증가함
erv = (10:100)/500
plot(erv, nsamp(erv, 0.05, 0.1), type="l", lwd=3, col=2, 
    main="오차한계에 따른 표본크기", xlab="오차한계", ylab="표본크기")

# (2) 유의수준을 작게 할수록 표본크기는 급격히 증가함
alpv = (10:100)/1000
plot(alpv, nsamp(0.02, alpv, 0.1), type="l", lwd=3, col=2, 
    main="유의수준에 따른 표본크기", xlab="유의수준", ylab="표본크기")

# (3) 모비율이 0.5일 때, 표본크기는 최대가 됨
pv = (1:99)/100
plot(pv, nsamp(0.02, 0.05, pv), type="l", lwd=3, col=2, 
    main="모비율에 따른 표본크기", xlab="모비율", ylab="표본크기")
abline(v=0.5, lty=2, col=4)


# 함수 bntest.plot() 실행
bntest.plot(x=2:4, n=10, p0=0.1, side="up")


# 함수 bntest2.plot() 실행
bntest2.plot(x=15, n=200, p0=0.1, alp=0.05, side="low")


# 함수 bntest2.plot() 실행
bntest2.plot(x=15, n=200, p0=0.1, alp=0.05, side="low")


# [응용] 양측검정의 예
bntest2.plot(x=15, n=200, p0=0.1, alp=0.05, side="two")






# 데이터 입력
x = c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
# 함수 var1.ci() 실행
var1.ci(x, dig=3)



# 함수 var1.test() 실행
chi0 = var1.test(x=1.24, n=25, var0=0.8, side="up")

# 함수 chitest.plot() 실행
chitest.plot(stat=chi0$stat, df=chi0$df, side="up")




# 데이터 입력 & 모분산 검정, var1.test() 함수
x = c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
chi0 = var1.test(x=x, var0=2, side="two")

# 그래프 검토
chitest.plot(stat=chi0$stat, df=chi0$df, side="two")




# 모의실험 함수 cimean.sim() 실행
cimean.sim(n=16, mu=10, sig=2)

cimean.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)



# 모의실험 함수 civar.sim() 실행
civar.sim(n=16, mu=10, sig=2)

civar.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)



# # 모의실험 함수 ciprob.sim() 실행
ciprob.sim(n=16, p=0.6, alp=0.05, N=100)

ciprob.sim(n=16, p=0.6, alp=0.05, N=10000, plot=FALSE)



# (2) 검정력 함수 ⇒ meanpower.plot() 실행
n = c(10, 30, 50, 100)
meanpower.plot(mu0=100, mu1=102, sig=5, nv=n, side="up")


# (2) 모평균에 대한 검정(양측)의 검정력 함수
n = c(10, 30, 50, 100)
meanpower.plot(mu0=100, mu1=102, sig=5, nv=n, side="two")


# (2) 검사특성곡선 함수 meanchar.plot() 실행
n = c(10, 30, 50, 100)
meanchar.plot(mu0=100, mu1=98, sig=5, nv=n, side="low")


# (2) 모평균에 대한 검정(양측)의 검사특성곡선
n = c(10, 30, 50, 100)
meanchar.plot(mu0=100, mu1=102, sig=5, nv=n, side="two")


