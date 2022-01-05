# 함수 norm.sim() 실행
norm.sim(ns=10, mu=100, sig=10, N=10000)


n <- 16; se=5/sqrt(n)
# 방법 1 (직접계산), mu=0 (임의의 수 입력)
pnorm(2, 0, se) - pnorm(-2, 0, se)

# 방법 2 (표준화)
pnorm(2/se) - pnorm(-2/se)



# 직접 계산
ceiling((qnorm(0.975)/0.4)^2)

# 간단한 함수 정의 (p=신뢰수준, sigma=오차한계(표준편차의 배수))
findsn <- function(p, sigma) ceiling((qnorm(1-(1-p)/2)/sigma)^2)
findsn(0.95, 4:1/10)



# 세 가지 유의수준(alpha) 및 색상 정의
alp = c(0.01, 0.05, 0.1); dcol = c(2, 4, "green4")
# 함수 norm.spn() 실행
norm.spn(kp=0.4, alp, dcol=dcol)



# 모의실험 함수 tdist.sim() 실행
tdist.sim(ns=10, mu=100, sig=10)



# 모의실험 함수 chi.sim() 실행
chi.sim(ns=10, mu=100, sig=10)


# 카이제곱분포 모의실험 함수 chi.sim() 실행
chi.sim(ns=10, mu=100, sig=10, muknow=FALSE)



# 자유도, 누적확률 입력
nu <- c(5, 10, 30, 100)
p <- 0.95

# 분위수/자유도 (상수 c)
qchisq(p, nu-1) / (nu-1)


# 함수 fdist.sim2() 실행
fdist.sim2(sig1=2, sig2=7, n1=8, n2=6)



qf(0.05, 11, 15, lower.tail=FALSE); qf(0.95, 11, 15)

pf(0.5, 11, 15) + 1-pf(2, 11, 15)





# 표본크기 지정
nv = c(10, 30, 50)
# (1) t(3) 분포에 대해 함수 clt.plot() 실행
clt.plot("t", para=3, ns=nv, d=rep(0.4, 3))



pnorm(69, 70, sqrt(21/40))

pbinom(2760, 4000, 0.7)


pnorm(9, 10, sqrt(100/40))

# 야심찬 시도 a 실패 원인은?
 pnorm(9.5, 10, sqrt(100/40))

pgamma(360, 40, scale=10)

pnorm(4, 3.5, sqrt(35/480)) - pnorm(3, 3.5, sqrt(35/480))



# 각각 10, 30, 50개씩 샘플링
nv = c(10, 30, 50)
# (1) B(n, 0.5) 기댓값=0.5n, 분산=0.25n
clt.plot("bin", para=0.5, ns=nv, d=c(0.6, 0.4, 0.3))


pbinom(15, 40, 1/3) - pbinom(9, 40, 1/3)

pnorm(15.5, 40/3, sqrt(80/9)) - pnorm(9.5, 40/3, sqrt(80/9))

pnorm(15, 40/3, sqrt(80/9)) - pnorm(10, 40/3, sqrt(80/9))



