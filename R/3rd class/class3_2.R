# 균일분포의 R 함수

# 확률밀도함수 (min=a=하한, max=b=상한)
# min, max를 생략하면 [0,1]로 계산함
dunif(x, min = 0, max = 1)

# 누적분포함수 (q=분위수)
punif(q, min = 0, max = 1, lower.tail = TRUE)

# 분위수 (p=누적확률)
qunif(p, min = 0, max = 1, lower.tail = TRUE)

# 균일 확률변수 (n=난수의 개수)
runif(n, min = 0, max = 1)
# Excel 함수 = RANDBETWEEN(min, max) ⇒ 난수 1개만 생성



# 균일분포 함수 정의
fx = function(x) dunif(x, 0, 1); fy = function(y) dunif(y, 2, 6)
# 그래프 창 크기 설정, 화면 분할, 마진 설정
win.graph(7, 6); par(mfrow=c(2, 1)); par(mar=c(3,4,4,2))
# 함수 cont.exp() 실행
cont.exp(fx, -0.2, 1.2, prt=T, plot=T)

cont.exp(fy, 1.2, 6.8, xn="y", prt=T, plot=T)







# 지수분포의 R 함수

# 확률밀도함수 f(x) (rate=λ), 초기치 (rate = 1)
dexp(x, rate = 1)
# Excel 함수 = EXPON.DIST(x, rate, FALSE)

# 누적분포함수 (q=분위수)
pexp(q, rate = 1, lower.tail = TRUE)
# Excel 함수 = EXPON.DIST(x, rate, TRUE)

# 분위수 (p=누적확률)
qexp(p, rate = 1, lower.tail = TRUE)

# 지수 확률변수 (n=난수의 개수)
rexp(n, rate = 1)


# 모수 설정 및 함수 cont.mpdf() 실행
lamb = 1:5
cont.mpdf("exp", 0, 3, para=lamb, ymax=5)




theta=10; lamb = 1/theta
# 조건부 확률 (위쪽 확률이므로 lower=FALSE로 설정)
pexp(5+3, lamb, lower=F)/pexp(5, lamb, lower=F)

# 비기억 특성 확인
pexp(3, lamb, lower=F)





theta=10000; lamb = 1/theta
# 누적확률이 1-0.9인 분위수 계산
qexp(1-0.9, lamb)



# 해를 구할 함수 정의
lamb = function(r) pexp(10000, r, lower=F) - 0.9
# 해 찾기 ⇒ uniroot() 함수 (범위 0~1, 오차한계 1E-10)
uniroot(lamb, c(0, 1), tol=1E-10)[[1]]










# 확률밀도함수 f(x) (shape=, rate=와 scale= 중 하나만 입력)
# 초기치 (rate = 1, scale = 1/rate)
dgamma(x, shape, (rate), scale)
# Excel 함수 = GAMMA.DIST(x, shape, scale, FALSE)

# 누적분포함수 F(x)
pgamma(x, shape, (rate), scale, lower.tail = TRUE)
# Excel 함수 = GAMMA.DIST(x, shape, scale, TRUE)

# 분위수 (p=누적확률)
qgamma(p, shape, (rate), scale, lower.tail = TRUE)
# Excel 함수 = GAMMA.INV(p, shape, scale)

# 감마 확률변수 (n=난수의 개수)
rgamma(n, shape, (rate), scale)
# Excel 함수 = GAMMA.INV(RAND( ), shape, scale) ⇒ 난수 1개 생성


# 모수 설정 및 함수 실행
alp = c(0.5, 1, 2, 3); rate = 1
cont.mpdf("gamma", 0, 8, para=alp, para2=rate, ymax=1.2)



# 모수 계산
EX = 10; VX = 50
alp = EX^2 / VX; th = EX / alp

# (1) 이 전동안마기가 3년간 고장 없이 작동할 확률
pgamma(3, alp, 1/th, lower=F)


# (2) 이 전동안마기가 8년간 고장 없이 작동할 확률
pgamma(8, alp, 1/th, lower=F)


# (3) 5년간 고장이 없었을 때, 앞으로 3년 더 고장 없이 작동할 확률
pgamma(8, alp, 1/th, lower=F) / pgamma(5, alp, 1/th, lower=F)











# 와이블분포 R 함수
 
# 확률밀도함수 f(x) (shape=α, scale=θ), 초기치 (scale = 1)
dweibull(x, shape, scale)
# Excel 함수 = WEIBULL.DIST(x, shape, scale, FALSE)

# 누적분포함수 F(x)
pweibull(x, shape, scale, lower.tail = TRUE)
# Excel 함수 = WEIBULL.DIST(x, shape, scale, TRUE)

# 분위수 (p=누적확률)
qweibull(p, shape, scale, lower.tail = TRUE)

# 와이블 확률변수 (n=난수의 개수)
rweibull(n, shape, scale)


# 모수 설정 및 함수 실행
th = 1; alp = c(0.5, 1, 2, 3)
cont.mpdf("weibull", 0, 5, para=alp, para2=th, ymax=1.2)



# 모수 입력
alp = 2; th = 5

# (1) 이 전동안마기가 3년간 고장 없이 작동할 확률
pweibull(3, alp, th, lower=F)


# (2) 5년간 고장이 없었을 때, 앞으로 3년 더 고장 없이 작동할 확률
pweibull(8, alp, th, lower=F)

pweibull(5, alp, th, lower=F)

pweibull(8, alp, th, lower=F) / pweibull(5, alp, th, lower=F)










# 베타분포 R 함수

# 확률밀도함수 f(x) (shape1=, shape2=, ncp=비중심모수(사용 안함))
dbeta(x, shape1, shape2, ncp = 0)
# Excel 함수 없음

# 누적분포함수 F(x)
pbeta(x, shape1, shape2, ncp = 0, lower.tail = TRUE)
# Excel 함수 = BETA.DIST(x, shape1, shape2)

# 분위수 (p=누적확률)
qbeta(p, shape1, shape2, ncp = 0, lower.tail = TRUE)
# Excel 함수 = BETA.INV(p, shape1, shape2)

# 베타 확률변수 (n=난수의 개수)
rbeta(n, shape1, shape2, ncp = 0)
# Excel 함수 = BETA.INV(RAND( ), shape1, shape2) ⇒ 난수 1개 생성




alp = c(0.5, 5, 2, 5); bet = c(0.5, 5, 5, 2)
# 모수 값을 표기할 위치 지정
x1 = (alp-1)/(alp+bet-2); x2 = c(0.3, 0.7, 0.5, 0.5)
# 함수 cont.mpdf() 실행
cont.mpdf("beta", 0, 1, para=alp, para2=bet, ymax=3, xp1=x1, xp2=x2)





