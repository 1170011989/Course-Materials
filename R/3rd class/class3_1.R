# 확률분포 입력 & 기댓값 계산
x = 1:20; p = rep(1, 20); disc.exp(x, p)

# 15 이상의 번호가 나올 확률
sum(x>=15)/length(x)


# 이항분포 R 함수
# 확률분포함수 (size=n=표본크기, prob=p=성공확률)
dbinom(x, size, prob)
# Excel 함수 = BINOM.DIST(x, size, prob, FALSE)

# 누적분포함수 (q=분위수, lower.tail=TRUE=아래로부터 누적)
pbinom(q, size, prob, lower.tail = TRUE)
# Excel 함수 = BINOM.DIST(x, size, prob, TRUE)

# 분위수 (p=누적확률)
qbinom(p, size, prob, lower.tail = TRUE)
# Excel 함수 = BINOM.INV(size, prob, p)

# 이항 확률변수(n=난수의 개수)
rbinom(n, size, prob)
# Excel 함수는 없으나, 아래와 같이 한 개의 난수 생성
= BINOM.INV(size, prob, RAND( ))


# 시행횟수, 성공확률, 확률변수 x의 범위 정의
n = 10
p=c(0.2, 0.5, 0.8)
x = 0:n
# 빈 리스트 생성
fx1 = list()
# 확률분포함수 계산 ⇒ dbinom( ) 함수 사용
for (i in 1:3) fx1[[i]] = dbinom(x, n, p[i])
# 분포별 합계 확인 ⇒ sapply(리스트, sum) 함수
sapply(fx1, sum)

# 제목 설정 및 함수 disc.mexp() 실행, 그래프 작성
mt1 = paste0("B(10,", p, ")")
disc.mexp(x, fx1, mt=mt1)





# 확률분포
x = 0:20
fx = dbinom(x, 20, 0.03)
# 이산형 확률변수의 기댓값 계산
disc.exp(x, fx, prt=TRUE)

# 각각 0, 1, 2개의 불량품이 발견될 확률
dbinom(0:2, 20, 0.03)

# 3개 이상의 불량품이 발견될 확률 (두 가지 방법)
1-sum(dbinom(0:2, 20, 0.03))

pbinom(2, 20, 0.03, lower=F)





# 초기하분포  R 함수

# 확률분포함수 (x=표본 성공개수, m=r=모집단 성공 개체 수,
# 		n=N-r=모집단 실패 개체 수, k=표본개수)
dhyper(x, m, n, k)
# Excel 함수 = HYPGEOM.DIST(x, k, m, N, FALSE)

# 누적분포함수 (q=분위수, lower.tail=TRUE=아래로부터 누적)
phyper(q, m, n, k, lower.tail = TRUE)
# Excel 함수 = HYPGEOM.DIST(x, k, m, N, TRUE)

# 분위수 (p=누적확률)
qhyper(p, m, n, k, lower.tail = TRUE)

# 초기하 확률변수(nn=난수의 개수)
rhyper(nn, m, n, k)




# 모집단 구성, 표본개수, 확률변수 범위(치역) 정의
N = 50; S =c(10, 25, 40); n = 10; x = 0:n
# 빈 리스트 생성
fx2 = list()
for (i in 1:3) fx2[[i]] = dhyper(x, S[i], N-S[i], n)
# 분포별 합계 확인 ⇒ sapply(리스트, sum) 함수
sapply(fx2, sum)

# 제목 설정 및 함수 disc.mexp() 실행
mt2 = paste0("HG(10, 50,", S,")")
disc.mexp(x, fx2, mt=mt2)

# ① 기댓값은 성공확률에 비례하여 증가함
# ② 분산은 p=0.5일 때 가장 크고, p=0.2와 0.8일 때 같음
# ③ 이항분포에 비해 분산이 다소 감소함
# 이항분포와 초기하분포의 확률분포함수 그래프 비교
mt12 = paste0("HG(10,50,", S,"):B(10,", p,")")
disc.mexp(x, fx2, fx1, mt=mt12)
# 이항분포에 비해 분산이 다소 감소함


# 확률분포
x = 0:20
fx = dhyper(x, 50, 950, 30)
# 이산형 확률변수의 기댓값 계산
disc.exp(x, fx, prt=TRUE)

# 각각 0, 1, 2, 3개의 불량품이 발견될 확률
dhyper(0:3, 50, 950, 30)


# 3개 이하의 불량품이 발견될 확률 (두 가지 방법)
sum(dhyper(0:3, 50, 950, 30))

phyper(3, 50, 950, 30)







# 포아송분포  R 함수

# 확률분포함수 (lambda=기댓값)
dpois(x, lambda)
# Excel 함수 = POISSON.DIST(x, lambda, FALSE)

# 누적분포함수 (q=분위수, lower.tail=TRUE=아래로부터 누적)
ppois(q, lambda, lower.tail = TRUE)
# Excel 함수 = POISSON.DIST(x, lambda, TRUE)

# 분위수 (p=누적확률)
qpois(p, lambda, lower.tail = TRUE)

#포아송 확률변수(n=난수의 개수)
rpois(n, lambda)




# 평균값, X의 범위 정의
L =c(2, 5, 8); x = 0:30
# 확률분포함수 계산, dpois() 함수 사용
fx3 = list()
for (i in 1:3) fx3[[i]] = dpois(x, L[i])
# 합계 확인 (30을 넘을 확률은 거의 무시됨)
sapply(fx3, sum)


# 제목 설정 및 함수 disc.mexp() 실행
mt3 = paste0("Poisson(", L,")")
disc.mexp(x, fx3, mt=mt3)


# 이항, 초기하, 포아송분포 확률분포함수 그래프 비교
mt123 = paste0("HG(50) : B(10) : Pois (", L, ")")
x=0:10
for (i in 1:3) fx3[[i]] = dpois(x, L[i])
disc.mexp(x, fx2, fx1, fx3, mt=mt123)



# 확률분포 정의, 기댓값 계산
x = 0:20; fx = dpois(x, 1.5); disc.exp(x, fx, prt=TRUE)

# 3개 이상의 결점이 발견될 확률 (두 가지 방법)
1-sum(dpois(0:2, 1.5)); ppois(2, 1.5, lower=F)

# 10개의 단위 제품에서 20개의 결점이 발견될 확률
dpois(20, 15)





# 기하분포 R 함수
# Excel 함수 없음

# 확률분포함수 (x=실패횟수, prob=p=성공확률)
dgeom(x, prob)

# 누적분포함수 (q=분위수, lower.tail=TRUE=아래로부터 누적)
pgeom(q, prob, lower.tail = TRUE)

# 분위수 (p=누적확률)
qgeom(p, prob, lower.tail = TRUE)

# 기하 확률변수(n=난수의 개수)
rgeom(n, prob)


# 성공확률, X의 범위 정의 (범위가 무한대까지기 때문에 넓게 설정)
p = c(0.1, 0.3, 0.5)
x = 1:200
# 확률분포함수 계산, ※ dgeom( ) 함수는 x 대신 x-1 입력!
fx4 = list()
for (i in 1:3) fx4[[i]] = dgeom(x-1, p[i])

# 기댓값과 분산 계산
disc.mexp(x, fx4, plot=F)


# 확률분포함수 그래프 직접 작성 (범위 상한을 50으로 함)
mt4 = paste0("Geometric(", p,")")
win.graph(9, 3)
par(mfrow=c(1,3))
par(mar=c(3,4,4,2))
for (k in 1:3) plot(1:50, fx4[[k]][1:50], type="h", main=mt4[k], 
				ylab="f(x)", xlab="", lwd=3, col=2)



# 확률분포 (x의 범위를 충분히 넓게 잡아야 함)
x = 1:100
fx = dgeom(x-1, 1/6)

# 이산형 확률변수의 기댓값 계산
disc.exp(x, fx, prt=TRUE)


# 각각 0, 1, 2회의 실패 이내에 ‘6’이 나올 확률
dgeom(0:2, 1/6)


# 3회의 시행 (2회의 실패) 이내에 ‘6’이 나올 확률 (두 가지 방법)
sum(dgeom(0:2, 1/6))

pgeom(2, 1/6)




# 확률분포함수 (x=실패횟수, size=목표 성공 횟수, prob=성공확률)
dnbinom(x, size, prob)
# Excel 함수 = NEGBINOM.DIST(x, size, prob, FALSE)
# 누적분포함수 (q=분위수, lower.tail=TRUE=아래로부터 누적)
pnbinom(q, size, prob, lower.tail = TRUE)
# Excel 함수 = NEGBINOM.DIST(x, size, prob, TRUE)
# 분위수 (p=누적확률)
qnbinom(p, size, prob, lower.tail = TRUE)
# 음이항 확률변수(n=난수의 개수)
rnbinom(n, size, prob)


# 성공확률, 목표 성공 횟수, X의 범위 정의 (넓게 설정)
ps =0.4; r = c(1, 2, 4); xr = 1:100
# dnbinom() 함수는 r번 성공까지의 실패횟수의 확률분포임에 유의!
fx5 = list()
for (i in 1:3) fx5[[i]] = dnbinom(xr-r[i], r[i], ps)
# 합계 확인 
sapply(fx5, sum)

# 확률분포함수 그래프 직접 작성 (범위 상한 30으로 설정)
mt5 = paste0("Neg-Binom(0.4,", r,")")
win.graph(9,3); par(mfrow=c(1,3)); par(mar=c(3,4,4,2))
for (k in 1:3) plot(xr[1:30], fx5[[k]][1:30], type="h", main=mt5[k], 
				ylab="f(x)", xlab="", lwd=3, col=2)

# 기댓값과 분산 계산
disc.mexp(xr, fx5, plot=F)

# ① 기댓값은 목표 성공회수에 비례하여 증가함
# ② 분산 또한 목표 성공회수에 비례하여 증가함


# 확률분포 (x의 범위를 충분히 넓게 잡아야 함) ⇒ 기댓값 계산
x = 3:250; ps=0.1; r=3
fx = dnbinom(x-r, r, ps); disc.exp(x, fx)

# 10번째 시도 만에 세 개의 프로젝트를 성공시킬 확률
dnbinom(10-r, r, ps)






# 확률분포함수 (x=(x1,x2,...,xk), size=n, prob=(p1,p2,...,pk))
dmultinom(x, size = NULL)
# 다항 확률변수(n=난수 벡터의 개수)
rmultinom(n, size, prob)



# 성공확률, 표본크기, X의 범위 정의, ‘scatterplot3d’ 패키지 적재
ps = matrix(c(1,1,8, 1,5,4, 1,1,1), nrow=3, ncol=3, byrow=T)
library(scatterplot3d)
# 다항확률분포 그래프 작성 함수 multinorm.plot() 실행
multinorm.plot(ps, 5)



# 성공확률, 표본크기, X의 범위 정의
ps = c(2,4,3,1)
n = 20

# (3) (x1=3, x2=6, x3=8)이 나올 확률
dmultinom(c(3,6,8,3), size=20, prob=ps)


# (4) (x1=3, x2=6)이 나올 확률
dmultinom(c(3,6,11), size=20, prob=c(2,4,4))


# (5) (x1=3)이 나올 확률 (두 가지 방법)
dmultinom(c(3, 17), size=20, prob=c(2,8))

dbinom(x=3, size=20, prob=0.2)

