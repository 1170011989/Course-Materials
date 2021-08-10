# 표본공간 생성
S = tosscoin2(4)

# 뒷면의 개수를 세는 함수 정의
countT=function(x) sum(x=="T")

# 확률변수 정의 ⇒ apply() 함수를 행별로 적용
X=apply(S, 1, countT)

# 원소의 개수 집계 및 확률분포 생성
table(X)/nrow(S)


# 주사위 4개 눈의 합 확률분포
rolldie.sum(4)

# 초기하 분포
hyp.sample(50, 8, 10)

# 확률밀도함수 f(x) 정의
pdf = function(x) 2*exp(-2*x)*(x>0)
# 적분함수 integrate() 사용하여 확률 계산
integrate(pdf, 0, 1)[[1]]

# 동전 3개 중 뒷면의 개수
(freq = choose(3, 0:3))

disc.cdf(0:3, freq, mt="동전 3개 중 뒷면의 개수 CDF")


# 확률밀도함수 f(x) 정의 및 누적분포함수 그래프 작성
pdf = function(x) 2*exp(-2*x)*(x>0)
# 누적확률 표시 (F(0.2), F(0.4), F(0.6), F(0.8), F(1), F(2))
cont.cdf(pdf, low=-1, up=3, xs=c((1:5)*0.2, 2))




# 표본공간 생성
S = rolldie2(2); N = nrow(S)
# 최대치 X, 최소치 Y 주변 도수분포
X = apply(S, 1, max); Y = apply(S, 1, min)
# 결합 도수분포와 결합 확률분포 ⇒ 함수 disc.joint2() 실행
disc.joint2(X, Y)


# 두 이산형 확률변수의 결합확률분포 그래프
install.packages("scatterplot3d") # 설치하지 않은 경우
library(scatterplot3d)
disc.joint2(X, Y, prt=FALSE, plot=TRUE)




# 확률분포함수 정의
pdf = function(a, b, x, y) 2/(a+b)*(a*x+b*y)*(x>0 & x<1)*(y>0 & y<1)
# 세 가지 예 (a, b) = (1, 1), (2, 5), (5, 15)
pdf1 = function(x, y) pdf(1, 1, x, y)
pdf2 = function(x, y) pdf(2, 5, x, y)
pdf3 = function(x, y) pdf(5, 15, x, y)
# (1) 확률분포함수의 조건 충족 검토
cont.jcdf(pdf1, Inf, Inf); cont.jcdf(pdf2, Inf, Inf); cont.jcdf(pdf3, Inf, Inf)


# (2) 함수 cont.jcdf()를 사용하여 결합 확률 계산 (이중 적분)
cont.jcdf(pdf1, 0.5, 0.5)


# 3차원 그래픽 패키지
library(scatterplot3d)
# [응용] 누적분포함수 플롯
cont.jcdfp(pdf1, 0, 1, 0, 1)



# X, Y의 결합확률분포(도수) 정의
jfreq = function(x, y) { if (y %in% 1:6 & x %in% 1:6) { 
		if (x==y) {pdf = 1
		} else if (x>y) {pdf = 2
		} else pdf = 0  } }
# X, Y의 결합확률분포를 행렬로 저장
fxy = matrix(NA, 6, 6)
for (x in 1:6) for (y in 1:6) fxy[x, y] = jfreq(x, y)
rownames(fxy) = colnames(fxy) = 1:6
# 주변확률분포 계산 함수 disc.marg2() 실행
disc.marg2(fxy)


# [응용] 두 이산형 확률변수의 주변확률분포 그래프
disc.marg2(fxy, prt=FALSE, plot=TRUE)


# 두 연속형 확률변수의 결합확률밀도함수
pdf = function(a, b, x, y) 2/(a+b)*(a*x+b*y)*(x>=0 & x<=1)*(y>=0 & y<=1)
# 두 가지 예 (a, b) = (2, 5), (5, 15)
pdf1 = function(x, y) pdf(2, 5, x, y)
pdf2 = function(x, y) pdf(5, 15, x, y)
# 주변확률밀도함수 그래프 함수 cont.marg2() 실행
cont.marg2(pdf2, -0.2, 1.2, -0.2, 1.2, 0:2/2, 0:2/2)



# X, Y의 결합확률분포(도수) 정의
jfreq = function(x, y) (x==y)+2*(x>y)
fxy = matrix(NA, 6, 6)
for (x in 1:6) for (y in 1:6) fxy[x, y] = jfreq(x, y)
rownames(fxy) = colnames(fxy) = 1:6
# 함수 disc.cond2() 실행 ⇒ 조건부 확률분포
disc.cond2(fxy, Ys=1:6)
# [응용] 조건부 확률분포 f(x|y=1) 그래프
disc.cond2(fxy, Ys=1:6, prt=FALSE, plot=TRUE)


# 두 연속형 확률변수의 결합확률밀도함수
pdf = function(a, b, x, y) 2/(a+b)*(a*x+b*y)*(x>=0 & x<=1)*(y>=0 & y<=1)
# 예 (a, b) = (1, 1)
pdf1 = function(x, y) pdf(1, 1, x, y)
# [응용] 두 이산형 확률변수의 조건부 확률밀도함수 그래프
cont.cond2(pdf1, yc=0.1, xs=0:2/2, lo=-0.2, up=1.2)


# 최대 최소 독립성 예제
S = rolldie2(2)
X = apply(S, 1, max); Y = apply(S, 1, min)
disc.ind2(X, Y, prt=FALSE)


S = rolldie2(2)
# 3이상 눈의 개수 X
X = apply(S, 1, function(x) sum(x>=3))
# 짝수의 개수 Y
Y = apply(S,1, function(x) sum(x %% 2 ==0))
# 함수 disc.ind2()를 실행하여 독립성 판정 ⇒ X와 Y는 독립
disc.ind2(X, Y)



# [응용] X, Y 독립성 검토 (X=중앙값, Y=범위)
X = apply(S, 1, median)
Y = apply(S, 1, function(x) max(x)-min(x))
disc.ind2(X, Y, prt=FALSE)



# 두 연속형 확률변수의 결합확률밀도함수
pdf = function(a, b, x, y) 2/(a+b)*(a*x+b*y)
	*(x>=0 & x<=1)*(y>=0 & y<=1)

# 예 (a, b) = (1, 1) ⇒ 독립성 검토 함수 cont.ind2() 실행
pdf1 = function(x, y) pdf(1, 1, x, y)
cont.ind2(pdf1, lo1=0, up1=1, lo2=0, up2=1)


# 두 연속형 확률변수의 결합확률밀도함수
pdf = function(x, y) ifelse(x<0 | y<0, 0, 8*exp(-2*x - 4*y))

# 두 연속형 확률변수의 독립성 검토 함수 실행
cont.ind2(pdf, lo1=0, up1=100, lo2=0, up2=100)




# X의 pdf 정의
fx = function(x) 2*x*(x>=0 & x<=1)
# 변수변환법
ty = function(x) 10*x - 4
tw = function(x) -10*x + 4
fy = function(y) fx((y+4)/10)/10
fw = function(w) fx((-w+4)/10)/10
# 함수 cont.trans()를 사용하여 확률 계산
cont.trans(fx, list(y=ty, w=tw), list(fy, fw), 0.3, 0.7, lo=0, up=1)



# [응용] 결과 출력
cont.trans(fx, list(y=ty, w=tw), list(fy, fw), 0.3, 0.7, plot=TRUE, cex=1.3)
# Y의 확률밀도함수는 의 확률밀도함수의 하한 0을 -4로 이동시키고 폭을 10배로 넓힌 형태임 (높이는 10분의 1일이 됨)
# W의 확률밀도함수는 의 확률밀도함수를 0을 중심으로 대칭 이동시킨 형태임









