# 확률분포
x = c(-3, -1, 1, 3)*100
p = c(1, 3, 3, 1)/8
# 함수 disc.exp() 실행 후 기댓값 출력
disc.exp(x, p)



# [응용] 이산형 확률분포와 기댓값 그래프 작성
disc.exp(x, p, prt=FALSE, plot=TRUE)



# 확률분포 정의 및 확률변수의 기댓값 계산
pdf = function(x) 2*exp(-2*x)*(x>0)
xp = function(x) x*pdf(x)
integrate(xp, 0, Inf)


# 확률분포
x = 0:3
p = c(1, 3, 3, 1)
y = x^2
# 기댓값 계산
disc.exp(y, p)



# [응용] 그래프
disc.exp(y, p, prt=FALSE, plot=TRUE)


# 확률분포
pdf = function(x) 2*exp(-2*x)*(x>0)
y = function(x) 3*x-3

# 연속형 확률변수의 기댓값 계산 및 플롯
yp = function(x) y(x)*pdf(x)
integrate(yp, 0, Inf)[[1]]



# 표본공간 생성
S = rolldie2(2); N = nrow(S)
# 최대치 X, 최소치 Y
X = apply(S, 1, max); Y = apply(S, 1, min)
# 결합도수분포
tXY = table(X, Y)
# 함수 disc.jexp() 실행
disc.jexp(tXY)



# 결합/주변 도수분포 출력
disc.jexp(tXY, prt="", pprt=TRUE)


# 확률밀도함수 정의
pdf = function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)
# 함수 cont.jexp() 실행
cont.jexp(pdf)



# [응용] 반환값 활용
cont.jexp(pdf, prt="")$Exy



# 확률분포 (도수만 입력해도 계산됨)
x = 1:6; f = rep(1,6)
# 이산형 확률변수의 기댓값 계산 및 플롯
disc.exp(x, f)


# 확률밀도함수 정의
pdf = function(x) 2*exp(-2*x)*(x>0)
# 연속형 확률변수의 분산 계산
xp = function(x) x*pdf(x)
x2p = function(x) x^2*pdf(x)
Ex = integrate(xp, 0, Inf)[[1]]
Ex2 = integrate(x2p, 0, Inf)[[1]]
Vx = Ex2 - Ex^2
cat("Var(X) =",round(Ex2, 4),"-",round(Ex, 4),"^2 = ",round(Vx, 4),"\n")


# 확률분포 (도수만 입력)
x = 1:6; f = rep(1,6)
y = 100*x - 400
# 이산형 확률변수의 기댓값 계산 및 플롯
disc.exp(y, f)



# 확률밀도함수와 확률변수 Y 정의
fx = function(x) 2*exp(-2*x)*(x>0)
y = function(x) 20*x - 10
# 연속형 확률변수의 분산 계산
yp = function(x) y(x)*fx(x)
y2p = function(x) y(x)^2*fx(x)
Ey = integrate(yp, 0, Inf)[[1]]
Ey2 = integrate(y2p, 0, Inf)[[1]]
Vy = Ey2 - Ey^2
cat("Var(Y) =",round(Ey2, 4),"-",round(Ey, 4),"^2 = ",round(Vy, 4), "\n")


# 표본공간 생성
S = rolldie2(2); N = nrow(S)
# 최대치 X, 최소치 Y
X = apply(S, 1, max); Y = apply(S, 1, min); tXY = table(X, Y)
# 두 이산형 확률변수의 공분산 계산
disc.jexp(tXY, prt="cov")


# 확률밀도함수
pdf = function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)
# 두 연속형 확률변수의 공분산 계산
cont.jexp(pdf, prt="cov")


# 표본공간 생성
S = rolldie2(2)
# 최대치 X, 최소치 Y
X = apply(S, 1, max)
Y = apply(S, 1, min)
tXY = table(X, Y)

# 두 이산형 확률변수의 상관계수 계산과정 출력
disc.jexp(tXY, prt="cor")


# 확률분포
pdf = function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)

# 두 연속형 확률변수의 상관계수 계산과정 출력
cont.jexp(pdf, prt="cor")



# 상관계수만 출력
cont.jexp(pdf, prt="")$Cxy



# 표본공간 생성
S = rolldie2(4); (N = nrow(S))
[1] 1296
# 확률변수 생성
item = c("합", "범위", "최대치", "최소치")
X = list()
X[[1]] = apply(S, 1, sum)
X[[3]] = apply(S, 1, max)
X[[4]] = apply(S, 1, min)
X[[2]] = X[[3]]-X[[4]]
# 기댓값과 분산, 공분산, 상관계수 계산
corr.plot(X, Mt, item)
# 그래프 제목 설정
Mt = paste("주사위 4개 눈의", item, "확률분포")
# [응용] 함수 corr.plot() 실행
corr.plot(X, Mt, item, prt=F, pprt=T, plot=T)













