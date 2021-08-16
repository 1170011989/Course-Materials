# 정규분포의 R 함수

# 확률밀도함수 (mean=기댓값, sd=표준편차)
# mean, sd를 생략하면 표준정규분포로 계산함 (mean = 0, sd = 1)
dnorm(x, mean = 0, sd = 1)
# Excel 함수 = NORM.DIST(x, mean, sd, FALSE)

# 누적분포함수 (q=분위수)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE)
# Excel 함수 = NORM.DIST(x, mean, sd, TRUE)

# 분위수 (p=누적확률)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE)
# Excel 함수 = NORM.INV(p, mean, sd)

# 정규 확률변수 (n=난수의 개수)
rnorm(n, mean = 0, sd = 1)
# Excel 함수 = NORM.INV(RAND( ), mean, sd) ⇒ 난수 1개 생성




# 정규분포 모수 지정 및 함수 cont.spdf() 실행
mu = c(0,0,2,2); sig=c(1,2,1,2)
cont.spdf("norm", -7, 7, mu, sig, xp=mu)


# 표준정규분포 사용
pnorm(1); pnorm(-1.5)

pnorm(1)-pnorm(-1.5)


# 정규분포 모수, 확률계산 영역 설정
mu = 2; sig = 2; a = -1; b = 4
# 함수 norm.trans() 실행
norm.trans(mu, sig, a, b)


# 표준정규분포 (0 ~ 2.99), 0.01 단위, 10개 열
pv <- matrix(pnorm(0:299/100), ncol=10, byrow=T)
colnames(pv) <- 0:9/100
rownames(pv) <- 0:29/10
print(round(pv, 4))


# 표준정규분포 누적확률 그래프에서 표시할 점 지정
zp = seq(-2, 2, by=0.5)
# 누적확률 그래프 작성 함수 snorm.cdf() 실행
snorm.cdf(zp)

# 누적확률 표시 점 지정 및 함수 snorm.prob() 실행
zp = 1:4; snorm.prob(zp)

# 분위수 계산/표시 누적확률 지정
pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
# 표준정규 분위수 작성 함수 snorm.quant() 실행
snorm.quant(pv, pv)


# pnorm(x, mean, sd) 사용
pnorm(185, 175, 8) - pnorm(180, 175, 8)



# qnorm(p, mean, sd) 사용
qnorm(0.05, 10, 1.5)

# 표준화 사용
10+qnorm(0.05)*1.5



# pnorm(p, mean, sd) 사용
pnorm(0, 21.5-20, sqrt(0.4^2 +0.3^2))




# pnorm(x, mean, sd) 사용
pnorm(60, 50, 4)-pnorm(40, 50, 4)




# 정확한 계산 a pbinom(x, n, p) 사용
pbinom(4, 25, 0.2)


# 정규근사 a pnorm(x, mean, sd) 사용
pnorm(4, 5, 2)


# 연속성 보정
pnorm(4.5, 5, 2)





# 정확한 계산
pbinom(45, 100, 0.5)-pbinom(39, 100, 0.5)


# 정규근사 (연속성 보정)
pnorm(-0.9)-pnorm(-2.1); pnorm(45.5, 50, 5) - pnorm(39.5, 50, 5)











# 카이제곱분포의 R 함수

# 카이제곱분포의 확률밀도함수 (df=자유도, ncp=비중심모수(미사용))
dchisq(x, df, ncp = 0)
# Excel 함수 = CHISQ.DIST(x, df, FALSE)

# 카이제곱분포의 누적분포함수 F(x)
pchisq(x, df, ncp = 0, lower.tail = TRUE)
# Excel 함수 = CHISQ.DIST(x, df, TRUE)

# 카이제곱분포의 분위수 (p=누적확률)
qchisq(p, df, ncp = 0, lower.tail = TRUE)
# Excel 함수 = CHISQ.INV(p, df)

# 카이제곱분포의 확률변수 (n=난수의 개수)
rchisq(n, df, ncp = 0)
# Excel 함수 = CHISQ.INV(RAND( ), df) ⇒ 한 개의 난수 생성



# 누적확률 표시 점 지정 및 함수 chi.prob() 실행
k= 1:10; nu= 5; chi.prob(nu, k)



nu = 5
# 표에서 나타낼 값
pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
# 그래프에서 나타낼 값, chi.quant() 실행
pv2 = c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
chi.quant(nu, pv, pv2)



# 자유도 입력 및 x-축 값 생성
nu = c(5, 10, 15, 20); up = qchisq(0.99, max(nu))
# 함수 cont.spdf() 실행
cont.spdf("chi", 0, up, para=nu, xp=nu)




# 자유도, 누적확률 입력
nu <- c(5, 10, 30, 100)
p <- 0.95

# 분위수  (상수 a)
qchisq(p, nu)


# 기댓값 대비 95% 분위수 비율
qchisq(p, nu) / nu











# t-분포의 확률밀도함수 f(x) (df=자유도, ncp=비중심모수(미사용))
dt(x, df, ncp)
# Excel 함수 = T.DIST(x, df, FALSE)

# t-분포의 누적분포함수 F(x)
pt(x, df, ncp, lower.tail = TRUE)
# Excel 함수 = T.DIST(x, df, TRUE)

# t-분포의 분위수 (p=누적확률)
qt(p, df, ncp, lower.tail = TRUE)
# Excel 함수 = T.INV(p, df)

# t-분포의 확률변수 (n=난수의 개수)
rt(n, df, ncp)
# Excel 함수 = T.INV(RAND( ), df) ⇒ 한 개의 난수 생성



# 자유도 지정
nu = c(1, 5, 10, 30)
# 확률밀도 비교 함수 tnorm.comp() 실행
tnorm.comp(nu)



# 표시 확률 지정
p = c(5:9/10, 0.95, 0.975, 0.99, 0.995, 0.999, 0.9995); nc = length(p)
# 자유도 지정
df = c(1:40, 50, 100, Inf); nr = length(df)
# 분위수 생성
qv = array(0, dim=c(nr, nc))
colnames(qv) = p; rownames(qv) = df
for (i in 1:nc) qv[,i] = qt(p[i], df)
print(round(qv, 3))













# F-분포의 R 함수
# F-분포의 확률밀도함수 (df1, df2 =자유도, ncp=비중심모수(사용하지 않음))
df(x, df1, df2, ncp)
# Excel 함수 = F.DIST(x, df1, df2, FALSE)
# F-분포의 누적분포함수 F(x)
pf(x, df1, df2, ncp, lower.tail = TRUE)
# Excel 함수 = F.DIST(x, df1, df2, TRUE)
# F-분포의 분위수 (p=누적확률)
qf(p, df1, df2, ncp, lower.tail = TRUE)
# Excel 함수 = F.INV(p, df1, df2)
# F-분포의 확률변수 (n=난수의 개수)
rf(n, df1, df2, ncp)
# Excel 함수 = F.INV(RAND( ), df1, df2)





# (1) F(8,5)-분포에 대해 함수 fdist.sim() 실행
fdist.sim(nu1=8, nu2=5)




# 누적확률 표시 점 및 자유도 지정
k= 1:7; nu1 = 8; nu2= 5
# 함수 f.prob() 실행
f.prob(nu1, nu2, k)





# 자유도 및 F-분포 계산/표시 누적확률 지정
nu1 = 8; nu2 = 5
pv = c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
pv2 = c(0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
# 함수 f.quant() 실행
f.quant(nu1, nu2, pv, pv2)




# 자유도 지정 및 x-축 값 생성
nu1 = c(3, 3, 20, 20); nu2 = c(3, 20, 3, 20)
up = max(qf(0.95, nu1, nu2))
# 함수 cont.spdf() 실행
cont.spdf("f", 0, up, nu1, nu2, xp=nu2/(nu2-2))




