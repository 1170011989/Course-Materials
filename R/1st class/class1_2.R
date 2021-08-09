# 표본공간 생성 (차례로 던지는 경우) ⇒ rolldie2(n) 함수
S = rolldie2(2); nrow(S)

# 동시에 던지는 경우 ⇒ 순서 무시됨 ⇒ 표본공간 축소 ⇒ subset(n) 함수
S2 = subset(S, X1 <= X2); nrow(S2)

# 표본공간 정렬 (X1, X2 순으로) ⇒ order( ) 함수
S2 = S2[order(S2$X1,S2$X2),]

# subset() 함수를 사용하여 사상 정의
# 사상 A (합이 짝수) ⇒ (x %% y) 함수 ⇒ x를 y로 나눈 나머지
A = subset(S2, ((X1+X2) %% 2)==0); element(A, 20)

# 사상 B (합이 8 이상)
B = subset(S2, (X1+X2) >=8); element(B)

# 사상 C (눈금 차이가 1 이하)
C = subset(S2, abs(X1 - X2)<=1); element(C, 20)

# 교사상(intersection) ⇒ intersect2( ) 함수
AB = intersect2(A, B); element(AB)

AC = intersect2(A, C); element(AC)

BC = intersect2(B, C); element(BC)

ABC = intersect2(A, BC); element(ABC)

# 여사상(complement) ⇒ setdiff2( ) 함수 사용
Bc = setdiff2(S2, B); element(Bc, 20)

# 교사상(intersection)
ABc = intersect2(A, Bc); element(ABc)

# 사상 A (눈금의 차가 3 이상)
A = subset(S2, abs(X1-X2)>=3); Ae = element(A)

# 사상 B (눈금의 곱이 20 이상)
B = subset(S2, X1*X2 >= 20); Be = element(B)

# 상호배반인지 확인 ⇒ 교사상의 원소가 없으므로 상호배반임
intersect2(Ae, Be)
character(0)

# 패키지 ‘animation' 설치 & # 설치가 끝난 후 패키지 ‘animation' 불러옴
install.packages("animation")
library(animation)
# 패키지 ‘animation' 옵션 설정 (최대 시행횟수, 시간간격) & # 창 크기 조정
ani.options(nmax = 500, interval = 0.01); win.graph(7,4)
# 대수의 법칙 ⇒ lln.ani() 함수 실행
lln.ani(FUN = function(n, mu) rbinom(n, size=1, prob = mu), mu = 0.5, 
	type="n", col.poly="blue")
# 제목, 격자 표시
title(main="Law of Large Numbers (동전 던지기)"); grid()

# 표본공간 생성 및 정렬
S = tosscoin2(4); S = S[order(S$X1, S$X2, S$X3, S$X4),]
# 표본공간 원소 출력 (한 줄에 8개 씩)
element(S, 8)

# 앞면의 개수를 세는 함수 정의
counth = function(x) sum(x=="H")
# 앞면이 두 번 이상 나오는 사상 (한 줄에 6개 씩 출력)
A = subset(S, apply(S, 1, counth)>=2); element(A, 6)

# 확률 계산 ⇒ pprt() 함수; nrow() 함수는 행의 개수(원소의 개수)를 구함
pprt(A, nrow(S))

# 표본공간 생성 (원소의 개수가 많아 나열하지 않음)
S = rolldie2(4); (N= nrow(S))

# (1) 숫자의 합이 15 이상인 사상의 확률 출력 ⇒ pprt() 함수
A = subset(S, X1+X2+X3+X4 >=15); pprt(A, N)

# (2) 한 개 이상의 6이 나오는 사상 (행(원소) 별로 최댓값 적용)
B = subset(S, apply(S, 1, max)==6); pprt(B, N)

# (3) 한 개 이상의 1이 나오는 사상 (행(원소) 별로 최솟값 적용)
C = subset(S, apply(S, 1, min)==1); pprt(C, N)

# (4) 교사상(intersection) ⇒ intersect2() 함수
AB=intersect2(A,B); AC=intersect2(A,C); BC=intersect2(B,C); ABC=intersect2(AB,C) 
pprt(AB, N); pprt(AC, N); pprt(BC, N); pprt(ABC, N)

# 사상의 이름(번호)을 벡터 형태로 저장
library(gplots); Av=rownames(A); Bv=rownames(B); Cv=rownames(C)
# 벤다이어그램 작성
win.graph(7,4); par(mar=c(0,2,0,2)) # 창 크기, 마진 조정
venn(list(Av, Bv, Cv), showSet=T)

AuB=union2(A,B); AuC=union2(A,C); BuC=union2(B,C); AuBuC=union2(AuB,C)
pprt(AuB, N); pprt(AuC, N); pprt(BuC, N); pprt(AuBuC, N)

# sort()는 오름차순 정렬 함수, diff()는 원소 차이 계산 함수
# all()은 모든 원소가 참일 때 참(TRUE)이 되는 함수
# is.stra() 함수는 모든 원소가 연속되는 숫자인 경우에만 참(TRUE)이 됨
is.stra = function(x) all(diff(sort(x))==1)
stra = subset(S, apply(S, 1, is.stra)); nrow(stra)


# 연속숫자가 나올 확률 표시
pprt(stra, nrow(S))

# 여사상(complement)  ⇒ setdiff2( ) 함수 사용
Bc = setdiff2(S, B); Ac = setdiff2(S, A); Cc = setdiff2(S, C)
ABc = intersect2(A, Bc); pprt(ABc, N)

AcBc = intersect2(Ac, Bc); pprt(AcBc, N)

ABCc = intersect2(AB, Cc); pprt(ABCc, N)

ABcC = intersect2(ABc, C); pprt(ABcC, N)

# 숫자의 합이 15 이상인 사상의 조건부 확률 (앞의 예에 이어서)
cprt(A, B); cprt(A, C); cprt(A, BC); cprt(A, BuC)


# 카드 생성 (무늬만 고려하므로 숫자 무시)
CD = rep(c("C", "D", "H", "S"), each=13)
# 표본공간 생성 ⇒ urnsample2( ) 함수 사용
CD4 = urnsample2(CD, size = 4); (N = nrow(CD4))

# 무늬가 같은지 샘플만 추출, unique(x) 함수는 중복되지 않는 원소만 추출
# 따라서 sameshape() 함수는 중복되지 않는 원소가 1개일 때만 참(TRUE)
sameshape = function(x) length(unique(x))==1
Flush= subset(CD4, apply(CD4, 1, sameshape))
# 결과 출력
pprt(Flush, N)


# 표본공간 생성
S=rolldie2(5); (N=nrow(S))

# 함수 정의 (눈의 합이 짝수, 눈의 범위=5)
even=function(x) (sum(x)%%2)==0
span5=function(x) (max(x)-min(x))==5
# 사상 생성 (A: 눈의 합이 짝수, B: 눈의 범위=5)
A = subset(S, apply(S,1,even)); nrow(A)

B=subset(S, apply(S,1,span5)); nrow(B)

# 두 이산형 확률변수의 독립성 판정 함수 실행 ⇒ 두 사상은 독립
indep.event(A, B, N)

# 생산비율, 라인별 불량률
prior <- c(0.2, 0.4, 0.3, 0.1)
cond <- c(4, 2, 1, 5)/100

# 불량이 각 라인에서 발생할 확률
tot <- prior*cond; tot

# 합계
sum(tot)

# 사후확률 계산
prior = c(0.2, 0.4, 0.3, 0.1); cond = c(0.04, 0.02, 0.01, 0.05)
(tot = prior*cond); (stot = sum(tot))
(post = tot / stot)

# 결과 그림
bayes.plot(prior, post)
