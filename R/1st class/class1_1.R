# 도수분포표 작성 ⇒ 함수[1] freq.table() 실행
data(iris); str(iris); x=iris[[2]]
freq.table(x)

# 저장된 데이터(exa2_2) 불러오기 ⇒ data() 함수 사용
data(exa2_2); x = exa2_2
# 데이터 구조 확인 ⇒ str( ) 함수 사용
str(x)
# (1) 신입생의 입학전형 분포표
# 도수 계산 ⇒ table( ) 함수 사용
table(x$입학전형)
# 상대빈도 계산 ⇒ prop.table( ) 함수 사용
prop.table(table(x$입학전형))
# (2) 신입생의 참여활동 분포표
table(x$참여활동)
prop.table(table(x$참여활동))
# (3) 신입생의 입학전형과 참여활동 결합분포표
table(x$입학전형, x$참여활동)
# [참고] ‘x$’를 안 쓰는 방법 ⇒ with() 함수 사용
with(x, table(입학전형, 참여활동))
with(x, addmargins(table(입학전형, 참여활동)))
with(x, round(addmargins(prop.table(table(입학전형, 참여활동))), 4))


# 그래픽 화면 분할
par(mfrow=c(2,2))
# 4개의 히스토그램 작성
for (k in 1:4) hist(iris[[k]])

# 그래프 꾸미기
제목 = paste0(colnames(iris[1:4]), 
	"의 히스토그램"); 제목
색상 = c("yellow", "lightgreen", 
	"lightpink", "skyblue")
# 히스토그램 다시 작성
for (k in 1:4) hist(iris[[k]], main=제목[k], 
	xlab=colnames(iris[k]),
	ylab="도수", col=색상[k])

# 그림을 저장하거나 복사하려면, 그림 위에서 오른쪽 마우스를 클릭!


# 함수[2] unstable.hist() 실행
unstable.hist()

# [응용] 파라미터 변경(N=100, m2=4, a=11, b=12, vc=무지개 색
unstable.hist(N=100, m2=4, a=11, 
	b=12, c=8, vc=rainbow(4))



# 함수 strat.hist() 실행
strat.hist()


# [응용 1] 표준편차(s) 변경
strat.hist(s=c(0.5, 1, 1.5))
# [응용 2] 층의 개수(ng)=3 -> 4, 층별 평균(m) -> c(6,9,12,15) 변경
strat.hist(ng=4, m=(2:5)*3)



# 저장된 데이터(exa2_3) 불러오기
data(exa2_3);  x = exa2_3
# 데이터 구조 확인 ⇒ 100개의 데이터가 10개씩 10열로 들어 있음
str(x)


# 데이터프레임을 행렬 형태로 변환 ⇒ as.matrix( ) 함수 사용
x = as.matrix(x)
# 줄기-잎 그림 ⇒ stem() 함수
stem(x)


# 행렬 데이터를 1열로 변환 ⇒ matrix() 함수
data(exa2_3); x2 = matrix(as.matrix(exa2_3), ncol=1)
# 그래프 창 크기를 납작하게 바꿈 (7×7) ⇒ (7×4)
win.graph(7, 4)
# 상자그림 (가로로 그림) ⇒ boxplot() 함수
boxplot(x2, horizontal=T, main="저항 데이터의 상자그림", col="cyan")
# 데이터 점 표시 ⇒ points() 함수
points(x2, rep(1,100))
# 통계량 계산 ⇒ fivenum() 함수 ⇒ (최소치, Q1, Q2, Q3, 최대치)
xfn = fivenum(x2); xfn

# 통계량 표시 (최소치, Q1, Q2, Q3, 최대치) ⇒ text(x, y, labels) 함수
text(xfn, 0.65, labels=xfn, pos=3)
# 통계량 범례 표시 (평균, 표준편차, 표본개수) ⇒ legend() 함수
# 문자열을 공백 없이 묶을 때는 paste0() 함수 사용
legend("topright", paste0(c("평균", "표준편차", "표본개수"), "=", 
	round(c(mean(x2), sd(x2), length(x)), 4)), text.col=4) 


# 행렬이나 데이터프레임 형태의 데이터는 자동으로 열별로 분류됨
# 층화 상자그림 작성 (boxwex=상자 두께)
x = as.matrix(tab2_3)
win.graph(7, 5)
boxplot(x, main="저항데이터의 열별 상자그림", boxwex=0.3, 
	ylab="저항", col=7)
grid(col=3)

# 데이터 점 표시 (x의 열별로 10개씩)
points(rep(1:10, each=10), x, pch=19, cex=0.8, col=2)

# 열별 통계량(최소치, 사분위수, 최대치) 표시 ⇒ apply( ) 함수
xstat = apply(x, 2, fivenum)
text(rep(1:10, each=5), xstat, labels=xstat, col=4, cex=0.7, pos=4)



# 함수[4] corr.plot6() 실행
corr.plot6()
# [응용] 인수 변경 (x평균, 표준편차, y평균, 표준편차, 상관계수, 표본개수)
corr.plot6(m1=50, s1=10, m2=65, s2=5, r=0.8, r2=0.9, n=100)


# 기본 함수 plot()과 함수[5] scat.lm() 실행
plot(mtcars$wt, mtcars$mpg)  	# 매우 밋밋함
scat.lm(mtcars$wt, mtcars$mpg) 	# 제목과 축 라벨이 이상함
# [응용] 그래프 제목과 축 라벨 넣기
with(mtcars, scat.lm(wt, mpg, mt="자동차 중량 대 연비 산점도", 
	xl="중량(wt)", yl="연비(mpg)"))


# 산점 행렬도 ⇒ pairs( ) 함수
pairs(mtcars[c(1,2,4,6)]) 	# 다소 밋밋함
# 다른 방법(~ 식) 적용 ⇒ 같은 그림
pairs(~mpg+cyl+hp+wt, data=mtcars)

# [응용] 제목 추가, panel() 함수 사용 ⇒ 점에 색 채우기, 회귀직선 추가
pairs(~mpg+cyl+hp+wt, data=mtcars, 	main="자동차 특성치 산점행렬도", 
	panel=function(x,y) { 
	    points(x,y, pch=19, col=4)
	    abline(lm(y~x), col=2) } )

# 자동차 연비는 기통(cyl), 출력(hp), 중량(wt) 등 모두와 음의 상관관계

# 기통(cyl), 출력(hp), 중량(wt) 등은 서로 간에 모두 양의 상관관계

# 데이터 읽기 (데이터프레임 --> 행렬로 변환함)
data(exa2_3); x = as.matrix(exa2_3)
# 함수[6] location.est() 실행, 계산 결과만 출력
location.est(x)


# 함수[7] spread.est() 실행, 계산 결과만 출력
data(exa2_3); x = as.matrix(exa2_3)
spread.est(x)

