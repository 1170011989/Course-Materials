## Rstat Package 설치 - working directory 혹은 package folder에 압축해제

# library path 확인
.libPaths();

# library path 추가 (현재 디렉토리) - 혹은 본인이 원하는 주소로
# 주의할 점은 'C:\Users' 가 아닌 'C:\\Users' 로 입력해야한다는 것
aa=getwd();
.libPaths('C:\\Users');

# library 구동
library(Rstat);

# 그 외 다른 패키지 설치 시에도 path만 잘 지정하면 됩니다.
