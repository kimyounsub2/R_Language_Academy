getwd()
# 부모학년이 자녀 대학 합격여부에 연관이 있는지 검증하기
data <- read.csv("data/Part-III/descriptive.csv",header = T)
head(data)

# 데이터 특성보기
dim(data) # 행과 열 차원보기
length(data) # 열 길이
length(data$survey) # 컬럼의 관찰치
str(data) # 데이터구조보기 -행/열,data
str(data$survey)
summary(data) # 데이터 특성(최소,최대,평균,분위수,노이즈-NA) 제공

# 명목상 의미 없는 수치로 표현된 변수 - 성별
length(data$gender)
summary(data$gender) # 최소,최대,중위수,평균 - 의미없음
table(data$gender) # 각 성별 빈도수

# 성별 outline제거
data <- subset(data,data$gender == 1 | data$gender == 2)
#data 테이블을 대상으로 성별이 1또는 2인 데이터 대상 subset만듬
x <- table(data$gender)
barplot(x) # 범주형
prop.table(x) # 비율계산 : 0 < x < 1
y <- prop.table(x)
round(y*100,2) # 백분율 적용 (소숫점 2자리)

# 계급순위를 수치로 표현한 변수 - 학력수준
length(data$level)
summary(data$level) # 명목척도와 함께 의미 없음
table(data$level) # 빈도분석 - 의미 있음
x1 <- table(data$level) # 각 학력수주에 빈도수 저장
x1
barplot(x1) # 명목/ 서열척도 -> 막대차트
# 부모의 학력이 고졸인 경우 115 대졸 99, 대학원 70으로 나타남

# 등간척도 변수의 기술통계량
# 속성의 간격이 일정한 변수 - 덧셈/뺄셈 연산가능
survey <- data$survey
survey
summary(survey) # 만족도(5점 척도)인 경의 의미가 있다
x1 <- table(survey) # 빈도수
x1
hist(survey) # 연속형 척도 시각화 -> 범주화 -> 히스토그림

# 비율척도 변수의 기술통계량
# 수치로 직접 입력한 변수
length(data$cost)
summary(data$cost) # 요약통계량 - 의미있음(mean) - 8.784,생활비 통계량
mean(data$cost) # NA

# 데이터 정제 - 결측치 제거 및 outline 제거
plot(data$cost)
data <- subset(data,data$cost >= 2 & data$cost <= 10) # 총점기준
data
x <- data$cost
x
mean(x) # 평균 5.36
median(x)
min(x)
max(x)
range(x)
sort(x)
sort(x, decreasing = T)

sd(x)
var(x)
# 표준편차 : 표본의 평균에서 얼마나 떨어져 있는가 - 산포도
quantile(x,1/4) # 1사분위수 - 25%, 4.6
quantile(x, 3/4) # 3 사분위수 -75%, 6.2

# 왜도/첨도 사용을 위한 패키지
# install.packages("moments") 
library(moments) # 왜도/첨도 사용을 위한 패키
cost <- data$cost
# 왜도 - 평균 중심으로 기울어짐 정도
skewness(cost)
# 0보다 작으면, 왼쪽방향 비대칭 꼬리, 0보다 크면, 오른쪽 방향 비대칭꼬리
# 0에 근사하면 중심으로 좌우대칭

# 첨도 - 표준정규분포와 비교하여 얼마나 뾰족하낙 측정 지표
kurtosis(cost)
# 표준정규분포와 비교하여 첨도가 3이면 정규분포 곡성르 이루고, 
# 첨도가 3보다 크면 정규분포 보다 표족한 형태, 3보다작으면
# 정규분포 보다 완만한 형태이다.

# 히스토그램으로 왜도/첨도 확인
hist(cost)
# 왼쪽방향 비대칭 꼬리, 정규분포 첨도 보다 완만함
# 데이터가 정규분포 형태를 띄고 있는가의 여부를 알기위해 비대칭인지 확인하자

install.packages("Hmisc")
library(Hmisc)

# 전체 변수 대상 기술통계량 제공 - 빈도와 비율 데이터 일괄 수행
describe(data)
# 명목,서열,등간척도 - m,missing,unique, 빈도수, 비율
# 비율척도 -n, missing, unique, mean, lowest, highest

# 개별 변수 기술통계량
describe(data$gender) # 특정변수(명목) 기술통계량 - 비율 제공
describe(data$age) # 특정 변수(비율) 기술통계량 - lowest, highest
summary(data$age)

# Hmisc보다 더 유용한 통계 패키지이다.
install.packages("prettyR")
library(prettyR)

# 전체 변수 대장
freq(data) # 각 변수별 : 빈도, 결측치, 백분율, 특징-소수점 제공
# 개별 변수 대상
freq(data$gender) # 빈도와 비율 제공

# 기술통계량 보고서 데이터 작성
# 거주지역 변수 리코딩
data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >= 2 & data$resident <=4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"
x <- table(data$resident2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)

# 성별 변수 리코딩
data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"

x <- table(data$gender2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)

# 나이 변수 리코딩
summary(data$age)
data$age2[data$age <= 45] <- "중년층"
data$age2[data$age >= 46 & data$age <=59] <- "장년층"
data$age2[data$age >= 60] <- "노년층"
head(data)

x <- table(data$age2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)

# 학력수준 리코딩
data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"

x <- table(data$level2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)

# 합격여부 리코딩
data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "실패"

x <- table(data$level2)
prop.table(x)
y <- prop.table(x)
round(y*100,2)

head(data)

