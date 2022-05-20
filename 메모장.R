a <-1
a + 5 
b <- 10
b + 20
##### 변수 #####
# <-(할당연산자) : alt + -
# 실행 + enter
# 전체 실행 : ctrl + alt + r

install.packages("xts") # foreign 패키지 설치
library(descr) # 빈도 분석 패키지
library(foreign) # SPSS 파일 로드
library(dplyr) # 전처리
library(ggplot2) # 시각화
library(readxl) # 엑셀 파일 불러오기
library(plotly) # 인터랙티브 그래프 만들기
library(dygraphs) # 인터랙티브 시계열 그래프 만들기
library(xts) # 시간 순서 속성을 지니는 xts 데이터 타입으로 변경
mpg <- as.data.frame(ggplot2::mpg)
# console 창 삭제 : ctrl + L
# ctrl + shift +c : 전체주석
x <- c(1, 2, 3)
x
mean(x)# 평균
max(x)# 최대
min(x)# 최소

# setwd("c:/Work_R") 지정장소에서 작업을 실시
# getwd()입력하면 data경로를 찾을수 있다.
# head() - 데이터 앞부분 확인하기
# tail() - 데이터 뒷부분 확인하기
# View() - 뷰어 창에서 데이터 확인하기
# dim() - 몇행 몇열로 구성되는지 알아보기
# str() - 속성 파악하기
# ls() - 변수명 파악하기
# summary() - 요약통계량 산출하기
# summarise 요약하기
# ggplo2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)
##### R에서 사용하는 기호들 #####
#!= 같지 않다
#│ 또는
#& 그리고
#%in% 매칭 확인

#자주 사용하는 요약통계량 함수
#함수 의미
mean() 평균
sd() 표준편차
sum() 합계
median() 중앙값
min() 최솟값
max() 최댓값
n() 빈도
