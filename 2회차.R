english <- c(90,80,60,70)
english

math <- c(50,60,100,20)
math

# english,math로 데이터 프레임 생성해서 df_midterm에 할당
df_midterm <- data.frame(english,math)
df_midterm

class <- c(1,1,2,2)
class

df_midterm <- data.frame(english,math,class)
df_midterm

mean(df_midterm$english) # df_midterm의 english로 평균 산출

mean(df_midterm$math) # df_midterm의 math로 평균산출

#데이터 프레임 한번에 만들기
df_midterms <- 
  data.frame(english =c(90,80,60,70),
             math=c(50,60,100,20), 
             class=c(1,1,2,2))
df_midterms

st <- data.frame(제품=c("사과","딸기","수박"),
                   가격=c(1800,1500,3000),
                   판매량=c(24,38,13))
st
mean(st$가격)
mean(st$판매량)

#엑셀 파일 불러오기
# readxl 패키지 설치 install.packages("readxl")
# readxl 패키지 로드
library(readxl)

df_exam <- read_excel("Data/excel_exam.xlsx") # 엑셀 파일을 불러와서 df_exam에 할당
df_exam # 출력
# setwd("c:/Work_R") 지정장소에서 작업을 실시
# getwd()입력하면 data경로를 찾을수 있다.

mean(df_exam$english)
mean(df_exam$science)
#엑셀 파일 첫 번째 행이 변수명이 아니라면?
df_exam_novar <- read_excel("Data/excel_exam_novar.xlsx",col_names = F) # 컬럼이름이 없어서 false로 인식되어 전체 출력되게 실제로 엑셀에서 이름이 없다
df_exam_novar

df_exam_novar1 <- read_excel("Data/excel_exam_novar.xlsx")
df_exam_novar1

# 엑셀 파일에 시트가 여러개 있을때
df_exam_sheet <- read_excel("Data/excel_exam_sheet.xlsx", sheet=3) # 엑셀 3번째 시트 출력
df_exam_sheet

#CSV 파일 불러오기
# 1. 범용 데이터 형식, 2. 값 사이를 쉼표(,)로 구분 3. 용량작음,다양한 소프트웨어에서 사용
df_csv_exam1 <- read.csv("Data/csv_exam.csv")
df_csv_exam1

#문자가 들어있는 파일을 불러올 때는 stringsAsFactors = F
df_csv_exam <- read.csv("Data/csv_exam.csv",stringsAsFactors = F)
df_csv_exam


library(readxl)
setwd('c:/Work._R/Data') # 전체 경로를 미리 지정하고 다음에 파일 이름만으로 결로를 불러올수 있다.
df_exam <- read_excel("excel_exam.xlsx")
df_exam
df_csv_exam <- read.csv("csv_exam.csv") # csv로 불러오기
write.csv(df_midterm, file = "df_midterm.csv") # csv로 저장하기

# df_midterms폴더로 저장하기
df_midterms <- 
  data.frame(english =c(90,80,60,70),
             math=c(50,60,100,20), 
             class=c(1,1,2,2))
df_midterms
write.csv(df_midterm, file ="df_midterm.csv")

getwd()
exam <- read.csv("Data/csv_exam.csv")
exam
#head() - 데이터 앞부분 확인하기
head(exam) # 앞에서부터 6행까지 출력
head(exam,7)

#tail() - 데이터 뒷부분 확인하기
tail(exam) # 뒤에서 6행까지 출력

#View() - 뷰어 창에서 데이터 확인하기
View(exam) # V를 대문자로

#dim() - 몇행 몇열로 구성되는지 알아보기
dim(exam) 

#str() - 속성 파악하기
str(exam)

#summary() - 요약통계량 산출하기
summary(exam)

# ggplo2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)
mpg
head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)

# 풀어보기
setwd('c:/Work._R/Data')
getwd()

#dplyr 패키지 설치 및 로드
#install.packages("dplyr")
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw
# 데이터 프레임 복사본 만들기
df_new <- df_raw 
df_new 
# 변수명 바꾸기 rename() 새변수명 = 기존변수명 순서로 변경
df_new <- rename(df_new,v2 = var2) # var2를 v2로 변경
df_new

mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg
mpg1 <- rename(mpg1, city=cty, highway=hwy)
head(mpg1)

df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))
df
# 파생변수 생성
df$var_sum <- df$var1 + df$var2
df
df$var_mean <- (df$var1+df$var2)/2
df
mpg <- as.data.frame(ggplot2::mpg)
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
mean(mpg$total)
summary(mpg$total) #요약 통계량 산출
hist(mpg$total) # 히스토그램 생성

# 조건문으로 합격 판정 변수 만들기
mpg$test <- ifelse(mpg$total >= 20, "pass","fail")
head(mpg,20)

# 빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test)

# 막대 그래프빈 빈도 표현하기
library(ggplot2)
qplot(mpg$test) # 연비 합격 빈도 막대 그래프 생성

#total을 기준으로 A,B,B등급 부여
mpg$grade <- ifelse(mpg$total >= 30,"A",
                    ifelse(mpg$total >=20, "B","C"))
head(mpg,50)
table(mpg$grade)
qplot(mpg$grade)
#A,B,C,D 등급 부여
mpg$grade2 <- ifelse(mpg$total >=30,"A",
                     ifelse(mpg$total >= 25,"B",
                            ifelse(mpg$total >=20,"C","D")))
head(mpg,50)
table(mpg$grade2)
qplot(mpg$grade2, fill=mpg$grade2)

# 분석도전
# 1
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

# 2
midwest <- rename(midwest, total=poptotal, asian=popasian)
head(midwest)

# 3
midwest$asian_total <- midwest$asian/midwest$total*100
hist(midwest$asian_total)

# 4
midwest$avg <- mean(midwest$asian_total)
midwest$asian_total_avg <- ifelse(midwest$asian_total>midwest$avg,"large","small")

# 5
table(midwest$asian_total_avg)

# 조건에 맞는 데이터만 추출하기
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

# exam에서 class가 1인 경우만 추출하여 출력
# filter()에 조건을 입력할때 같다는 의미하는 등호 == 두번 반복
exam %>% filter(class == 1)
# %>%기호는 파이프 연산자(pipe operator)라고 읽습니다.
# 단축키 [Ctrl+Shit+M]으로 %>% 기호 입력
exam %>% filter(class != 1) # 1반이 아닌 경우

# 초과, 미만, 이상, 이하 조건걸기
exam %>% filter(math > 50) # 수학점수가 50이상
exam %>% filter(english >=80)

# 여러 조건 중 하나 이상 충족하는 행 추출하기
exam %>% filter(math >=90 | english >=90) # 수학점수가 90점 이상이거나 영어점수가 90점이상인 경우

# 목록에 해당되는 행 추출하기
exam %>% filter(class==1 | class==3 | class==5 )

# %in% 기호 이용하기 위와 같은 결과가 나온다.
exam %>% filter(class %in% c(1,3,5))

# 추출한 행으로 데이터 만들기
class1 <- exam %>% filter(class==1)
class1
class2 <- exam %>% filter(class==2)
class2
mean(class1$math)
mean(class2$math)

# 혼자서 해보기
#1
mpg <- as.data.frame(ggplot2::mpg)
head(mpg,20)
displ_4 <- mpg %>% filter(displ <= 4)
displ_5 <- mpg %>% filter(displ >= 5)
mean(displ_4$hwy)
mean(displ_5$hwy)

#2
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

#3
mpg_3 <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_3$hwy)

# 필요한 변수만 추출하기
exam %>% select(math)
exam %>% select(class,math,english)
# 변수 제외하기
exam %>% select(-math)
exam %>% select(-math,-english)

#dplyr 함수 조합하기
# class가 1인 행만 추출한 다음 english추출
exam %>% 
  filter(class==1) %>% 
  select(english) 

# 일부만 출력하기 head
exam %>% 
  select(id,math) %>% 
  head(10)

#혼자서 해보기
head(mpg,20)
# 1
mpg_2 <- mpg %>% select(class,cty)
mpg_2

# 2
mpg_suv <- mpg_2 %>% filter(class == "suv")
mean(mpg_suv$cty)
mpg_compact <- mpg_2 %>% filter(class == "compact")
mean(mpg_compact$cty)

# 순서대로 정렬하기
# 오름차순 정렬(arrange)
exam %>% arrange(math)
# 내림차순 정렬(desc)
exam %>% arrange(desc(math))
# 정렬 기준 변수 여러개 지정
exam %>% arrange(class,math) # class 및 math 오름차순 정렬