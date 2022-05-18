getwd()
exam <- read.csv("Data/csv_exam.csv")
exam

# 기본 R에서 제공하는 파생변수 사용법
exam$total = exam$math + exam$english + exam$science

library(dplyr)
# 파생변수 추가하기
exam %>% # dplyr패키지로 파생변수 생성하는 방법
  mutate(total = math + english + science) %>% 
  head 

# 여러 파생변수 한번에 추가하기
exam %>%
  mutate(total = math + english + science, # 총합 변수 추가
         mean = (math + english + science)/3) %>% # 총평균 변수 추가
  head # 일부 추출

# mutate()에 ifelse() 적용하기
exam %>% 
  mutate(test=ifelse(science >= 60, "pass","fail")) %>% 
  head

# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>% # 총합 변수 추가
  arrange(total) %>% # 총합 변수 기준 정렬
  head # 일부 추출

exam_result <- exam %>%
  mutate(total = math + english + science, 
         mean = (math + english + science)/3) %>% 
  mutate(test=ifelse(science >= 60, "pass","fail"))
exam_result

# 혼자서 해보기
# Q1. mpg 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.
mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg_new <- mpg # 복사본 만들기
mpg_new <- mpg_new %>% 
  mutate(total = cty + hwy) # 합산 변수 만들기

# Q2. 앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가세요.
mpg_new <- mpg_new %>% 
  mutate(mean = total/2) # 평균 변수 만들기

# Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.
mpg_new %>%
  arrange(desc(mean)) %>% # 내림차순 정렬
  head(3) # 상위 3 행 출력

# Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요. 데이터는 복사본 대신 mpg 원본을 이용하세요.
mpg %>%
  mutate(total = cty + hwy, # 합산 변수 만들기
         mean = total/2) %>% # 평균 변수 만들기
  arrange(desc(mean)) %>% # 내림차순 정렬
  head(3) # 상위 3 행 출력

# 집단별로 요약하기
exam %>% summarise(mean_math = mean(math))
exam %>% 
  group_by(class) %>% #class별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출

# 여러 요약 통계량 한번에 산출하기
exam %>%
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math), # math 평균
            sum_math = sum(math), # math 합계
            median_math = median(math), # math 중앙값
            n = n()) # 학생 수

# 각 집단별로 다시 집단 나누기
# group_by에 여러 변수를 지정하면 집단을 나눈 후 다시 하위 집단으로 나눌수 있다.
mpg %>% 
  group_by(manufacturer, drv) %>% # 회사별, 구방방식별 분리
  summarise(mean_cty = mean(cty)) %>% # cty 평균 산출
  head(10)

#dplyr 패키지로 조합하기
mpg %>% 
  group_by(manufacturer) %>% # 회사별로 분리
  filter(class == "suv") %>% # suv 추출
  mutate(tot = (cty+hwy)/2) %>% # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>% # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>% # 내림차순 정렬
  head(5) # 1~5 위까지 출력

# 데이터 합치기(컬럼 가로로 합치기)
test1 <- data.frame(id = c(1,2,3,4,5), # 중간고사 데이터 생성
                    midterm = c(60,80,70,90,85))
test1
test2 <- data.frame(id = c(1,2,3,4,5), # 기말고사 데이터 생성
                    final = c(70,83,65,95,80))
test2
# id 기준으로 합치기
total <- left_join(test1,test2, by = "id") # id기준으로 합쳐 total에 할당
total

# 다른 데이터 활용해 변수 추가하기
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("Kim","lee","park","choi","jung"))
name

# class 기준으로 합치기 컬럼 추가하기
exam_new <- left_join(exam, name, by ="class")
exam_new

# 세로로 합치기
# 학생 1~5 번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))
group_a
# 학생 6~10 번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_b

# bind_row : 데이터를 세로로 합칠수 있다. 두데이터의 변수명이 같아야 한다.
group_all <- bind_rows(group_a, group_b)
group_all

x <- c(1,2,3,4,5,6) # 벡터 x 생성
matrix(x, nrow = 2, ncol =3) # 변수 x를 2*3 행렬로 구성
matrix(x, nrow =3, ncol=2) # 변수 x를 3*2 행렬로 구성

y <- c(1,2,3,4,5,6) # 벡터 y 생성
array(y,dim=c(2,2,3)) # 변수 y를 2*2행렬, 3차원 배열로 구성

list1 <- list(c(1,2,3),"Hello") # 숫자형 벡터, 문자형 벡터로 구성된 list1생성
list1
str(list1)

# 다중형 데이터 세트
# data.frame을 구성하기 위한 변수 생성
ID <- c(1,2,3,4,5,6,7,8,9,10)
SEX <- c("F","M","F","M","M","F","F","F","M","F")
AGE <- c(50,40,28,50,27,23,56,47,20,38)
AREA <- c("서울","경기","제주","서울","서울","서울","경기","서울","인천","경기")

# 변수를 포함한 데이터 프레임 구조로 dataframe_ex 데이터 세트에 저장
dataframe_ex <- data.frame(ID,SEX,AGE,AREA)
dataframe_ex

# 데이터 프레임 생성하기
DAA <- data.frame(ID=ID, SEX=SEX)
View(DAA)

library(readxl)
excel_data_ex <- read_excel("data/data_ex.xls")
View(excel_data_ex)

ex_data <- read.table("data/data_ex.txt", fileEncoding = "euc-kr") 
View(ex_data)

ex_data1 <- read.table("data/data_ex.txt",fileEncoding = "euc-kr",header = TRUE)
View(ex_data1)

ex_data2 <- read.table("data/data_ex.txt",fileEncoding = "euc-kr",header = TRUE,skip = 2)
View(ex_data2)

ex_data3 <- read.table("data/data_ex.txt",fileEncoding = "euc-kr",header = TRUE,nrows=7)
View(ex_data3)

# 쉼표로 구분된 데이터 가져오기
# sep 옵션으로 구분자 지정 후 ex1_data로 저장
ex1_data <- read.table("data/data_ex1.txt"  ,fileEncoding = "euc-kr",header = TRUE, sep = ",")
View(ex1_data)

# 변수명을 추가하여가져오기
varname <- c("IDD","SEXX","AGEE","AREAA")
# col.names 옵션으로 변수명(varname 변수의 데디어 값) 지정후 ex2_data로 저장
ex2_data <- read.table("data/data_ex2.txt"  ,fileEncoding = "euc-kr",sep = ",",col.names = varname)
View(ex2_data)

# R데이터 저장하고 불러오기
save(dataframe_ex, file = "testdata.rda")
load("testdata.rda")
View(testdata)
getwd()

#CSV로 파일 저장하기
id <- c(1,2,3,4,5)
sex <- c("F","M","F","M","F")
data_ex <- data.frame(id=id,sex=sex)
write.csv(data_ex, file="data/data_ex.csv")

#TXT 파일로 저장하기
id <- c(1,2,3,4,5)
sex <- c("F","M","F","M","F")
data_ex <- data.frame(id=id,sex=sex)
write.table(data_ex, file="data/data_ex.txt")

exdata1 <- read_excel("data/Sample1.xlsx")
View(exdata1)
str(exdata1)

# 변수명 변경하기
exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)
View(exdata1)
ls(exdata1) # 변수명이 나온다.

# 파생변수 생성하기
exdata1$AMT <- exdata1$Y17_AMT + exdata1$Y16_AMT
exdata1$CNT <- exdata1$Y17_CNT + exdata1$Y16_CNT
View(exdata1)
exdata1$AVG_AMT <- exdata1$AMT / exdata1$CNT
View(exdata1)

# exdata1 데이터 세트에서 AGE 변수 값이 50이상이면 Y 50미만이면 N값으로
exdata1$AGE50_YN <- ifelse(exdata1$AGE >= 50, "Y","N")
View(exdata1)
exdata1$AGE_GR10 <- ifelse(exdata1$AGE >= 50, "A1.50++",
                           ifelse(exdata1$AGE >= 40, "A2.4049",
                                  ifelse(exdata1$AGE >= 30, "A3.3039",
                                         ifelse(exdata1$AGE >= 20, "A4.2029", "A5.0019"))))
View(exdata1)

exdata1 %>% select(ID)
exdata1 %>% filter(AREA == "서울")
exdata1 %>% filter(AREA =="서울" & Y17_CNT >= 10)

exdata1 %>% select(-AREA)
exdata1 %>% select(-AREA,-Y17_CNT)

write.csv(exdata1, file="exdata1.csv")

install.packages('writexl')
library(writexl)
write_xlsx(exdata1,path="exdata1.xlsx")

# 변수 값을 그룹별로 합산하기
exdata1 %>% group_by(AREA) %>% summarise(SUM_Y17_AMT = sum(Y17_AMT))

# 데이터 결합하기
m_history <- read_excel("data/Sample2_m_history.xlsx")
View(m_history)
f_history <- read_excel("data/Sample3_f_history.xlsx")
View(f_history)
# m_history 와 f_history 합치기
exdata_bindjoin <- bind_rows(m_history,f_history)
View(exdata_bindjoin)

# 다양한 join
jeju_y17_history <- read_excel("data/Sample4_y17_history.xlsx")
jeju_y16_history <- read_excel("data/Sample5_y16_history.xlsx")
View(jeju_y17_history)
View(jeju_y16_history)
# left_join()
bind_col <- left_join(jeju_y17_history,jeju_y16_history, by ="ID")
View(bind_col)
# inner_join
bind_col_inner <- inner_join(jeju_y17_history,jeju_y16_history, by ="ID")
View(bind_col_inner)
# full_join
bind_col_full <- full_join(jeju_y17_history,jeju_y16_history, by ="ID")
View(bind_col_full)

# 빈도 분석
exdata1 <- read_excel("data/Sample1.xlsx")
exdata1

# install.packages("descr")
library(descr)

# exdata1의 AREA 변수의 빈도분석(그래프 제외 : plot = F) 결과를 freq_test에 할당
freq_test <- freq(exdata1$AREA,plot = F)
freq_test
# 줄기 잎 그림(Stem-and-LeafPlot): 변수의 값을 자릿수에 따라 분류하는 시각화 방법
stem(exdata1$AGE)
