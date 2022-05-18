
# 결측치 만들기 - 표기 NA

df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df) # 결측치 확인
table(is.na(df)) # 결측치 빈도 출력

# 변수별로 결측치 확인하기
table(is.na(df$sex)) # sex 결측치 빈도 출력
table(is.na(df$score)) # score 결측치 빈도 출력

# 결측치 포함된 상태로 분석-정상적으로 연산되지 않고 NA가 출력됨
mean(df$score) # 평균산출
sum(df$score) # 합계 산출

# 결측치 제거하기 - 결측치 있는 행 제거하기
library(dplyr)
df %>% filter(is.na(score)) # score가 NA인 데이터만 출력
df %>% filter(!is.na(score)) # score 결측치 제거

# 결측치 제외한 데이터로 분석하기
df_nomiss <- df %>% filter(!is.na(score)) # score 결측치 제거
mean(df_nomiss$score) # score 평균치 산출
sum(df_nomiss$score) # score 합계 산출

# 여러 변수 동시에 결측치 없는 데이터 추출하기
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))# score,sex 결측치 제외
df_nomiss

# 결측치가 하나라도 있으면 제거하기
df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2
# - 분석에 필요한 데이터까지 손실 될 가능성 유의
# - filter()을 이용해 분석에 사용할 변수의 결측치만 제거하는 방식을 권한다.

# 함수의 결측치 제외 기능 이용하기 - na.rm = T
mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T) # 결측치 제외하고 합계 산출

# summarise()에서 na.rm = T사용하기
exam <- read.csv("data/csv_exam.csv")
exam[c(3,8,15),"math"] <- NA # 3,8,15 행의 math에 NA 할당
exam
exam %>% summarise(mean_math = mean(math)) # 결측치 때문에 계산이 안됨
exam %>% summarise(mean_math = mean(math, na.rm = T)) # 결측치 제외하고 평균

# 다른 함수들에 적용
exam %>% summarise(mean_math = mean(math, na.rm = T), # 평균 산출
                   sum_math = sum(math, na.rm = T), # 합계 산출
                   median_math = median(math, na.rm = T)) # 중앙값 산출

# 평균값으로 결측치 대체하기
mean(exam$math, na.rm = T) # 결측치 제외하고 math 평균 산출
exam$math <- ifelse(is.na(exam$math), 55, exam$math) # math 가 NA 면 55 로 대체
table(is.na(exam$math))

# 이상치(Outlier) - 정상범주에서 크게 벗어난 값
# 이상치 포함된 데이터 생성 - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
# 이상치 확인하기
table(outlier$sex)
table(outlier$score)
# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
# score 가 1~5아니면 NA할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
# 결측치 제외하고 분석
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# 상자그림으로 극단치 기준 정해서 제거하기
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy) # 상자그림 출력
boxplot(mpg$hwy)$stats # 상자그림 통계치 출력

# 12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

# 산점도 만들기
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
# 배경 설정하기 x 축 displ, y 축 hwy 로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))
# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# 축 범위를 조정하는 설정 추가하기. x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue") +
  xlim(3, 6) +
  ylim(10, 30)
View(mpg)

# 집단별 평균표 만들기
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
df_mpg
# 그래프 그리기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()
# 크기순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col(color = "red", fill="blue")

# 막대그래프2 - 빈도 막대 그래프
# x축 범주 변수, y축 빈도
ggplot(data = mpg, aes(x = drv)) + geom_bar()
# x축 연속변수, y축 빈도
ggplot(data = mpg, aes(x=hwy)) + geom_bar(color = "red", fill = "green")

# 선 그래프 - 시계열 그래프 만들기
economics <- as.data.frame(ggplot2::economics)
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

# 상자 그림 만들기
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

# 히스토그램
library(readxl)
exdata1 <- read_excel("data/Sample1.xlsx")
exdata1
hist(exdata1$AGE)

# 빈도 분석 및 막대그래프
library(descr)
# exdata1의 SEX변수 분포 막대그래프 그리기, 그래프 명칭은'성별(barplot)'로 함
freq(exdata1$SEX, plot = T, main = '성별(barplot)')

dist_sex <- table(exdata1$SEX)
# dist_sex에 대한 막대그래프 그리기
barplot(dist_sex)
# y축을 0~8로 변경
barplot(dist_sex, ylim = c(0,8))
# y축 제목을 'FREQUENCY', 항목 값을 'Female','Male'로 변경
barplot(dist_sex, ylim = c(0,8), main = "BARPLOT", xlab = "SEX", ylab ="FREQUENCY", names = c("Female","Male"))
# 그래프 색 변경
barplot(dist_sex, ylim = c(0,8), 
        main = "BARPLOT", xlab = "SEX", ylab ="FREQUENCY",
        names = c("Female","Male"), 
        col = c("pink","navy"))

# 상자그림(Boxpiot)
boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT,
        ylim = c(0,60),
        main = "boxplot",
        names = c("17년건수","16년건수"),
        col = c("green","yellow"))

y1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 25)
boxplot(y1)

# ggplot2 패키지로 그릴수 있는 다양한 그래프
str(airquality)

ggplot(airquality, aes(x=Day, y=Temp)) +
  geom_line(color ="red", size=1) +
  geom_point(size=3)

# 꺽은선 그래프에 평행선 그리기
ggplot(economics, aes(x=date, y=psavert)) +
  geom_line() +
  geom_hline(yintercept = mean(economics$psavert))

# 개인 저축률(psavert)이 최솟값일 때의 날짜(date)를 구해 x_inter 변수에 할당
x_inter <- filter(economics, psavert == min(economics$psavert))$date
ggplot(economics, aes(x=date, y=psavert)) +
  geom_line() +
  geom_vline(xintercept = x_inter)

# 도형 및 화살표,annotate 함수
ggplot(mtcars, aes(x =wt,y=mpg)) +
  geom_point() +
  annotate("rect", xmin =3, xmax = 4, ymin = 12, ymax = 21, alpha = 0.5, fill = "skyblue")

# 산점도에 사각형, 화살표, 레이블 추가하기
ggplot(mtcars, aes(x= wt, y = mpg)) +
  geom_point() +
  annotate("rect", xmin = 3, xmax = 4, ymin = 12, ymax = 21, alpha = 1, fill="skyblue") +
  annotate("segment", x = 2.5, xend = 3.7, y = 10, yend = 17, color="red", arrow=arrow())+ # 화살표
  annotate("text", x=2.5, y=10, label="point")
