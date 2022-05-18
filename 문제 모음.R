##### 2회차 #####
# 1. 2013년_프로야구선수_성적.csv을 data1 변수에 넣으세요.
setwd('c:/Work._R/Data')
getwd()
data1 <- read.csv("2013년_프로야구선수_성적.csv")
data1
# 2. data1에서 경기수가 120 경기 이상인 선수들만 data_a 변수에 넣으세요.
data_a <- data1 %>% filter(경기 >= 120) 
data_a
# 3. data1에서 경기수가 120 경기 이상이고 득점도 80점 이상인 선수들만 data_b 변수에 저장하세요.
data_b <- data1 %>% filter(경기 >=120 & 득점 >= 80)
data_b
# 4. 포지션이 1루수와 3루수인 선수만 data_c에 넣어 출력하세요.
data_c <- data1 %>% filter(포지션=="1루수" | 포지션=="3루수" )
data_c                             
# 5. 선수명, 포지션, 팀 컬럼 데이터만 조회하기
data1 %>% 
  select(선수명, 포지션, 팀)
# 6. 순위 ~ 타수 컬럼까지 출력하기
data1 %>% 
  select(순위, 선수명, 포지션, 팀, 경기, 타수)
# 7. 특정 컬럼만 제외하고 출력하기(홈런, 타점, 도루)
data1 %>% 
  select(-홈런,-타점,-도루)
# 8. 타수가 400초과한 선수명, 팀, 경기, 타수 칼럼을 출력하세요.
data1 %>% 
  filter(타수>400) %>% 
  select(선수명,팀,경기,타수)
# 9.위 8번을 타수 순으로 정렬하세요.
data1 %>% 
  filter(타수>400) %>% 
  select(선수명,팀,경기,타수) %>% 
  arrange(타수)

##### 3회차######
# 1. 회사별로 "suv" 자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기

mpg %>%
  group_by(manufacturer) %>% # 회사별로 분리
  filter(class == "suv") %>% # suv 추출
  mutate(tot = (cty+hwy)/2) %>% # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>% # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>% # 내림차순 정렬
  head(5) # 1~5 위까지 출력

# 2.mpg 데이터를 이용해서 분석 문제를 해결해 보세요.
# mpg 데이터의 fl 변수는 자동차에 사용하는 연료(fuel)를 의미합니다. 아래는 자동차 연료별 가격을 나타낸 표입니다.
# fl 연료 종류 가격(갤런당 USD)
# c CNG 2.35
# d diesel 2.38
# e ethanol E85 2.11
# p premium 2.76
# r regular 2.22
#우선 이 정보를 이용해서 연료와 가격으로 구성된 데이터 프레임을 만들어 보세요.
mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
mpg <- left_join(mpg, fuel, by = "fl")
head(mpg,20)
# 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.
mpg %>% 
  select(model,fl,price_fl) %>% 
  head(5)

# excel_exam.xlsx에서 20명의 학생의 데이터를 불러와서
exam_excl <- read_excel("data/excel_exam.xlsx")
head(exam_excl,20)

# 1. math, english, science 3과목의 합계를 출력하세요.(s_total)
s_total <- exam_excl %>% mutate(total = math + english + science)
head(s_total,20)

# 2. math, english, science 3과목의 평균을 출력하세요.(s_mean)
s_mean <- exam_excl %>% mutate(total = math + english + science, 
                               mean = (math + english + science)/3)
head(s_mean,20)

# 3. 평균이 60점 이상이면 "pass" 그렇지 않으면 "fail"를 부여하는파생변수test1를 생성하세요.
test1 <- exam_excl %>% mutate(total = math + english + science,
                              mean = (math + english + science)/3) %>% 
  mutate (test=ifelse(mean >= 60, "pass", "fail"))
head(test1)

# 4. 3의 결과의 빈도수와 그래프로 나타내세요
table(test1$test)
qplot(test1$test)

# 5. 평균이 80점 이상이면 "A", 70이상이면 "B", 그외는 "C"를 부여하는 파생변수 test2를 생성하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(test1)

# 6. 5의 결과를 상위 10개만 출력하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(head(test1,10))

# 7. 5의 결과를 하위 7개만 출력하세요.
test1$test2 <- ifelse(test1$mean >= 80, "A",
                      ifelse(test1$mean >= 70, "B","c"))
View(tail(test1,7))

# 8. 5의 결과를 빈도수와 그래프로 나타내세요.
table(test1$test2)
qplot(test1$test2)

##### 4회차 #####
mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA # NA 할당하기
View(mpg)

# Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.
table(is.na(mpg))
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다
mpg %>% filter(!is.na(hwy)) %>% # 결측치 제외
  group_by(drv) %>% # drv 별 분리
  summarise(mean_hwy = mean(hwy)) # hwy 평균 구하기

# Q1. mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. x 축은 cty, y 축은 hwy 로 된 산점도를 만들어 보세요.
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# Q2. 미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를 이용해서 전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를 만들어 보세요. 전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 산점도에 표시되게 설정하세요.
midwest <- as.data.frame(ggplot2::midwest)
View(midwest)
ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)

options(scipen = 99)
options(scipen = 0)

# Q1. 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv" 차종을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비 가 높은 순으로 정렬하세요.
mpg <- as.data.frame(ggplot2::mpg)

mpg_suv <- mpg %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
mpg_suv

ggplot(data = mpg_suv, aes(x = reorder(manufacturer, -mean_cty),
                           y = mean_cty)) + geom_col()

# Q2. 자동차 중에서 어떤 class(자동차 종류)가 가장 많은지 알아보려고 합니다. 자동차 종류별 빈도를 표현한 막대 그래프를 만들어 보세요.
ggplot(data = mpg, aes(x = class)) + geom_bar()

# Q1. psavert(개인 저축률)가 시간에 따라서 어떻게 변해왔는지 알아보려고 합니다. 시간에 따른 개인 저축률의 변화를 나타낸 시계열 그래프를 만들어 보세요.
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

# Q1. class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시 연비)가 어떻게 다른지 비교해보려고 합니다. 세 차종의 cty를 나타낸 상자 그림을 만들어보세요.
mpg <- as.data.frame(ggplot2::mpg)
new <- mpg %>% 
  filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data = new, aes(x = class, y = cty)) + geom_boxplot()

#1.학생별국어성적_new.txt 데이터를 불러오세요.
library(readxl)
data <- read.table("data/학생별국어성적_new.txt", fileEncoding = "euc-kr",header = TRUE, sep=",")
View(data)

#2 x축은 이름, y축은 점수로 산점도에 표시되게 그래프를 그려 보세요.
ggplot(data = data, aes(x=이름, y=점수)) + geom_point(color="red", size=5)+geom_point(color="white", size=2)

#3 2을 막대그래프로 그려 보세요.
ggplot(data = data, aes(x = 이름, y = 점수)) + geom_col(color= 'black', fill='skyblue')

#4 학생별과목별성적_3기_3명.csv를 아래와 같은 그래프로 출력하세요.
data3 <- read.csv('data/학생별과목별성적_3기_3명.csv',fileEncoding = "euc-kr", header = T, sep = ',')
View(data3)
ggplot(data = data3, aes(x = 과목, y = 점수,group = 이름, color = 이름)) + geom_line() +geom_point(size=5,  pch = 15)
